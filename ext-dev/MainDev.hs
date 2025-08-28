{-# LANGUAGE OverloadedStrings #-}

module MainDev (main) where

import qualified BackgroundWriter
import qualified Build
import qualified CommandParser
import qualified Control.Monad as Monad
import qualified Data.ByteString
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder
import qualified Data.ByteString.Char8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSChar
import qualified Data.Char as Char
import qualified Data.List
import qualified Data.Map
import qualified Data.Maybe
import qualified Data.Name as Name
import qualified Data.NonEmptyList as NE
import qualified Data.Utf8 as Utf8
import qualified Elm.Details
import qualified Elm.Docs as Docs
import qualified Elm.ModuleName
import qualified Elm.Outline
import qualified Elm.Package as Pkg
import qualified Elm.Version
import qualified Ext.Common
import qualified Ext.CompileMode
import qualified Ext.CompileProxy
import qualified Ext.Dev
import qualified Ext.Dev.CallGraph
import qualified Ext.Dev.EntryPoints
import qualified Ext.Dev.Explain
import qualified Ext.Dev.Find
import qualified Ext.Dev.Imports
import qualified Ext.Dev.Json.Encode
import qualified Ext.Dev.Lookup
import qualified Ext.Dev.Package
import qualified Ext.Dev.Project
import qualified Ext.Dev.Usage
import qualified Ext.Log
import qualified File
import qualified Gen.Commands
import qualified Gen.Commands.Make
import qualified Json.Decode
import Json.Encode ((==>))
import qualified Json.Encode
import qualified Json.String
import qualified Make
import qualified Reporting
import qualified Reporting.Exit as Exit
import qualified Reporting.Render.Type.Localizer
import qualified Stuff
import qualified System.Directory as Dir
import qualified System.Exit as Exit
import qualified System.FilePath as Path
import qualified System.IO as IO
import qualified System.Process as Process
import qualified Terminal.Colors
import qualified Terminal.Dev.Args
import qualified Terminal.Dev.Error
import qualified Terminal.Dev.Out
import qualified Terminal.Helpers
import qualified Text.PrettyPrint.ANSI.Leijen as P
import qualified Text.Read
import qualified Watchtower.Live
import qualified Watchtower.Server
import qualified Watchtower.Server.LSP
import qualified Watchtower.Server.MCP
import qualified Watchtower.Server.Run
import qualified Watchtower.Server.Daemon as Daemon
import qualified Ext.Test.Runner as TestRunner
import qualified Ext.Test.Install as TestInstall
import qualified Ext.Test.Templates.Loader as TestTemplates
import qualified Network.Socket as Net
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar, threadDelay)
import qualified Control.Exception as Exception
import qualified Text.Read as Read

parseModuleList :: String -> Maybe (NE.List Elm.ModuleName.Raw)
parseModuleList str = case Data.Maybe.catMaybes $ fmap parseElmModule (splitOn ',' str) of
  [] -> Nothing
  (top : remain) -> Just (NE.List top remain)

parseElmModule :: String -> Maybe Elm.ModuleName.Raw
parseElmModule charsRaw =
  let chars = trimWhitespace charsRaw
   in if length chars == 0
        then Nothing
        else
          let pieces = splitOn '.' chars
           in if all isValidElmPiece pieces
                then Just (Name.fromChars chars)
                else Nothing

isValidElmPiece :: String -> Bool
isValidElmPiece [] = False
isValidElmPiece (x : xs) = Char.isUpper x && all isValidChar xs
  where
    isValidChar c = Char.isAlphaNum c || c == '_'

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn delimiter = go
  where
    go [] = []
    go xs =
      let (before, remainder) = break (== delimiter) xs
       in before : case remainder of
            [] -> []
            _ : after -> go after

trimWhitespace :: String -> String
trimWhitespace = reverse . dropWhile Char.isSpace . reverse . dropWhile Char.isSpace

inspectGroup :: Maybe String
inspectGroup = Just "Inspection"

devGroup :: Maybe String
devGroup = Just "Development"

-- Command handlers
serverCommand :: CommandParser.Command
serverCommand = CommandParser.command ["server"] "Start the Elm Dev server" devGroup CommandParser.noArg parseServerFlags runServer
  where
    portFlag = CommandParser.flagWithArg "port" "Port to run the server on" Text.Read.readMaybe
    parseServerFlags = CommandParser.parseFlag portFlag
    runServer _ maybePort = do
      -- Ext.CompileMode.setModeMemory
      -- Ext.CompileMode.setModeDisk
      Ext.Log.withAllBut [Ext.Log.Performance] $ Watchtower.Server.serve Nothing (Watchtower.Server.Flags maybePort)

mcpCommand :: CommandParser.Command
mcpCommand = CommandParser.command ["mcp"] "Start the Elm Dev MCP server" devGroup CommandParser.noArg parseServerFlags runServer
  where
    parseServerFlags = CommandParser.noFlag
    runServer _ _ = do
      -- Ext.CompileMode.setModeMemory
      _state <- Daemon.ensureRunning
      proxyStdioToTcp Ext.Log.TempMCP "127.0.0.1" (Daemon.mcpPort _state)

lspCommand :: CommandParser.Command
lspCommand = CommandParser.command ["lsp"] "Start the Elm Dev LSP server" devGroup CommandParser.noArg parseServerFlags runServer
  where
    parseServerFlags = CommandParser.noFlag
    runServer _ _ = do
      Ext.CompileMode.setModeMemory
      _state <- Daemon.ensureRunning
      proxyStdioToTcp Ext.Log.TempLSP "127.0.0.1" (Daemon.lspPort _state)

-- | Transparent stdio <-> TCP proxy; no protocol logic, binary-safe
proxyStdioToTcp :: Ext.Log.TempChannel -> String -> Int -> IO ()
proxyStdioToTcp chan host port = do
  let attempts = 50
  let backoffUs = 100000 -- 100ms
  let connectWithRetry n = do
        addrs <- Net.getAddrInfo Nothing (Just host) (Just (show port))
        let serverAddr = head addrs
        sock <- Net.socket (Net.addrFamily serverAddr) Net.Stream Net.defaultProtocol
        result <- Exception.try (Net.connect sock (Net.addrAddress serverAddr)) :: IO (Either IOError ())
        case result of
          Right _ -> pure sock
          Left _ -> do
            Net.close sock
            if n <= 0 then ioError (userError "daemon LSP not accepting connections") else do
              threadDelay backoffUs
              connectWithRetry (n - 1)
  sock <- connectWithRetry attempts
  h <- Net.socketToHandle sock IO.ReadWriteMode
  IO.hSetBinaryMode h True
  IO.hSetBuffering h IO.NoBuffering
  IO.hSetBinaryMode IO.stdin True
  IO.hSetBinaryMode IO.stdout True
  done <- newEmptyMVar
  _ <- forkIO $ do
    let loop = do
          chunk <- BS.hGetSome IO.stdin 4096
          if BS.null chunk then putMVar done () else do
            Ext.Log.logTempBytes chan chunk
            BS.hPut h chunk
            IO.hFlush h
            loop
    loop
  _ <- forkIO $ do
    let loop = do
          chunk <- BS.hGetSome h 4096
          if BS.null chunk then putMVar done () else do
            Ext.Log.logTempBytes chan chunk
            BS.hPut IO.stdout chunk
            IO.hFlush IO.stdout
            loop
    loop
  _ <- takeMVar done
  IO.hClose h

-- Daemon commands
daemonServeCommand :: CommandParser.Command
daemonServeCommand = CommandParser.command ["daemon","serve"] "Run the Elm Dev daemon (internal)" devGroup parseArgs parseFlags runCmd
  where
    parseArgs = CommandParser.noArg
    parseFlags = CommandParser.noFlag
    runCmd _ _ = do
      Daemon.serve

daemonStartCommand :: CommandParser.Command
daemonStartCommand = CommandParser.command ["daemon","start"] "Start daemon if not running" devGroup parseArgs parseFlags runCmd
  where
    parseArgs = CommandParser.noArg
    parseFlags = CommandParser.noFlag
    runCmd _ _ = do
      _ <- Daemon.start
      pure ()

daemonStopCommand :: CommandParser.Command
daemonStopCommand = CommandParser.command ["daemon","stop"] "Stop daemon" devGroup parseArgs parseFlags runCmd
  where
    parseArgs = CommandParser.noArg
    parseFlags = CommandParser.noFlag
    runCmd _ _ = do
      Daemon.stop

daemonStatusCommand :: CommandParser.Command
daemonStatusCommand = CommandParser.command ["daemon","status"] "Show daemon status" devGroup parseArgs parseFlags runCmd
  where
    parseArgs = CommandParser.noArg
    parseFlags = CommandParser.noFlag
    runCmd _ _ = do
      st <- Daemon.status
      case st of
        Nothing -> IO.hPutStrLn IO.stderr "daemon: not running"
        Just s -> LBSChar.putStrLn (Data.ByteString.Builder.toLazyByteString (Json.Encode.encodeUgly (Json.Encode.object [
                    ("pid" ==> Json.Encode.int (Daemon.pid s)),
                    ("lspPort" ==> Json.Encode.int (Daemon.lspPort s)),
                    ("mcpPort" ==> Json.Encode.int (Daemon.mcpPort s)),
                    ("httpPort" ==> Json.Encode.int (Daemon.httpPort s)),
                    ("version" ==> Json.Encode.string (Json.String.fromChars (Daemon.version s)))])))

docsCommand :: CommandParser.Command
docsCommand = CommandParser.command ["inspect", "docs"] "Report the docs.json" inspectGroup parseDocsArgs parseDocsFlags runDocs
  where
    outputFlag = CommandParser.flagWithArg "output" "Output file path" Just
    parseDocsArgs = CommandParser.parseArgList pathOrModuleName
    parseDocsFlags = CommandParser.parseFlag outputFlag
    runDocs (path, _) maybeOutput = do
      let actualPath = case path of
            "" -> "."
            p -> p
      maybeRoot <- Stuff.findRoot
      case maybeRoot of
        Nothing ->
          Terminal.Dev.Out.json maybeOutput (Left Terminal.Dev.Error.CouldNotFindRoot)
        Just root -> do
          maybeDocs <- Ext.Dev.docs root actualPath
          case maybeDocs of
            Nothing ->
              Terminal.Dev.Out.json maybeOutput (Left Terminal.Dev.Error.CouldNotFindModule)
            Just docs ->
              Terminal.Dev.Out.json maybeOutput (Right (Docs.encode (Data.Map.singleton (Docs._name docs) docs)))

pathOrModuleName :: CommandParser.Arg String
pathOrModuleName = CommandParser.arg "My.Module"

warningsCommand :: CommandParser.Command
warningsCommand = CommandParser.command ["inspect", "warnings"] "Report warnings" inspectGroup parseWarningsArgs parseWarningsFlags runWarnings
  where
    outputFlag = CommandParser.flagWithArg "output" "Output file path" Just
    parseWarningsArgs = CommandParser.parseArgList pathOrModuleName
    parseWarningsFlags = CommandParser.parseFlag outputFlag
    runWarnings (path, _) maybeOutput = do
      let actualPath = case path of
            "" -> "."
            p -> p
      compilationCheckResult <- loadAndEnsureCompiled (Just (NE.singleton (Name.fromChars actualPath)))
      case compilationCheckResult of
        Left err ->
          Terminal.Dev.Out.json maybeOutput (Left err)
        Right details -> do
          moduleResult <- Terminal.Dev.Args.modul actualPath
          case moduleResult of
            Left err ->
              Terminal.Dev.Out.json maybeOutput (Left err)
            Right (Terminal.Dev.Args.Module root (Terminal.Dev.Args.ModuleInfo moduleName modulePath) details) -> do
              eitherWarnings <- Ext.Dev.warnings root modulePath
              case eitherWarnings of
                Left err ->
                  Terminal.Dev.Out.json
                    maybeOutput
                    ( Right
                        ( Json.Encode.list
                            id
                            [ Json.Encode.object
                                [ "filepath" ==> Json.Encode.string (Json.String.fromChars modulePath),
                                  "module" ==> Json.Encode.name moduleName,
                                  "warnings" ==> Json.Encode.list id []
                                ]
                            ]
                        )
                    )
                Right (mod, warningList) ->
                  Terminal.Dev.Out.json
                    maybeOutput
                    ( Right
                        ( Json.Encode.list
                            id
                            [ Json.Encode.object
                                [ "filepath" ==> Json.Encode.string (Json.String.fromChars modulePath),
                                  "module" ==> Json.Encode.name moduleName,
                                  "warnings"
                                    ==> Json.Encode.list
                                      (Watchtower.Live.encodeWarning (Reporting.Render.Type.Localizer.fromModule mod))
                                      warningList
                                ]
                            ]
                        )
                    )

importsCommand :: CommandParser.Command
importsCommand = CommandParser.command ["inspect", "imports"] "Report all imports" inspectGroup parseImportsArgs parseImportsFlags runImports
  where
    outputFlag = CommandParser.flagWithArg "output" "Output file path" Just
    entrypointsFlag = CommandParser.flagWithArg "entrypoints" "Comma-separated list of entrypoint modules" parseModuleList
    parseImportsArgs = CommandParser.parseArgList pathOrModuleName
    parseImportsFlags = CommandParser.parseFlag2 outputFlag entrypointsFlag
    runImports (path, _) (maybeOutput, maybeEntrypoints) = do
      if null path
        then Terminal.Dev.Out.json maybeOutput (Left Terminal.Dev.Error.CouldNotFindModule)
        else do
          result <- loadAndEnsureCompiled maybeEntrypoints
          case result of
            Left err ->
              Terminal.Dev.Out.json maybeOutput (Left err)
            Right details -> do
              let importSummary = Ext.Dev.Imports.getImportSummaryForMany details [Name.fromChars path]
              Terminal.Dev.Out.json
                maybeOutput
                ( Right
                    ( Ext.Dev.Imports.encodeSummary
                        importSummary
                    )
                )

usageCommand :: CommandParser.Command
usageCommand = CommandParser.command ["inspect", "usage"] "Module usage" inspectGroup parseUsageArgs parseUsageFlags runUsage
  where
    outputFlag = CommandParser.flagWithArg "output" "Output file path" Just
    entrypointsFlag = CommandParser.flagWithArg "entrypoints" "Comma-separated list of entrypoint modules" parseModuleList
    parseUsageArgs = CommandParser.parseArgList pathOrModuleName
    parseUsageFlags = CommandParser.parseFlag2 outputFlag entrypointsFlag
    runUsage (path, _) (maybeOutput, maybeEntrypoints) = do
      let actualPath = case path of
            "" -> "."
            p -> p
      result <- loadAndEnsureCompiled maybeEntrypoints
      case result of
        Left err ->
          Terminal.Dev.Out.json maybeOutput (Left err)
        Right details -> do
          usageSummary <- Ext.Dev.Usage.usageOfModule "." details (Name.fromChars actualPath)
          case usageSummary of
            Nothing ->
              Terminal.Dev.Out.json
                maybeOutput
                (Left Terminal.Dev.Error.CouldNotFindModule)
            Just summary ->
              Terminal.Dev.Out.json
                maybeOutput
                ( Right
                    ( Ext.Dev.Usage.encode
                        summary
                    )
                )

entrypointsCommand :: CommandParser.Command
entrypointsCommand = CommandParser.command ["inspect", "entrypoints"] "Report entrypoints" inspectGroup CommandParser.noArg parseEntrypointsFlags runEntrypoints
  where
    outputFlag = CommandParser.flagWithArg "output" "Output file path" Just
    parseEntrypointsFlags = CommandParser.parseFlag outputFlag
    runEntrypoints _ maybeOutput = do
      maybeRoot <- Stuff.findRoot
      case maybeRoot of
        Nothing ->
          Terminal.Dev.Out.json
            maybeOutput
            (Left Terminal.Dev.Error.CouldNotFindRoot)
        Just root -> do
          entryResult <- Ext.Dev.entrypoints root
          case entryResult of
            Left err ->
              Terminal.Dev.Out.json
                maybeOutput
                (Left (Terminal.Dev.Error.CompilationError err))
            Right entry ->
              Terminal.Dev.Out.json
                maybeOutput
                (Right (Json.Encode.list Ext.Dev.EntryPoints.encode entry))

elmModuleName = CommandParser.arg "Module.value"

explainCommand :: CommandParser.Command
explainCommand = CommandParser.command ["inspect", "definition"] "Explain a definition" inspectGroup parseExplainArgs parseExplainFlags runExplain
  where
    outputFlag = CommandParser.flagWithArg "output" "Output file path" Just
    parseExplainArgs = CommandParser.parseArgList elmModuleName
    parseExplainFlags = CommandParser.parseFlag outputFlag
    runExplain (path, _) maybeOutput = do
      let actualPath = case path of
            "" -> "."
            p -> p
      valueResult <- Terminal.Dev.Args.value actualPath
      case valueResult of
        Left err ->
          Terminal.Dev.Out.json maybeOutput (Left err)
        Right (Terminal.Dev.Args.Value root modName valueName) -> do
          compilationCheckResult <- Ext.CompileProxy.loadAndEnsureCompiled root (Just (NE.List modName []))
          case mapError Terminal.Dev.Error.CompilationError compilationCheckResult of
            Left err ->
              Terminal.Dev.Out.json
                maybeOutput
                (Left err)
            Right details -> do
              maybeFound <- Ext.Dev.Explain.explain details root modName valueName
              case maybeFound of
                Nothing ->
                  Terminal.Dev.Out.json maybeOutput (Left Terminal.Dev.Error.CouldNotFindModule)
                Just definition ->
                  Terminal.Dev.Out.json maybeOutput (Right (Ext.Dev.Explain.encode definition))

-- Helper functions
loadAndEnsureCompiled :: Maybe (NE.List Elm.ModuleName.Raw) -> IO (Either Terminal.Dev.Error.Error Elm.Details.Details)
loadAndEnsureCompiled maybeEntrypoints = do
  maybeRoot <- Stuff.findRoot
  case maybeRoot of
    Nothing ->
      pure (Left Terminal.Dev.Error.CouldNotFindRoot)
    Just root -> do
      compilationResult <- Ext.CompileProxy.loadAndEnsureCompiled root maybeEntrypoints
      pure (mapError Terminal.Dev.Error.CompilationError compilationResult)

mapError :: (a -> c) -> Either a b -> Either c b
mapError f (Left x) = Left (f x)
mapError _ (Right x) = Right x

formatCommandWithEllipsis :: String -> String -> String
formatCommandWithEllipsis cmd desc =
  let maxLength = 80
      cmdLength = length cmd
      descLength = length desc
      minSpacing = 2 -- Minimum number of spaces between cmd and desc
      availableSpace = maxLength - cmdLength - descLength
      spacing = max minSpacing (availableSpace - 2)
      dots = replicate spacing '.'
   in cmd ++ " " ++ dots ++ " " ++ desc

-- Main function
main :: IO ()
main = do
  CommandParser.run
    ( \commands givenCommand ->
        -- Show Help
        let joinArgs [] = ""
            joinArgs argList = " " ++ Data.List.intercalate " " argList

            -- Filter commands that match the givenCommand prefix
            filteredCommands =
              if null givenCommand
                then commands
                else filter (\cmd -> Data.List.isPrefixOf givenCommand (CommandParser.cmdName cmd)) commands
         in unlines
              [ "",
                "Welcome to Elm Dev",
                ""
              ]
              ++
              -- Group commands by their group (if any)
              let groupedCommands = Data.List.groupBy (\a b -> CommandParser.cmdGroup a == CommandParser.cmdGroup b) filteredCommands
                  formatCommand (CommandParser.CommandMetadata name argList group desc) =
                    formatCommandWithEllipsis
                      ("  elm-dev " ++ Terminal.Colors.green (Data.List.intercalate " " name) ++ Terminal.Colors.grey (joinArgs argList))
                      desc
                  formatGroup cmds = case CommandParser.cmdGroup (head cmds) of
                    Just group -> ["", group ++ ":", ""] ++ map formatCommand cmds
                    Nothing -> map formatCommand cmds
               in concatMap (unlines . formatGroup) groupedCommands
    )
    [ Gen.Commands.initialize,
      Gen.Commands.Make.command,
      Gen.Commands.addPage,
      Gen.Commands.addStore,
      Gen.Commands.addEffect,
      Gen.Commands.addListener,
      Gen.Commands.addDocs,
      Gen.Commands.addTheme,
      Gen.Commands.customize,
      serverCommand,
      mcpCommand,
      daemonServeCommand,
      daemonStartCommand,
      daemonStopCommand,
      daemonStatusCommand,
      lspCommand,
      entrypointsCommand,
      docsCommand,
      warningsCommand,
      importsCommand,
      usageCommand,
      explainCommand,
      testInitCommand,
      testCommand
    ]

-- Test command
testCommand :: CommandParser.Command
testCommand = CommandParser.command ["test"] "Discover, compile, and run Elm tests" devGroup CommandParser.noArg parseFlags runCmd
  where
    parseFlags = CommandParser.noFlag
    runCmd _ _ = do
      Ext.CompileMode.setModeMemory
      maybeRoot <- Stuff.findRoot
      case maybeRoot of
        Nothing -> IO.hPutStrLn IO.stderr "Could not find project root"
        Just root -> do
          result <- TestRunner.run root
          case result of
            Left err -> IO.hPutStrLn IO.stderr err
            Right json -> LBSChar.putStrLn (Data.ByteString.Builder.toLazyByteString (Json.Encode.encodeUgly (Json.Encode.string (Json.String.fromChars json))))


-- elm-dev test init
testInitCommand :: CommandParser.Command
testInitCommand = CommandParser.command ["test","init"] "Initialize tests: install elm-explorations/test and add Example.elm" devGroup CommandParser.noArg parseFlags runCmd
  where
    parseFlags = CommandParser.noFlag
    runCmd _ _ = do
      Ext.CompileMode.setModeMemory
      maybeRoot <- Stuff.findRoot
      case maybeRoot of
        Nothing -> IO.hPutStrLn IO.stderr "Could not find project root"
        Just root -> do
          -- Install elm-explorations/test into test-dependencies
          result <- TestInstall.installTestDependency (Pkg.toName (Utf8.fromChars "elm-explorations") "test")
          case result of
            Left _ -> IO.hPutStrLn IO.stderr "Failed to install elm-explorations/test"
            Right _ -> pure ()
          -- Write tests/Example.elm if it does not exist
          let testsDir = root Path.</> "tests"
          Dir.createDirectoryIfMissing True testsDir
          let examplePath = testsDir Path.</> "Example.elm"
          exists <- Dir.doesFileExist examplePath
          if exists
            then pure ()
            else BS.writeFile examplePath TestTemplates.exampleElm
          putStrLn "\nCheck out the documentation for getting started at https://package.elm-lang.org/packages/elm-explorations/test/latest"
