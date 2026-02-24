{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

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
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Data.Utf8 as Utf8
import qualified Data.Word as Word
import qualified Elm.Details
import qualified Elm.Docs as Docs
import qualified Elm.ModuleName
import qualified Elm.Outline
import qualified Elm.Package as Pkg
import qualified Elm.Version
import qualified ElmDevVersion
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
import qualified Install
import qualified Reporting
import qualified Reporting.Exit as Exit
import qualified Reporting.Render.Type.Localizer
import qualified Stuff
import qualified System.Directory as Dir
import qualified System.Exit as Exit
import qualified System.FilePath as Path
import qualified System.IO as IO
import qualified System.Process as Process
import qualified System.Environment
import qualified Terminal.Colors
import qualified Terminal.Dev.Args
import qualified Terminal.Dev.Error
import qualified Terminal.Dev.Out
import qualified Terminal.Helpers
import qualified Text.PrettyPrint.ANSI.Leijen as P
import qualified Text.Read
import qualified Watchtower.Live
import qualified Watchtower.Server
import qualified Watchtower.Server.Run
import qualified Watchtower.Server.Daemon as Daemon
import qualified Watchtower.Server.Proxy
import qualified Ext.Test.Runner
import qualified Ext.Test.Install
import qualified Ext.Test.Result.Report
import qualified Ext.Test.Templates.Loader as TestTemplates
import qualified Network.Socket as Net
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar, threadDelay)
import qualified Control.Exception as Exception
import qualified Text.Read as Read
import qualified System.CPUTime as CPUTime
import qualified Watchtower.Server.MCP.Docs as DocsMarkdown


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

docsGroup :: Maybe String
docsGroup = Just "Documentation"

data DocsInput
  = DocsFromPackage Pkg.Name (Maybe Elm.Version.Version)
  | DocsFromModules [String]
  | DocsFromProjectAndDependencies

data ModulePattern
  = ExactModule Name.Name
  | NamespaceModule String

docsMarkdownCommand :: CommandParser.Command
docsMarkdownCommand = CommandParser.command ["docs"] "Render docs as markdown" docsGroup parseArgs parseFlags runCmd
  where
    parseArgs = CommandParser.parseOptionalArgList (CommandParser.arg "package-or-module")
    outputFlag = CommandParser.flagWithArg "output" "Output directory path" Just
    parseFlags = CommandParser.parseFlag outputFlag
    runCmd args maybeOutputDir =
      case parseDocsInput args of
        Left err ->
          IO.hPutStrLn IO.stderr err

        Right (DocsFromPackage packageName maybeVersion) -> do
          generated <- renderPackageDocs packageName maybeVersion
          case generated of
            Left err ->
              IO.hPutStrLn IO.stderr err

            Right docsByModule ->
              writeOrPrintDocs maybeOutputDir docsByModule

        Right (DocsFromModules patterns) -> do
          generated <- renderProjectDocs patterns
          case generated of
            Left err ->
              IO.hPutStrLn IO.stderr err

            Right docsByModule ->
              writeOrPrintDocs maybeOutputDir docsByModule

        Right DocsFromProjectAndDependencies -> do
          generated <- renderProjectAndDependencyDocs
          case generated of
            Left err ->
              IO.hPutStrLn IO.stderr err

            Right (projectDocs, packageDocs) ->
              case maybeOutputDir of
                Nothing ->
                  printProjectAndDependencyDocs projectDocs packageDocs

                Just outputDir ->
                  writeProjectAndDependencyDocs outputDir projectDocs packageDocs


renderPackageDocs :: Pkg.Name -> Maybe Elm.Version.Version -> IO (Either String [(Name.Name, Text.Text)])
renderPackageDocs packageName maybeVersion = do
  resolvedVersion <- resolvePackageVersion packageName maybeVersion
  case resolvedVersion of
    Nothing ->
      pure
        ( Left
            ( "Could not find a version for package "
                ++ Pkg.toChars packageName
                ++ "."
            )
        )

    Just version -> do
      docsResult <- Ext.Dev.Package.getDocs packageName version
      case docsResult of
        Left _ ->
          pure
            ( Left
                ( "Could not load docs for "
                    ++ Pkg.toChars packageName
                    ++ "@"
                    ++ Elm.Version.toChars version
                    ++ "."
                )
            )

        Right docs ->
          pure
            ( Right
                (map (renderModuleEntryFromPackage packageName version) (Data.Map.toList docs))
            )


resolvePackageVersion :: Pkg.Name -> Maybe Elm.Version.Version -> IO (Maybe Elm.Version.Version)
resolvePackageVersion _ (Just version) =
  pure (Just version)

resolvePackageVersion packageName Nothing = do
  maybeRoot <- Stuff.findRoot
  case maybeRoot of
    Just root ->
      Ext.Dev.Package.getCurrentlyUsedOrLatestVersion root packageName

    Nothing ->
      Ext.Dev.Package.getPackageNewestPackageVersionFromRegistry packageName


renderProjectDocs :: [String] -> IO (Either String [(Name.Name, Text.Text)])
renderProjectDocs patterns = do
  maybeRoot <- Stuff.findRoot
  case maybeRoot of
    Nothing ->
      pure (Left "Could not find elm.json file in this directory or any parent directories.")

    Just root -> do
      details <- Ext.CompileProxy.loadProject root
      let availableModules = Data.Map.keys (Elm.Details._locals details)
      case resolveModulePatterns patterns availableModules of
        Left err ->
          pure (Left err)

        Right selectedModules -> do
          renderProjectModules root details selectedModules


renderProjectAndDependencyDocs :: IO (Either String ([(Name.Name, Text.Text)], [((Pkg.Name, Elm.Version.Version), [(Name.Name, Text.Text)])]))
renderProjectAndDependencyDocs = do
  maybeRoot <- Stuff.findRoot
  case maybeRoot of
    Nothing ->
      pure (Left "Could not find elm.json file in this directory or any parent directories.")

    Just root -> do
      details <- Ext.CompileProxy.loadProject root

      let localModules = Data.Map.keys (Elm.Details._locals details)
      projectDocsResult <- renderProjectModules root details localModules

      case projectDocsResult of
        Left err ->
          pure (Left err)

        Right projectDocs -> do
          packageVersions <- collectDependencyVersions root details
          packagesRendered <- Monad.forM (Data.Map.toList packageVersions) $ \(packageName, version) -> do
            docsResult <- Ext.Dev.Package.getDocs packageName version
            case docsResult of
              Left _ ->
                pure
                  ( Left
                      ( "Could not load docs for dependency "
                          ++ Pkg.toChars packageName
                          ++ "@"
                          ++ Elm.Version.toChars version
                          ++ "."
                      )
                  )

              Right docs ->
                pure
                  ( Right
                      ( (packageName, version)
                      , map (renderModuleEntryFromPackage packageName version) (Data.Map.toList docs)
                      )
                  )

          pure
            ( case sequence packagesRendered of
                Left err -> Left err
                Right packageDocs -> Right (projectDocs, packageDocs)
            )


renderProjectModules :: FilePath -> Elm.Details.Details -> [Name.Name] -> IO (Either String [(Name.Name, Text.Text)])
renderProjectModules root details selectedModules = do
  rendered <- Monad.forM selectedModules $ \moduleName -> do
    case Ext.Dev.Project.lookupModulePath details moduleName of
      Nothing ->
        pure (Left ("Could not find module " ++ Name.toChars moduleName ++ "."))

      Just modulePath -> do
        maybeDocs <- Ext.Dev.docs root modulePath
        case maybeDocs of
          Nothing ->
            pure (Left ("Could not generate docs for module " ++ Name.toChars moduleName ++ "."))

          Just docsModule ->
            pure (Right (renderModuleEntryFromProject root modulePath (moduleName, docsModule)))

  pure
    ( case sequence rendered of
        Left err -> Left err
        Right entries -> Right entries
    )


collectDependencyVersions :: FilePath -> Elm.Details.Details -> IO (Data.Map.Map Pkg.Name Elm.Version.Version)
collectDependencyVersions root details =
  case Elm.Details._outline details of
    Elm.Details.ValidPkg _ _ exactDeps ->
      pure exactDeps

    Elm.Details.ValidApp _ -> do
      outlineResult <- Elm.Outline.read root
      case outlineResult of
        Left _ ->
          pure Data.Map.empty

        Right (Elm.Outline.Pkg _) ->
          pure Data.Map.empty

        Right (Elm.Outline.App appOutline) ->
          pure
            ( Data.Map.unions
                [ Elm.Outline._app_deps_direct appOutline
                , Elm.Outline._app_deps_indirect appOutline
                , Elm.Outline._app_test_direct appOutline
                , Elm.Outline._app_test_indirect appOutline
                ]
            )


writeOrPrintDocs :: Maybe FilePath -> [(Name.Name, Text.Text)] -> IO ()
writeOrPrintDocs maybeOutputDir docsByModule =
  case maybeOutputDir of
    Nothing ->
      printDocs docsByModule

    Just outputDir ->
      writeDocs outputDir docsByModule


printDocs :: [(Name.Name, Text.Text)] -> IO ()
printDocs docsByModule =
  case docsByModule of
    [] ->
      pure ()

    (firstEntry : remainingEntries) -> do
      TextIO.putStr (snd firstEntry)
      Monad.forM_ remainingEntries $ \(_, markdown) -> do
        TextIO.putStr "\n\n---\n\n"
        TextIO.putStr markdown


writeDocs :: FilePath -> [(Name.Name, Text.Text)] -> IO ()
writeDocs outputDir docsByModule = do
  writeDocsFiles outputDir docsByModule

  putStrLn
    ( "Wrote "
        ++ show (length docsByModule)
        ++ " markdown file(s) to "
        ++ outputDir
        ++ "."
    )


writeDocsFiles :: FilePath -> [(Name.Name, Text.Text)] -> IO ()
writeDocsFiles outputDir docsByModule = do
  Monad.forM_ docsByModule $ \(moduleName, markdown) -> do
    let outputPath = moduleNameToOutputPath outputDir moduleName
    Dir.createDirectoryIfMissing True (Path.takeDirectory outputPath)
    TextIO.writeFile outputPath markdown


printProjectAndDependencyDocs :: [(Name.Name, Text.Text)] -> [((Pkg.Name, Elm.Version.Version), [(Name.Name, Text.Text)])] -> IO ()
printProjectAndDependencyDocs projectDocs packageDocs = do
  putStrLn "# Project Modules"
  printDocs projectDocs

  Monad.forM_ packageDocs $ \((packageName, version), docsByModule) -> do
    putStrLn ""
    putStrLn
      ( "# Package "
          ++ Pkg.toChars packageName
          ++ "@"
          ++ Elm.Version.toChars version
      )
    printDocs docsByModule


writeProjectAndDependencyDocs :: FilePath -> [(Name.Name, Text.Text)] -> [((Pkg.Name, Elm.Version.Version), [(Name.Name, Text.Text)])] -> IO ()
writeProjectAndDependencyDocs outputDir projectDocs packageDocs = do
  let projectOutputDir = outputDir Path.</> "project"
      packagesOutputDir = outputDir Path.</> "packages"

  writeDocsFiles projectOutputDir projectDocs

  Monad.forM_ packageDocs $ \((packageName, version), docsByModule) -> do
    let packageRoot =
          packagesOutputDir
            Path.</> Pkg.toFilePath packageName
            Path.</> Elm.Version.toChars version
    writeDocsFiles packageRoot docsByModule

  let packageCount = length packageDocs
      packageModuleCount = sum (map (length . snd) packageDocs)

  putStrLn
    ( "Wrote docs for "
        ++ show (length projectDocs)
        ++ " project module(s) and "
        ++ show packageModuleCount
        ++ " dependency module(s) across "
        ++ show packageCount
        ++ " package(s) under "
        ++ outputDir
        ++ "."
    )


moduleNameToOutputPath :: FilePath -> Name.Name -> FilePath
moduleNameToOutputPath outputDir moduleName =
  let parts = splitOn '.' (Name.toChars moduleName)
   in Path.joinPath (outputDir : parts) ++ ".md"


resolveModulePatterns :: [String] -> [Name.Name] -> Either String [Name.Name]
resolveModulePatterns rawPatterns availableModules = do
  parsedPatterns <- Monad.mapM parseModulePattern rawPatterns
  let moduleNames = map Name.toChars availableModules
      availableSet = Set.fromList moduleNames

  selections <- Monad.mapM (resolvePattern availableSet moduleNames) parsedPatterns
  let deduped = dedupePreservingOrder (concat selections)

  if null deduped
    then Left "No matching modules were found in this project."
    else Right (map Name.fromChars deduped)


resolvePattern :: Set.Set String -> [String] -> ModulePattern -> Either String [String]
resolvePattern availableSet moduleNames patternToResolve =
  case patternToResolve of
    ExactModule moduleName ->
      let moduleChars = Name.toChars moduleName
       in if Set.member moduleChars availableSet
            then Right [moduleChars]
            else Left ("Module not found in this project: " ++ moduleChars)

    NamespaceModule namespacePrefix ->
      let namespaceStart = namespacePrefix ++ "."
          matches =
            filter
              (\candidate ->
                 Data.List.isPrefixOf namespaceStart candidate
                   && not (Data.List.isInfixOf "." (drop (length namespaceStart) candidate))
              )
              moduleNames
       in if null matches
            then Left ("No modules match pattern: " ++ namespacePrefix ++ ".*")
            else Right matches


dedupePreservingOrder :: [String] -> [String]
dedupePreservingOrder names =
  reverse (snd (foldl step (Set.empty, []) names))
  where
    step (seen, acc) name =
      if Set.member name seen
        then (seen, acc)
        else (Set.insert name seen, name : acc)


parseModulePattern :: String -> Either String ModulePattern
parseModulePattern charsRaw =
  let chars = trimWhitespace charsRaw
   in case Text.stripSuffix ".*" (Text.pack chars) of
        Just namespaceText ->
          let namespace = Text.unpack namespaceText
           in if isValidModuleNamespace namespace
                then Right (NamespaceModule namespace)
                else Left ("Invalid module wildcard pattern: " ++ chars)

        Nothing ->
          case parseElmModule chars of
            Nothing -> Left ("Invalid module name: " ++ chars)
            Just modul -> Right (ExactModule modul)


isValidModuleNamespace :: String -> Bool
isValidModuleNamespace chars =
  case splitOn '.' chars of
    [] ->
      False

    parts ->
      not (null chars) && all isValidElmPiece parts


parseDocsInput :: [String] -> Either String DocsInput
parseDocsInput [] =
  Right DocsFromProjectAndDependencies

parseDocsInput args =
  let packageLike = filter isPackageArgument args
      moduleLike = filter (not . isPackageArgument) args
   in case packageLike of
        [] ->
          Right (DocsFromModules moduleLike)

        [packageText] ->
          if null moduleLike
            then fmap (\(pkg, maybeVersion) -> DocsFromPackage pkg maybeVersion) (parsePackageWithVersion packageText)
            else Left "When providing a package, do not provide module names."

        _ ->
          Left "Please provide only one package name."


isPackageArgument :: String -> Bool
isPackageArgument chars =
  Data.List.isInfixOf "/" chars


parsePackageWithVersion :: String -> Either String (Pkg.Name, Maybe Elm.Version.Version)
parsePackageWithVersion raw =
  case splitOn '@' (trimWhitespace raw) of
    [pkgChars] ->
      case parsePkgName pkgChars of
        Nothing -> Left "Invalid package name. Expected author/project"
        Just pkg -> Right (pkg, Nothing)

    [pkgChars, versionChars] ->
      case (parsePkgName pkgChars, parseSemver versionChars) of
        (Nothing, _) -> Left "Invalid package name. Expected author/project"
        (_, Nothing) -> Left "Invalid version. Expected MAJOR.MINOR.PATCH"
        (Just pkg, Just version) -> Right (pkg, Just version)

    _ ->
      Left "Invalid package format. Expected author/project or author/project@MAJOR.MINOR.PATCH"


parseSemver :: String -> Maybe Elm.Version.Version
parseSemver versionChars =
  case splitOn '.' versionChars of
    [majorChars, minorChars, patchChars] -> do
      major <- readWord16 majorChars
      minor <- readWord16 minorChars
      patch <- readWord16 patchChars
      pure (Elm.Version.Version major minor patch)

    _ ->
      Nothing


readWord16 :: String -> Maybe Word.Word16
readWord16 chars =
  case Read.readMaybe chars :: Maybe Int of
    Nothing ->
      Nothing

    Just value ->
      if value < 0 || value > 65535
        then Nothing
        else Just (fromIntegral value)


renderModuleEntryFromProject :: FilePath -> FilePath -> (Name.Name, Docs.Module) -> (Name.Name, Text.Text)
renderModuleEntryFromProject root modulePath (moduleName, docsModule) =
  let moduleNameText = Text.pack (Name.toChars moduleName)
      meta =
        DocsMarkdown.ModuleMeta
          moduleNameText
          (Just root)
          (Just modulePath)
          Nothing
      fullBody = DocsMarkdown.renderModule meta docsModule
   in (moduleName, fullBody)


renderModuleEntryFromPackage :: Pkg.Name -> Elm.Version.Version -> (Name.Name, Docs.Module) -> (Name.Name, Text.Text)
renderModuleEntryFromPackage packageName version (moduleName, docsModule) =
  let moduleNameText = Text.pack (Name.toChars moduleName)
      packageText = Text.pack (Pkg.toChars packageName ++ "@" ++ Elm.Version.toChars version)
      meta =
        DocsMarkdown.ModuleMeta
          moduleNameText
          Nothing
          Nothing
          (Just packageText)
      fullBody = DocsMarkdown.renderModule meta docsModule
   in (moduleName, fullBody)

mcpCommand :: CommandParser.Command
mcpCommand = CommandParser.command ["mcp"] "Start the Elm Dev MCP server" devGroup CommandParser.noArg parseServerFlags runServer
  where
    parseServerFlags = CommandParser.noFlag
    runServer _ _ = do
      info <- Daemon.ensureRunning
      let host = Daemon.domain info
      let port = Daemon.mcpPort info
      Watchtower.Server.Proxy.run host port

lspCommand :: CommandParser.Command
lspCommand = CommandParser.command ["lsp"] "Start the Elm Dev LSP server" devGroup CommandParser.noArg parseServerFlags runServer
  where
    parseServerFlags = CommandParser.noFlag
    runServer _ _ = do
      info <- Daemon.ensureRunning 
      Watchtower.Server.Proxy.run (Daemon.domain info) (Daemon.lspPort info)


-- Daemon commands
devServeCommand :: CommandParser.Command
devServeCommand = CommandParser.command ["dev","serve"] "Run the Elm Dev dev (internal)" devGroup parseArgs parseFlags runCmd
  where
    parseArgs = CommandParser.noArg
    silentFlag = CommandParser.flag "silent" "Do not print startup banner"
    parseFlags = CommandParser.parseFlag silentFlag
    runCmd _ maybeSilent = do
      case maybeSilent of
        Just True -> Ext.Log.setMode Ext.Log.Silent
        _ -> Ext.Log.setMode Ext.Log.StdOut
      params <- Daemon.allocateServeParams
      Daemon.serve params
      case maybeSilent of
        Just True -> pure ()
        _ -> Daemon.printBanner params

devStartCommand :: CommandParser.Command
devStartCommand = CommandParser.command ["dev","start"] "Start dev if not running" devGroup parseArgs parseFlags runCmd
  where
    parseArgs = CommandParser.noArg
    parseFlags = CommandParser.noFlag
    runCmd _ _ = do
      status <- Daemon.ensureRunning
      LBSChar.putStrLn (encodeStatus status)

devStopCommand :: CommandParser.Command
devStopCommand = CommandParser.command ["dev","stop"] "Stop dev" devGroup parseArgs parseFlags runCmd
  where
    parseArgs = CommandParser.noArg
    parseFlags = CommandParser.noFlag
    runCmd _ _ = do
      Daemon.stop


encodeStatus :: Daemon.StateInfo -> LBS.ByteString
encodeStatus s = 
  let domainStr = Json.Encode.string (Json.String.fromChars (Daemon.domain s)) in
  Data.ByteString.Builder.toLazyByteString (
      let lspObj = Json.Encode.object [ ("domain" ==> domainStr), ("port" ==> Json.Encode.int (Daemon.lspPort s)) ]
          mcpObj = Json.Encode.object [ ("domain" ==> domainStr), ("port" ==> Json.Encode.int (Daemon.mcpPort s)) ]
          httpObj = Json.Encode.object [ ("domain" ==> domainStr), ("port" ==> Json.Encode.int (Daemon.httpPort s)) ]
      in Json.Encode.encodeUgly (Json.Encode.object [
              ("pid" ==> Json.Encode.int (Daemon.pid s)),
              ("lsp" ==> lspObj),
              ("mcp" ==> mcpObj),
              ("http" ==> httpObj),
              ("version" ==> Json.Encode.string (Json.String.fromChars (Daemon.version s)))
          ])
      )

devStatusCommand :: CommandParser.Command
devStatusCommand = CommandParser.command ["dev","status"] "Show dev server status" devGroup parseArgs parseFlags runCmd
  where
    parseArgs = CommandParser.noArg
    parseFlags = CommandParser.noFlag
    runCmd _ _ = do
      st <- Daemon.status
      case st of
        Nothing -> IO.hPutStrLn IO.stderr "dev server: not running"
        Just status ->
          LBSChar.putStrLn (encodeStatus status)

-- Discover Elm projects under the current root and print them
discoverCommand :: CommandParser.Command
discoverCommand = CommandParser.command ["discover"] "Discover Elm projects" devGroup CommandParser.noArg parseFlags runCmd
  where
    parseFlags = CommandParser.noFlag
    runCmd _ _ = do
      root <- Dir.getCurrentDirectory
      projects <- Ext.Dev.Project.discover root
      let json = Json.Encode.list Ext.Dev.Project.encodeProjectJson projects
      LBSChar.putStrLn (Data.ByteString.Builder.toLazyByteString (Json.Encode.encode json))

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

version :: String
version = ElmDevVersion.version

-- Main function
main :: IO ()
main = do
  CommandParser.run
    version
    ( \commands givenCommand ->
        -- Show Help
        let 
          joinArgs [] = ""
          joinArgs argList = " " ++ Data.List.intercalate " " argList
          
          header =
              if null givenCommand
                then [ "",
                        "Welcome to Elm Dev",
                        ""
                      ]
                else [ "", 
                        "I don't recognize " ++ Terminal.Colors.yellow ("elm-dev " ++ Data.List.intercalate " " givenCommand) ++ ".",
                        "Here's what I know:",
                        ""
                      ]
        in unlines header
              ++ 
        -- Group commands by their group (if any)
        let includeDev = elem "dev" givenCommand
            visibleCommands = filter (\m -> CommandParser.cmdGroup m /= devGroup || includeDev) commands
            groupedCommands = Data.List.groupBy (\a b -> CommandParser.cmdGroup a == CommandParser.cmdGroup b) visibleCommands
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
      installCommand,
      docsMarkdownCommand,
      Gen.Commands.addPage,
      Gen.Commands.addStore,
      Gen.Commands.addEffect,
      Gen.Commands.addListener,
      -- Gen.Commands.addDocs,
      -- Gen.Commands.addTheme,
      Gen.Commands.customize,
      
      
      testInitCommand,
      testInstallCommand,
      testCommand,
      devServeCommand,
      devStartCommand,
      devStopCommand,
      devStatusCommand,
      discoverCommand,
      mcpCommand,
      lspCommand
      -- entrypointsCommand,
      -- docsCommand,
      -- warningsCommand,
      -- importsCommand,
      -- usageCommand,
      -- explainCommand,
      
    ]

testGroup :: Maybe String
testGroup = Just "Testing"

-- elm-dev install <author/project>
installCommand :: CommandParser.Command
installCommand = CommandParser.command ["install"] "Install a package" Nothing parseArgs parseFlags runCmd
  where
    parseArgs = CommandParser.parseOptionalArg (CommandParser.arg "author/project")
    parseFlags = CommandParser.noFlag
    runCmd maybePkgStr _ = do
      case maybePkgStr of
        Nothing -> Install.run Install.NoArgs ()
        Just pkgStr ->
          case parsePkgName pkgStr of
            Nothing -> IO.hPutStrLn IO.stderr "Invalid package name. Expected author/project"
            Just pkg -> Install.run (Install.Install pkg) ()

-- Test command
testCommand :: CommandParser.Command
testCommand = CommandParser.command ["test"] "Discover, compile, and run Elm tests" testGroup CommandParser.noArg parseFlags runCmd
  where
    seedFlag = CommandParser.flagWithArg "seed" "Random seed for fuzz tests" (Text.Read.readMaybe)
    fuzzFlag = CommandParser.flagWithArg "fuzz" "Number of fuzz runs per test" (Text.Read.readMaybe)
    parseFlags = CommandParser.parseFlag2 seedFlag fuzzFlag
    runCmd _ (maybeSeed, maybeFuzz) =  Ext.Log.withAllBut [Ext.Log.Performance] $ do
      -- Ext.Log.setMode Ext.Log.StdOut
      -- Ext.CompileMode.setModeMemory
      -- Ext.Log.log Ext.Log.Misc "Running test command"
      maybeRoot <- Stuff.findRoot
      case maybeRoot of
        Nothing -> IO.hPutStrLn IO.stderr "Could not find project root"
        Just root -> do
          startPs <- CPUTime.getCPUTime
          result <- Ext.Test.Runner.run Nothing Nothing maybeSeed maybeFuzz root
          case result of
            Left e -> case e of
              Ext.Test.Runner.TimedOut waited -> IO.hPutStrLn IO.stderr ("Timed out running tests after " <> show waited <> "s")
              Ext.Test.Runner.RunFailed msg -> IO.hPutStrLn IO.stderr msg
            Right runResults -> do
              endPs <- CPUTime.getCPUTime
              let durationMs :: Int
                  durationMs = fromInteger ((endPs - startPs) `div` 1000000000)
              putStrLn (Ext.Test.Result.Report.renderReportsWithDuration True (Just durationMs) (Just (Ext.Test.Runner.seed runResults)) (Just (Ext.Test.Runner.fuzz runResults)) (Ext.Test.Runner.reports runResults))


-- elm-dev test init
testInitCommand :: CommandParser.Command
testInitCommand = CommandParser.command ["test","init"] "Setup testing" testGroup CommandParser.noArg parseFlags runCmd
  where
    parseFlags = CommandParser.noFlag
    runCmd _ _ = do
      Ext.CompileMode.setModeMemory
      maybeRoot <- Stuff.findRoot
      case maybeRoot of
        Nothing -> IO.hPutStrLn IO.stderr "Could not find project root"
        Just root -> do
          -- Install elm-explorations/test into test-dependencies
          result <- Ext.Test.Install.installTestDependency (Pkg.toName (Utf8.fromChars "elm-explorations") "test")
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


-- elm-dev test install <author/project>
testInstallCommand :: CommandParser.Command
testInstallCommand = CommandParser.command ["test","install"] "Install test dep" testGroup parseArgs parseFlags runCmd
  where
    parseArgs = CommandParser.parseArg (CommandParser.arg "author/project")
    parseFlags = CommandParser.noFlag
    runCmd pkgStr _ = do      
      case parsePkgName pkgStr of
        Nothing -> IO.hPutStrLn IO.stderr "Invalid package name. Expected author/project"
        Just pkg -> do
          result <- Ext.Test.Install.installTestDependency pkg
          case result of
            Left _ -> IO.hPutStrLn IO.stderr "Failed to install test dependency"
            Right _ -> IO.hPutStrLn IO.stdout (Terminal.Colors.yellow pkgStr ++ " has been installed as a test dependency!")

-- Helper: parse "author/project" into Pkg.Name
parsePkgName :: String -> Maybe Pkg.Name
parsePkgName str =
  case break (== '/') str of
    (author, '/':project) | not (null author) && not (null project) ->
      Just (Pkg.toName (Utf8.fromChars author) project)
    _ -> Nothing
