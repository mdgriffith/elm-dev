{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE StandaloneDeriving #-}

module MainDev (main) where

import CommandParser
import qualified System.IO as IO
import qualified System.Exit as Exit
import qualified System.Process as Process
import qualified System.FilePath as Path
import qualified System.Directory as Dir
import qualified Data.Char as Char
import qualified Control.Monad as Monad
import qualified Text.PrettyPrint.ANSI.Leijen as P
import qualified Watchtower.Server
import qualified Watchtower.Live
import qualified Ext.Common
import qualified Data.Utf8 as Utf8
import qualified Data.ByteString.Builder
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8
import qualified Data.Map
import qualified Data.Maybe
import qualified Data.NonEmptyList as NE
import qualified Data.Name as Name
import qualified Data.ByteString
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSChar
import qualified Ext.Dev
import qualified Ext.Dev.Package
import qualified Ext.Dev.EntryPoints
import qualified Ext.Dev.Find
import qualified Ext.Dev.Lookup
import qualified Ext.Dev.Json.Encode
import qualified Ext.Dev.Imports
import qualified Ext.Dev.Project
import qualified Ext.Dev.Usage
import qualified Ext.Dev.CallGraph
import qualified Ext.Dev.Explain
import qualified Ext.CompileProxy
import qualified Reporting
import qualified Reporting.Render.Type.Localizer
import qualified Reporting.Exit as Exit
import qualified Elm.Docs as Docs
import qualified Json.String
import qualified Json.Decode
import qualified Json.Encode
import qualified Elm.ModuleName
import qualified Elm.Details
import qualified Elm.Outline
import qualified Elm.Package as Pkg
import qualified Elm.Version
import qualified Stuff
import qualified File
import qualified Terminal.Dev.Args
import qualified Terminal.Dev.Out
import qualified Terminal.Dev.Error
import qualified Terminal.Helpers
import qualified Make
import qualified Build
import qualified BackgroundWriter
import Json.Encode ((==>))
import qualified Gen.Commands
import qualified Text.Read
import qualified Terminal.Colors

-- Helper functions
parseMaybeInt :: String -> Maybe Int
parseMaybeInt str = Text.Read.readMaybe str

parseModuleList :: String -> Maybe (NE.List Elm.ModuleName.Raw)
parseModuleList str = case Data.Maybe.catMaybes $ fmap parseElmModule (splitOn ',' str) of
  [] -> Nothing
  (top : remain) -> Just (NE.List top remain)

parseElmModule :: String -> Maybe Elm.ModuleName.Raw
parseElmModule charsRaw =
  let chars = trimWhitespace charsRaw
  in if length chars == 0 then
    Nothing
  else
    let pieces = splitOn '.' chars
    in if all isValidElmPiece pieces then
      Just (Name.fromChars chars)
    else
      Nothing

isValidElmPiece :: String -> Bool
isValidElmPiece [] = False
isValidElmPiece (x:xs) = Char.isUpper x && all isValidChar xs
  where
    isValidChar c = Char.isAlphaNum c || c == '_'

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn delimiter = go
  where
    go [] = []
    go xs = 
      let (before, remainder) = break (== delimiter) xs
      in before : case remainder of
        [] -> []
        _:after -> go after

trimWhitespace :: String -> String
trimWhitespace = reverse . dropWhile Char.isSpace . reverse . dropWhile Char.isSpace

-- Command metadata
serverMetadata :: CommandMetadata
serverMetadata = CommandMetadata
  { cmdName = "server"
  , cmdDesc = "Start the Elm Dev server"
  , cmdArgs = []
  , cmdSubcommands = []
  }

docsMetadata :: CommandMetadata
docsMetadata = CommandMetadata
  { cmdName = "docs"
  , cmdDesc = "Report the docs.json for the given package or local file"
  , cmdArgs = []
  , cmdSubcommands = []
  }

warningsMetadata :: CommandMetadata
warningsMetadata = CommandMetadata
  { cmdName = "warnings"
  , cmdDesc = "Report missing type annotations and unused code"
  , cmdArgs = []
  , cmdSubcommands = []
  }

importsMetadata :: CommandMetadata
importsMetadata = CommandMetadata
  { cmdName = "imports"
  , cmdDesc = "Given a file, report everything it imports"
  , cmdArgs = []
  , cmdSubcommands = []
  }

usageMetadata :: CommandMetadata
usageMetadata = CommandMetadata
  { cmdName = "usage"
  , cmdDesc = "Given a file, report all modules that use it in your project"
  , cmdArgs = []
  , cmdSubcommands = []
  }

entrypointsMetadata :: CommandMetadata
entrypointsMetadata = CommandMetadata
  { cmdName = "entrypoints"
  , cmdDesc = "Given a directory with an elm.json, report all entrypoints for the project"
  , cmdArgs = []
  , cmdSubcommands = []
  }

explainMetadata :: CommandMetadata
explainMetadata = CommandMetadata
  { cmdName = "explain"
  , cmdDesc = "Given a qualified value, explain it's definition"
  , cmdArgs = []
  , cmdSubcommands = []
  }

-- Command handlers
serverCommand :: CommandParser.Command
serverCommand args = case CommandParser.parsedCommands args of
  ["server"] -> CommandParser.Run $ \() -> do
    case CommandParser.parseFlag portFlag args of
      Left err -> putStrLn err
      Right maybePort -> Watchtower.Server.serve Nothing (Watchtower.Server.Flags maybePort)
  _ -> CommandParser.NotMe serverMetadata
  where
    portFlag = CommandParser.flagWithArg "port" "Port to run the server on" parseMaybeInt

docsCommand :: CommandParser.Command
docsCommand args = case CommandParser.parsedCommands args of
  ["docs"] -> CommandParser.Run $ \() -> do
    case CommandParser.parseFlag outputFlag args of
      Left err -> putStrLn err
      Right maybeOutput -> do
        let path = case CommandParser.parsedPositional args of
              [] -> "."
              (p:_) -> p
        maybeRoot <- Stuff.findRoot
        case maybeRoot of
          Nothing ->
            Terminal.Dev.Out.json maybeOutput (Left Terminal.Dev.Error.CouldNotFindRoot)
          
          Just root -> do
            maybeDocs <- Ext.Dev.docs root path
            case maybeDocs of
              Nothing ->
                Terminal.Dev.Out.json maybeOutput (Left Terminal.Dev.Error.CouldNotFindModule)
              
              Just docs ->
                Terminal.Dev.Out.json maybeOutput (Right (Docs.encode (Data.Map.singleton (Docs._name docs) docs)))
  _ -> CommandParser.NotMe docsMetadata
  where
    outputFlag = CommandParser.flagWithArg "output" "Output file path" Just

warningsCommand :: CommandParser.Command
warningsCommand args = case CommandParser.parsedCommands args of
  ["warnings"] -> CommandParser.Run $ \() -> do
    case CommandParser.parseFlag outputFlag args of
      Left err -> putStrLn err
      Right maybeOutput -> do
        let path = case CommandParser.parsedPositional args of
              [] -> "."
              (p:_) -> p
        compilationCheckResult <- loadAndEnsureCompiled (Just (NE.singleton (Name.fromChars path)))
        case compilationCheckResult of
          Left err ->
            Terminal.Dev.Out.json maybeOutput (Left err)
          
          Right details -> do
            moduleResult <- Terminal.Dev.Args.modul path
            case moduleResult of
              Left err ->
                Terminal.Dev.Out.json maybeOutput (Left err)
              
              Right (Terminal.Dev.Args.Module root (Terminal.Dev.Args.ModuleInfo moduleName modulePath) details) -> do
                eitherWarnings <- Ext.Dev.warnings root modulePath
                case eitherWarnings of
                  Left err ->
                    Terminal.Dev.Out.json maybeOutput
                      (Right 
                        (Json.Encode.list id 
                          [ Json.Encode.object
                              [ "filepath" ==> Json.Encode.string (Json.String.fromChars modulePath)
                              , "module" ==> Json.Encode.name moduleName
                              , "warnings" ==> Json.Encode.list id []
                              ]
                          ]
                        )
                      )
                  
                  Right (mod, warningList) ->
                    Terminal.Dev.Out.json maybeOutput
                      (Right 
                        (Json.Encode.list id 
                          [ Json.Encode.object
                              [ "filepath" ==> Json.Encode.string (Json.String.fromChars modulePath)
                              , "module" ==> Json.Encode.name moduleName
                              , "warnings" ==> Json.Encode.list
                                  (Watchtower.Live.encodeWarning (Reporting.Render.Type.Localizer.fromModule mod))
                                  warningList
                              ]
                          ]
                        )
                      )
  _ -> CommandParser.NotMe warningsMetadata
  where
    outputFlag = CommandParser.flagWithArg "output" "Output file path" Just

importsCommand :: CommandParser.Command
importsCommand args = case CommandParser.parsedCommands args of
  ["imports"] -> CommandParser.Run $ \() -> do
    case CommandParser.parseFlag2 outputFlag entrypointsFlag args of
      Left err -> putStrLn err
      Right (maybeOutput, maybeEntrypoints) -> do
        let paths = CommandParser.parsedPositional args
        if null paths then
          Terminal.Dev.Out.json maybeOutput (Left Terminal.Dev.Error.CouldNotFindModule)
        else do
          result <- loadAndEnsureCompiled maybeEntrypoints
          case result of
            Left err ->
              Terminal.Dev.Out.json maybeOutput (Left err)
            
            Right details -> do
              let importSummary = Ext.Dev.Imports.getImportSummaryForMany details (fmap Name.fromChars paths)
              Terminal.Dev.Out.json maybeOutput
                (Right 
                  (Ext.Dev.Imports.encodeSummary
                      importSummary
                  )
                )
  _ -> CommandParser.NotMe importsMetadata
  where
    outputFlag = CommandParser.flagWithArg "output" "Output file path" Just
    entrypointsFlag = CommandParser.flagWithArg "entrypoints" "Comma-separated list of entrypoint modules" parseModuleList

usageCommand :: CommandParser.Command
usageCommand args = case CommandParser.parsedCommands args of
  ["usage"] -> CommandParser.Run $ \() -> do
    case CommandParser.parseFlag2 outputFlag entrypointsFlag args of
      Left err -> putStrLn err
      Right (maybeOutput, maybeEntrypoints) -> do
        let path = case CommandParser.parsedPositional args of
              [] -> "."
              (p:_) -> p
        result <- loadAndEnsureCompiled maybeEntrypoints
        case result of
          Left err ->
            Terminal.Dev.Out.json maybeOutput (Left err)
          
          Right details -> do
            usageSummary <- Ext.Dev.Usage.usageOfModule "." details (Name.fromChars path)
            case usageSummary of
              Nothing ->
                Terminal.Dev.Out.json maybeOutput
                  (Left Terminal.Dev.Error.CouldNotFindModule)
              
              Just summary ->
                Terminal.Dev.Out.json maybeOutput
                  (Right 
                    (Ext.Dev.Usage.encode
                        summary
                    )
                  )
  _ -> CommandParser.NotMe usageMetadata
  where
    outputFlag = CommandParser.flagWithArg "output" "Output file path" Just
    entrypointsFlag = CommandParser.flagWithArg "entrypoints" "Comma-separated list of entrypoint modules" parseModuleList

entrypointsCommand :: CommandParser.Command
entrypointsCommand args = case CommandParser.parsedCommands args of
  ["entrypoints"] -> CommandParser.Run $ \() -> do
    case CommandParser.parseFlag outputFlag args of
      Left err -> putStrLn err
      Right maybeOutput -> do
        maybeRoot <- Stuff.findRoot
        case maybeRoot of
          Nothing ->
            Terminal.Dev.Out.json maybeOutput
              (Left (Terminal.Dev.Error.CouldNotFindRoot))
          
          Just root -> do
            entryResult <- Ext.Dev.entrypoints root
            case entryResult of
              Left err ->
                Terminal.Dev.Out.json maybeOutput
                  (Left (Terminal.Dev.Error.CompilationError err))
              
              Right entry ->
                Terminal.Dev.Out.json maybeOutput
                  (Right (Json.Encode.list Ext.Dev.EntryPoints.encode entry))
  _ -> CommandParser.NotMe entrypointsMetadata
  where
    outputFlag = CommandParser.flagWithArg "output" "Output file path" Just

explainCommand :: CommandParser.Command
explainCommand args = case CommandParser.parsedCommands args of
  ["explain"] -> CommandParser.Run $ \() -> do
    case CommandParser.parseFlag outputFlag args of
      Left err -> putStrLn err
      Right maybeOutput -> do
        let path = case CommandParser.parsedPositional args of
              [] -> "."
              (p:_) -> p
        valueResult <- Terminal.Dev.Args.value path
        case valueResult of
          Left err ->
            Terminal.Dev.Out.json maybeOutput (Left err)
          
          Right (Terminal.Dev.Args.Value root modName valueName) -> do
            compilationCheckResult <- Ext.CompileProxy.loadAndEnsureCompiled root (Just (NE.List modName []))
            case mapError Terminal.Dev.Error.CompilationError compilationCheckResult of
              Left err ->
                Terminal.Dev.Out.json maybeOutput
                  (Left err)
              
              Right details -> do
                maybeFound <- Ext.Dev.Explain.explain details root modName valueName
                case maybeFound of
                  Nothing ->
                    Terminal.Dev.Out.json maybeOutput (Left (Terminal.Dev.Error.CouldNotFindModule))
                  
                  Just definition ->
                    Terminal.Dev.Out.json maybeOutput (Right (Ext.Dev.Explain.encode definition))
  _ -> CommandParser.NotMe explainMetadata
  where
    outputFlag = CommandParser.flagWithArg "output" "Output file path" Just

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

-- Main function
main :: IO ()
main = do
--   putStrLn "Welcome to Elm Dev!"
  runCommands
    (\commands givenCommand -> 
        -- Show Help
        unlines
          [ "" 
          , "Welcome to Elm Dev"
          , ""
          , "  elm-dev " ++ Terminal.Colors.green "server" ++ " [port] - Start the Elm Dev server"
          , "  elm-dev " ++ Terminal.Colors.green "docs" ++ " [output] - Report the docs.json for the given package or local file"
          , "  elm-dev " ++ Terminal.Colors.green "warnings" ++ " [output] - Report missing type annotations and unused code"
          , "  elm-dev " ++ Terminal.Colors.green "imports" ++ " [output] - Given a file, report everything it imports"
          ]
    )
    [ serverCommand
    , docsCommand
    , warningsCommand
    , importsCommand
    , usageCommand
    , entrypointsCommand
    , explainCommand
    ]
--   putStrLn "Happy hacking!"


