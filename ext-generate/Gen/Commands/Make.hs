{-# LANGUAGE OverloadedStrings #-}

module Gen.Commands.Make (command, run) where

import qualified CommandParser
import Control.Monad (when)
import Data.Aeson (eitherDecodeStrict)
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BS
import qualified Data.Char as Char
import Data.Function ((&))
import qualified Data.List (find)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Maybe as Maybe
import qualified Data.Name as Name
import Data.Text (Text, pack)
import qualified Data.Text as Text
import qualified Data.Text.Encoding
import qualified Data.Text.IO as TIO
import qualified Elm.ModuleName
import qualified Ext.CompileProxy
import qualified Ext.Log
import qualified Gen.Commands.Init
import qualified Gen.Config
import qualified Gen.Config as Config
import qualified Gen.Generate
import qualified Gen.Javascript
import qualified Gen.Templates
import qualified Gen.Templates.Loader
import qualified Make
import qualified Reporting.Exit as Exit
import qualified Stuff
import qualified System.Directory as Dir (createDirectoryIfMissing, doesFileExist, getCurrentDirectory, removeFile, withCurrentDirectory)
import System.FilePath ((<.>), (</>))
import qualified System.FilePath as FP
import qualified System.IO as IO
import Terminal ((!), (...), (?))
import qualified Terminal
import qualified Terminal.Colors
import qualified Terminal.Helpers
import qualified Text.PrettyPrint.ANSI.Leijen as P

-- MAKE COMMAND
command :: CommandParser.Command
command =
  CommandParser.command
    ["make"]
    "Build your Elm project"
    Nothing
    parseMakeArgs
    parseMakeFlags
    (run Nothing)
  where
    parseMakeFlags =
      CommandParser.parseFlag4
        (CommandParser.flag "debug" "Debug mode")
        (CommandParser.flag "optimize" "Make the code smaller and faster")
        (CommandParser.flagWithArg "output" "Output file path" Make.parseOutput)
        (CommandParser.flagWithArg "report" "Report type (json)" parseReportType)
    parseMakeArgs =
      CommandParser.parseArgList (CommandParser.arg "module")

parseReportType :: String -> Maybe Make.ReportType
parseReportType "json" = Just Make.Json
parseReportType _ = Nothing

run :: Maybe FilePath -> (String, [String]) -> (Maybe Bool, Maybe Bool, Maybe Make.Output, Maybe Make.ReportType) -> IO ()
run maybeCwd (fstModule, modules) (debug, optimize, output, report) = do
  let cwd = fromMaybe "." maybeCwd
  Dir.withCurrentDirectory cwd $ do
    codegenResult <- Gen.Generate.run
    case codegenResult of
      Right () -> do
        Ext.Log.with [Ext.Log.ElmCompilerError] $
          Make.run
            (fstModule : modules)
            ( Make.Flags
                (fromMaybe False debug)
                (fromMaybe False optimize)
                output
                report
                Nothing
            )
        return ()
      Left err -> do
        putStrLn err
        return ()
