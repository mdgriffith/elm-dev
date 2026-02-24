{-# LANGUAGE OverloadedStrings #-}

module Gen.Commands.Make (command, run) where

import qualified CommandParser
import Control.Monad (when)
import Data.Aeson (eitherDecodeStrict)
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString as SBS
import qualified Data.ByteString.Char8 as SBC
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
import qualified Http
import qualified Reporting.Exit as Exit
import qualified Stuff
import qualified System.Directory as Dir (createDirectoryIfMissing, doesFileExist, getCurrentDirectory, removeFile, withCurrentDirectory)
import System.FilePath ((<.>), (</>))
import qualified System.FilePath as FP
import qualified System.IO as IO
import qualified System.Environment as Env
import Terminal ((!), (...), (?))
import qualified Terminal
import qualified Terminal.Colors
import qualified Terminal.Helpers
import qualified Text.PrettyPrint.ANSI.Leijen as P
import qualified Watchtower.Server.Daemon
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types.Status as Status

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
      CommandParser.parseFlag5
        (CommandParser.flag "debug" "Debug mode")
        (CommandParser.flag "optimize" "Make the code smaller and faster")
        (CommandParser.flagWithArg "output" "Output file path" Make.parseOutput)
        (CommandParser.flagWithArg "report" "Report type (json)" parseReportType)
        (CommandParser.flag "hot" "Enable hot reload")
    parseMakeArgs =
      CommandParser.parseOptionalArgList (CommandParser.arg "module")

parseReportType :: String -> Maybe Make.ReportType
parseReportType "json" = Just Make.Json
parseReportType _ = Nothing

run :: Maybe FilePath -> [String] -> (Maybe Bool, Maybe Bool, Maybe Make.Output, Maybe Make.ReportType, Maybe Bool) -> IO ()
run maybeCwd modulesArg (debug, optimize, output, report, hot) = do
  cwd <- Dir.getCurrentDirectory
  modules <- resolveModules cwd modulesArg
  codegenResult <- Gen.Generate.run cwd
  case codegenResult of
    Left err ->
      putStrLn err
    Right () ->
      if fromMaybe False hot
        then do
          maybeDaemonStatus <- Watchtower.Server.Daemon.status

          case maybeDaemonStatus of
            Just st ->
              case modules of
                fstModule : _ -> do
                  let base = "http://" ++ Watchtower.Server.Daemon.domain st ++ ":" ++ show (Watchtower.Server.Daemon.httpPort st) ++ "/dev/js"
                  let qs =
                        [ ("dir", cwd)
                        , ("file", fstModule)
                        , ("debug", if fromMaybe False debug then "true" else "false")
                        , ("optimize", if fromMaybe False optimize then "true" else "false")
                        ]
                  let url = Http.toUrl base qs
                  _ <- hotDevRequest url output
                  return ()

                [] ->
                  putStrLn "No entrypoints provided."

            Nothing ->
              putStrLn "Daemon not running"
        else
          Dir.withCurrentDirectory cwd $ do
            Ext.Log.with [Ext.Log.ElmCompilerError] $
              Make.run
                modules
                ( Make.Flags
                    (fromMaybe False debug)
                    (fromMaybe False optimize)
                    output
                    report
                    Nothing
                )


resolveModules :: FilePath -> [String] -> IO [String]
resolveModules cwd modulesArg = do
  if not (null modulesArg)
    then return modulesArg
    else do
      hasElmDevJson <- Dir.doesFileExist (cwd </> "elm.dev.json")
      if hasElmDevJson
        then return ["elm-stuff/generated/Main.elm"]
        else return modulesArg

-- | GET the dev JS and handle output based on HTTP status
hotDevRequest :: String -> Maybe Make.Output -> IO (Either () ())
hotDevRequest url maybeOutput = do
  manager <- Http.getManager
  req <- HTTP.parseUrlThrow url
  HTTP.withResponse req manager $ \response -> do
    chunks <- HTTP.brConsume (HTTP.responseBody response)
    let body = SBS.concat chunks
    let code = Status.statusCode (HTTP.responseStatus response)
    if code == 200
      then do
        writeOutput maybeOutput body
        return (Right ())
      else do
        SBS.hPut IO.stderr body
        return (Right ())
  where
    writeOutput :: Maybe Make.Output -> SBS.ByteString -> IO ()
    writeOutput out bytes =
      case out of
        Just Make.DevNull -> return ()
        Just (Make.JS Make.Stdout) -> SBS.hPut IO.stdout bytes
        Just (Make.JS (Make.File path)) -> do
          Dir.createDirectoryIfMissing True (FP.takeDirectory path)
          SBS.writeFile path bytes
        Just (Make.Html Make.Stdout) -> SBS.hPut IO.stdout bytes
        Just (Make.Html (Make.File path)) -> do
          Dir.createDirectoryIfMissing True (FP.takeDirectory path)
          SBS.writeFile path bytes
        Nothing -> do
          let defaultPath = "elm.js"
          Dir.createDirectoryIfMissing True (FP.takeDirectory defaultPath)
          SBS.writeFile defaultPath bytes
