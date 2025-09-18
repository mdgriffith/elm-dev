{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Watchtower.Server.Dev (routes) where

import qualified Control.Concurrent.STM as STM
import Data.Aeson ((.=))
import qualified Data.Aeson as JSON
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB
import qualified Data.Text as Text
import qualified Data.Text.Encoding
import qualified Elm.Docs as Docs
import qualified Ext.Dev
import qualified Gen.Generate
import qualified Gen.Javascript
import qualified Json.Encode
import qualified Watchtower.Live as Live
import qualified Watchtower.Live.Client as Client
import qualified Watchtower.Server.DevWS as DevWS
import qualified Watchtower.Server.JSONRPC as JSONRPC
import qualified Watchtower.State.Project
import qualified Watchtower.State.Compile
import qualified Modify.Inject.Loader
import qualified Ext.CompileHelpers.Generic as CompileHelpers
import qualified Data.NonEmptyList as NE
import qualified Data.List as List
import qualified Reporting.Exit

import qualified Ext.Log
import Snap.Core
import Control.Monad.Trans (liftIO)
import qualified Ext.FileCache
import qualified File

-- | JSON decoders/encoders live in Watchtower.Server.JSONRPC; we mirror MCP style here

-- Parameters for js method
data JsParams = JsParams
  { jsDir :: FilePath
  , jsFile :: FilePath
  , jsDebug :: Maybe Bool
  , jsOptimize :: Maybe Bool
  }

instance JSON.FromJSON JsParams where
  parseJSON = JSON.withObject "JsParams" $ \o ->
    JsParams <$> o JSON..: "dir"
             <*> o JSON..: "file"
             <*> o JSON..:? "debug"
             <*> o JSON..:? "optimize"

-- Parameters for log method
data LogParams = LogParams
  { logDir :: FilePath
  , module_ :: String
  , identifier :: String
  , payload :: JSON.Value
  }

instance JSON.FromJSON LogParams where
  parseJSON = JSON.withObject "LogParams" $ \o ->
    LogParams
      <$> o JSON..: "dir"
      <*> o JSON..: "module"
      <*> o JSON..: "identifier"
      <*> o JSON..: "payload"

-- Parameters for interactive method
data InteractiveParams = InteractiveParams
  { interactiveDir :: FilePath
  , file :: FilePath
  }

instance JSON.FromJSON InteractiveParams where
  parseJSON = JSON.withObject "InteractiveParams" $ \o ->
    InteractiveParams <$> o JSON..: "dir" <*> o JSON..: "file"

-- | Snap routes for dev endpoints (non-JSON-RPC)
routes :: Live.State -> [(BS.ByteString, Snap ())]
routes state =
  [ ("/dev/js", jsHandler state)
  , ("/dev/interactive", interactiveHandler state)
  , ("/dev/log", logHandler)
  , ("/dev/fileChanged", fileChangedHandler state)
  ]

-- Helpers

-- Return the compileResult JSON for a project rooted by dir
getProjectStatus :: Live.State -> FilePath -> IO (Either String JSON.Value)
getProjectStatus liveState root = do
  existing <- Client.getExistingProject root liveState
  case existing of
    Left Client.NoProjectsRegistered ->
      pure $ Left "No projects registered"
    Left (Client.ProjectNotFound _) ->
      pure $ Left "Project not found"
    Right (Client.ProjectCache _ _ _ mCompileResult _, _) -> do
      result <- STM.readTVarIO mCompileResult
      pure (Right (Client.encodeCompilationResult result))

-- Snap handlers

jsHandler :: Live.State -> Snap ()
jsHandler state = do
  mDir <- getParam "dir"
  mFile <- getParam "file"
  mDebug <- getParam "debug"
  mOptimize <- getParam "optimize"
  mReport <- getParam "report"
  case (mDir, mFile) of
    (Nothing, _) -> problemSnap 400 "bad_request" (Text.pack "Missing query param: dir")
    (_, Nothing) -> problemSnap 400 "bad_request" (Text.pack "Missing query param: file")
    (Just dirBs, Just fileBs) -> do
      let dir = Text.unpack (Data.Text.Encoding.decodeUtf8 dirBs)
      let file = Text.unpack (Data.Text.Encoding.decodeUtf8 fileBs)
      let debug = parseBoolParamWithDefault False mDebug
      let optimize = parseBoolParamWithDefault False mOptimize
      let report = parseReportParam mReport
      e <- liftIO (compile state dir file debug optimize report)
      case e of
        Left errOut -> do
          modifyResponse $ setResponseCode 422
          case errOut of
            ErrorJson errJson -> do
              modifyResponse $ setContentType "application/json"
              writeLBS (JSON.encode errJson)
            ErrorTerminal msg -> do
              modifyResponse $ setContentType "text/plain; charset=utf-8"
              writeBS (Data.Text.Encoding.encodeUtf8 msg)
        Right jsBuilder -> do
          modifyResponse $ setContentType "application/javascript; charset=utf-8"
          writeBuilder jsBuilder

interactiveHandler :: Live.State -> Snap ()
interactiveHandler state = do
  mDir <- getParam "dir"
  mFile <- getParam "file"
  case (mDir, mFile) of
    (Nothing, _) -> problemSnap 400 "bad_request" (Text.pack "Missing query param: dir")
    (_, Nothing) -> problemSnap 400 "bad_request" (Text.pack "Missing query param: file")
    (Just dirBs, Just fileBs) -> do
      let dir = Text.unpack (Data.Text.Encoding.decodeUtf8 dirBs)
      let file = Text.unpack (Data.Text.Encoding.decodeUtf8 fileBs)
      value <- liftIO (interactiveDocs state dir file)
      modifyResponse $ setContentType "application/json"
      writeLBS (JSON.encode value)

logHandler :: Snap ()
logHandler = do
  body <- readRequestBody 1048576 -- 1MB
  case decodeAsValue body of
    Left _ -> problemSnap 400 "bad_request" (Text.pack "Invalid JSON body")
    Right _ -> do
      modifyResponse $ setContentType "application/json"
      writeLBS (JSON.encode (JSON.object ["ok" JSON..= True]))

-- Notify server that an absolute file path changed
fileChangedHandler :: Live.State -> Snap ()
fileChangedHandler state = do
  mPath <- getParam "path"
  case mPath of
    Nothing -> problemSnap 400 "bad_request" (Text.pack "Missing query param: path")
    Just pathBs -> do
      let path = Text.unpack (Data.Text.Encoding.decodeUtf8 pathBs)
      ok <- liftIO (File.exists path)
      if not ok
        then problemSnap 404 "not_found" (Text.pack "File not found")
        else do
          -- 1) Read from FS and write to cache
          contents <- liftIO (File.readUtf8 path)
          _ <- liftIO (Ext.FileCache.writeUtf8 path contents)

          -- 2) Compile relevant projects
          _ <- liftIO (Watchtower.State.Compile.compileRelevantProjects state [path])

          -- 3) Broadcast new state via DevWS
          liftIO $ do
            existing <- Client.getExistingProject path state
            case existing of
              Left _ -> pure ()
              Right (firstProj@(Client.ProjectCache _ _ _ mCompileResult _), rest) -> do
                let allProjs = firstProj : rest
                mapM_ (broadcastForProject mCompileResultAccessor) allProjs
          modifyResponse $ setContentType "application/json"
          writeLBS (JSON.encode (JSON.object ["ok" JSON..= True]))
  where
    mCompileResultAccessor (Client.ProjectCache _ _ _ m _ ) = m
    broadcastForProject getM (proj@(Client.ProjectCache _ _ _ _ _)) = do
      let m = getM proj
      result <- STM.readTVarIO m
      case result of
        Client.Success compileRes ->
          case compileRes of
            CompileHelpers.CompiledJs jsBuilder ->
              DevWS.broadcastCompiled state (Client.builderToString jsBuilder)
            CompileHelpers.CompiledHtml _ ->
              pure ()
            CompileHelpers.CompiledSkippedOutput ->
              pure ()
        Client.Error err -> do
          let errJson = Client.encodeCompilationResult (Client.Error err)
          DevWS.broadcastCompilationError state errJson
        Client.NotCompiled -> pure ()

-- Return raw JS as a Builder for efficient streaming to Snap
data Report = ReportJson | ReportTerminal

data ErrorOutput = ErrorJson JSON.Value | ErrorTerminal Text.Text

compile :: Live.State -> FilePath -> FilePath -> Bool -> Bool -> Report -> IO (Either ErrorOutput Builder.Builder)
compile state root file debug optimize report = do
  let wsUrl = Client.urlsDevWebsocket (Client.urls state)
      desired = CompileHelpers.getMode debug optimize
      flags = CompileHelpers.Flags
                desired
                (CompileHelpers.OutputTo CompileHelpers.Js)
      hotJs = Modify.Inject.Loader.hotJs wsUrl

  -- Find an existing project whose elm.json root equals the given root; otherwise create it
  existingProjects <- STM.readTVarIO (Client.projects state)
  projCache <- Watchtower.State.Project.upsert state flags root (NE.List file [])
  compiledR <- Watchtower.State.Compile.compile state projCache [file]
  Ext.Log.log Ext.Log.Live ("Recompiled")
  case compiledR of
    Left clientErr -> do
      Ext.Log.log Ext.Log.Live ("Compilation error")
      let errJson = Client.encodeCompilationResult (Client.Error clientErr)
      DevWS.broadcastCompilationError state errJson
      case report of
        ReportJson -> pure (Left (ErrorJson errJson))
        ReportTerminal -> do
          let msg = case clientErr of
                      Client.ReactorError exit -> Text.pack (Reporting.Exit.toAnsiString (Reporting.Exit.reactorToReport exit))
                      Client.GenerationError err -> Text.pack err
          pure (Left (ErrorTerminal msg))
    Right result ->
      case result of
        CompileHelpers.CompiledJs jsBuilder -> do
          Ext.Log.log Ext.Log.Live ("Attempting to broadcast compiled JS")
          DevWS.broadcastCompiled state (Client.builderToString jsBuilder)
          pure (Right (hotJs <> jsBuilder))

        CompileHelpers.CompiledHtml _ -> do
          Ext.Log.log Ext.Log.Live ("HTML output not supported on /dev/js")
          pure (Left (ErrorTerminal (Text.pack "HTML output not supported on /dev/js")))
            
        CompileHelpers.CompiledSkippedOutput -> do
          Ext.Log.log Ext.Log.Live ("Compilation skipped")
          pure (Left (ErrorTerminal (Text.pack "Compilation skipped")))
          

-- Perform the same logic as Questions.Interactive branch
interactiveDocs :: Live.State -> FilePath -> FilePath -> IO JSON.Value
interactiveDocs liveState cwd filepath = do
  maybeDocs <- Ext.Dev.docs cwd filepath
  case maybeDocs of
    Nothing -> pure (JSON.toJSON (Text.pack "Docs are not available"))
    Just docs -> do
      let docsJson = Docs.encode (Docs.toDict [docs])
      let docsProject = Json.Encode.object [("project", docsJson), ("viewers", Json.Encode.array [])]
      let flags = Json.Encode.object [("flags", docsProject)]
      let input = LBS.toStrict (Builder.toLazyByteString (Json.Encode.encodeUgly flags))
      result <- Gen.Javascript.run Gen.Javascript.interactiveJs input
      case result of
        Left err -> pure (JSON.toJSON err)
        Right output -> do
          case JSON.eitherDecodeStrict (Data.Text.Encoding.encodeUtf8 (Text.pack output)) of
            Left err -> pure (JSON.toJSON err)
            Right (Gen.Generate.GeneratedFiles _generatedFiles) -> do
              -- Return the docs JSON as Aeson.Value to mirror Questions.Interactive behavior
              let lbs = Builder.toLazyByteString (Json.Encode.encodeUgly docsJson)
              case JSON.eitherDecode lbs of
                Left e -> pure (JSON.String (Text.pack e))
                Right v -> pure v

-- Utilities

parseBoolParamWithDefault :: Bool -> Maybe BS.ByteString -> Bool
parseBoolParamWithDefault def m =
  case fmap (Text.toLower . Data.Text.Encoding.decodeUtf8) m of
    Just "1" -> True
    Just "true" -> True
    Just "yes" -> True
    Just "0" -> False
    Just "false" -> False
    Just "no" -> False
    _ -> def

parseReportParam :: Maybe BS.ByteString -> Report
parseReportParam m =
  case fmap (Text.toLower . Data.Text.Encoding.decodeUtf8) m of
    Just "json" -> ReportJson
    _ -> ReportTerminal

problemSnap :: Int -> BS.ByteString -> Text.Text -> Snap ()
problemSnap code problemCode detail = do
  modifyResponse $ setResponseCode code
  modifyResponse $ setContentType "application/json"
  writeLBS (JSON.encode (JSON.object
    [ "type" JSON..= (Text.pack "about:blank")
    , "status" JSON..= code
    , "code" JSON..= (Data.Text.Encoding.decodeUtf8 problemCode)
    , "detail" JSON..= detail
    ]))

-- Explicitly-typed JSON decode helper to avoid inline annotations at call-site
decodeAsValue :: LBS.ByteString -> Either String JSON.Value
decodeAsValue = JSON.eitherDecode


