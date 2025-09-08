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
import qualified Watchtower.Server.JSONRPC as JSONRPC
import qualified Ext.CompileHelpers.Generic as CompileHelpers
import qualified Data.NonEmptyList as NE
import qualified Data.List as List
import qualified Watchtower.State.Project
import qualified Watchtower.State.Compile
import Snap.Core
import Control.Monad.Trans (liftIO)

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
    Right (Client.ProjectCache _ _ mCompileResult _, _) -> do
      result <- STM.readTVarIO mCompileResult
      pure (Right (Client.encodeCompilationResult result))

-- Snap handlers

jsHandler :: Live.State -> Snap ()
jsHandler state = do
  mDir <- getParam "dir"
  mFile <- getParam "file"
  mDebug <- getParam "debug"
  mOptimize <- getParam "optimize"
  case (mDir, mFile) of
    (Nothing, _) -> problemSnap 400 "bad_request" (Text.pack "Missing query param: dir")
    (_, Nothing) -> problemSnap 400 "bad_request" (Text.pack "Missing query param: file")
    (Just dirBs, Just fileBs) -> do
      let dir = Text.unpack (Data.Text.Encoding.decodeUtf8 dirBs)
      let file = Text.unpack (Data.Text.Encoding.decodeUtf8 fileBs)
      let debug = parseBoolParamWithDefault False mDebug
      let optimize = parseBoolParamWithDefault False mOptimize
      e <- liftIO (compile state dir file debug optimize)
      case e of
        Left err -> problemSnap 422 "compile_failed" (Text.pack err)
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

-- Return raw JS as a Builder for efficient streaming to Snap
compile :: Live.State -> FilePath -> FilePath -> Bool -> Bool -> IO (Either String Builder.Builder)
compile state root file debug optimize = do
  let wsUrl = Client.urlsDevWebsocket (Client.urls state)
      desired = CompileHelpers.getMode debug optimize
      flags = CompileHelpers.Flags
                desired
                (CompileHelpers.OutputTo CompileHelpers.Js)
                (Just (CompileHelpers.ElmDevWsUrl (Builder.string8 wsUrl)))

  -- Find an existing project whose elm.json root equals the given root; otherwise create it
  existingProjects <- STM.readTVarIO (Client.projects state)
  projCache <- case List.find (\pc -> Client.getProjectRoot pc == root) existingProjects of
    Just pc -> pure pc
    Nothing -> Watchtower.State.Project.upsert state flags root (NE.List file [])

  compiledR <- Watchtower.State.Compile.compile state flags projCache [file]
  case compiledR of
    Left clientErr ->
      case clientErr of
        Client.ReactorError exit -> pure (Left (show exit))
        Client.GenerationError err -> pure (Left err)
    Right result ->
      case result of
        CompileHelpers.CompiledJs jsBuilder ->
          pure (Right jsBuilder)
        CompileHelpers.CompiledHtml _ ->
          pure (Left "HTML output not supported on /dev/js")
        CompileHelpers.CompiledSkippedOutput ->
          pure (Left "Compilation skipped")

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


