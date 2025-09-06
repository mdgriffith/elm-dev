{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Watchtower.Server.Dev (serve) where

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
import qualified Ext.CompileProxy as CompileProxy
import qualified Ext.CompileHelpers.Generic as CompileHelpers
import qualified Data.NonEmptyList as NE

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

-- | Main JSON-RPC server for Dev methods
serve :: Live.State -> JSONRPC.EventEmitter -> JSONRPC.Request -> IO (Either JSONRPC.Error JSONRPC.Response)
serve state _emit (JSONRPC.Request _ reqId method params) = do
  case Text.unpack method of
    "js" -> do
      case params of
        Just p ->
          case JSON.fromJSON p of
            JSON.Success (JsParams root _file _mDebug _mOptimize) -> do
              -- For dev server, do not compile. Read compile status from Live.State instead.
              result <- getProjectStatus state root
              case result of
                Left errStr -> pure (Left (JSONRPC.err reqId (Text.pack errStr)))
                Right json -> pure (Right (JSONRPC.success reqId json))
            JSON.Error e -> pure (Left (JSONRPC.err reqId (Text.pack ("Invalid params for js: " <> e))))
        Nothing -> pure (Left (JSONRPC.err reqId (Text.pack "Missing params for js")))
    "log" -> do
      case params of
        Just p ->
          case JSON.fromJSON p of
            JSON.Success (LogParams _root _mod _ident _payload) -> do
              -- Placeholder: accept and acknowledge
              pure (Right (JSONRPC.success reqId (JSON.object ["ok" .= True])))
            JSON.Error e -> pure (Left (JSONRPC.err reqId (Text.pack ("Invalid params for log: " <> e))))
        Nothing -> pure (Left (JSONRPC.err reqId (Text.pack "Missing params for log")))
    "interactive" -> do
      case params of
        Just p ->
          case JSON.fromJSON p of
            JSON.Success (InteractiveParams root path) -> do
              value <- interactiveDocs state root path
              pure (Right (JSONRPC.success reqId value))
            JSON.Error e -> pure (Left (JSONRPC.err reqId (Text.pack ("Invalid params for interactive: " <> e))))
        Nothing -> pure (Left (JSONRPC.err reqId (Text.pack "Missing params for interactive")))
    _ -> pure (Left (JSONRPC.errorMethodNotFound reqId method))

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

-- Compile a specific Elm file to JS and return JSON with code
compileJs :: FilePath -> FilePath -> Bool -> Bool -> IO (Either String JSON.Value)
compileJs root file debug optimize = do
  let desired = CompileHelpers.getMode debug optimize
  let flags = CompileHelpers.Flags desired (CompileHelpers.OutputTo CompileHelpers.Js) Nothing
  compiledR <- CompileProxy.compile root (NE.List file []) flags
  case compiledR of
    Left reactorErr -> do
      -- Serialize reactor error as string for now
      pure (Left (show reactorErr))
    Right (result, _fileInfo) ->
      case result of
        CompileHelpers.CompiledJs builder -> do
          let jsBytes = BL.toStrict (BB.toLazyByteString builder)
          let jsText = Data.Text.Encoding.decodeUtf8 jsBytes
          pure (Right (JSON.object [ "compiled" JSON..= True, "code" JSON..= jsText ]))
        CompileHelpers.CompiledHtml _ ->
          pure (Right (JSON.object [ "compiled" JSON..= True, "code" JSON..= (Text.pack "") ]))
        CompileHelpers.CompiledSkippedOutput ->
          pure (Right (JSON.object [ "compiled" JSON..= False, "code" JSON..= (Text.pack "") ]))

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


