{-# LANGUAGE OverloadedStrings #-}

module Watchtower.Server.DevWS
  ( State
  , Watchtower.Server.DevWS.init
  , websocket
  , broadcastCompiled
  , broadcastCompilationError
  ) where

import qualified Control.Concurrent.STM as STM
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Develop.Generate.Help
import Snap.Core hiding (path)
import Snap.Http.Server
import qualified Watchtower.Live.Client as Client
import qualified Watchtower.Websocket
import qualified Ext.Log

-- Keep the same State type as Live so other modules (e.g. Dev routes) continue to work
type State = Client.State

init :: Client.Urls -> IO Client.State
init urls =
  Client.State
    <$> Watchtower.Websocket.clientsInit
    <*> STM.newTVarIO []
    <*> STM.newTVarIO mempty
    <*> pure urls

websocket :: Client.State -> Snap ()
websocket state =
  route
    [ ("/ws", websocket_ state)
    ]

websocket_ :: Client.State -> Snap ()
websocket_ state@(Client.State mClients _ _ _) = do
  mKey <- getHeader "sec-websocket-key" <$> getRequest
  case mKey of
    Just key -> do
      let onJoined _clientId _totalClients = pure Nothing
      Watchtower.Websocket.runWebSocketsSnap $
        Watchtower.Websocket.socketHandler
          mClients
          onJoined
          (receive state)
          (T.decodeUtf8 key)
          Client.emptyWatch
    Nothing ->
      error404

error404 :: Snap ()
error404 = do
  modifyResponse $ setResponseStatus 404 "Not Found"
  modifyResponse $ setContentType "text/html; charset=utf-8"
  writeBuilder $ Develop.Generate.Help.makePageHtml "NotFound" Nothing

receive :: Client.State -> Client.ClientId -> T.Text -> IO ()
receive _state _clientId text = do
  case JSON.eitherDecode (LBS.fromStrict (T.encodeUtf8 text)) :: Either String JSON.Value of
    Left _ -> do
      Ext.Log.log Ext.Log.Live ("[DevWS] error decoding incoming: " <> T.unpack text)
      pure ()
    Right v -> handleIncoming v

handleIncoming :: JSON.Value -> IO ()
handleIncoming (JSON.Object obj) =
  case KeyMap.lookup (Key.fromText (T.pack "msg")) obj of
    Just (JSON.String t) ->
      case t of
        "Log" -> Ext.Log.log Ext.Log.Live "[DevWS] Log received" >> pure ()
        "Lazy" -> Ext.Log.log Ext.Log.Live "[DevWS] Lazy info received" >> pure ()
        _ -> pure ()
    _ -> pure ()
handleIncoming _ = pure ()

-- Outgoing helpers

broadcastCompiled :: Client.State -> T.Text -> IO ()
broadcastCompiled (Client.State mClients _ _ _) codeText = do
  let payload = JSON.object
        [ "msg" JSON..= JSON.String (T.pack "Compiled")
        , "details" JSON..= JSON.String codeText
        ]
  Watchtower.Websocket.broadcastWith mClients (\_ -> True) (aesonToText payload)

broadcastCompilationError :: Client.State -> JSON.Value -> IO ()
broadcastCompilationError (Client.State mClients _ _ _) errVal = do
  let payload = JSON.object
        [ "msg" JSON..= JSON.String (T.pack "CompilationError")
        , "details" JSON..= errVal
        ]
  Watchtower.Websocket.broadcastWith mClients (\_ -> True) (aesonToText payload)

aesonToText :: JSON.Value -> T.Text
aesonToText = T.decodeUtf8 . LBS.toStrict . JSON.encode

-- 