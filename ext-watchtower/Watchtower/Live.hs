{-# LANGUAGE OverloadedStrings #-}

module Watchtower.Live where

import Control.Concurrent.STM
import Control.Applicative ((<|>))
import System.IO (hFlush, hPutStr, hPutStrLn, stderr, stdout)
import Snap.Core hiding (path)
import Snap.Http.Server
import Snap.Util.FileServe
import Control.Monad.Trans (liftIO)
import Control.Monad (guard)
import qualified Data.ByteString.Builder
import qualified Data.ByteString.Lazy

import qualified Data.Text as T
import qualified Network.WebSockets      as WS
import qualified Network.WebSockets.Snap as WS

import qualified Develop.Generate.Help
import qualified Json.Encode
import qualified Json.String
import Json.Encode ((==>))
import qualified Reporting.Annotation as Ann
import qualified Watchtower.StaticAssets
import qualified Watchtower.Details
import qualified Ext.Sentry

import qualified Watchtower.Websocket
import qualified Watchtower.Compile
import qualified Data.Text.Encoding as T
import Ext.Common

data State =
        State
            { cache :: Ext.Sentry.Cache
            , clients :: [ Client ]
            }

type ClientId = T.Text

type Client = (ClientId, WS.Connection)


init :: IO State
init =
    fmap
        (\cache ->
            State cache []
        ) Ext.Sentry.init



websocket :: State -> Snap ()
websocket state =
  route
      [ ("/ws", websocket_ state)
      ]


websocket_ :: State -> Snap ()
websocket_ state = do
  mKey <- getHeader "sec-websocket-key" <$> getRequest
  case mKey of
    Just key -> do
      mClients <- liftIO $ newTVarIO []

      let
        onJoined clientId totalClients = do
          debug $ "So and so joined"
          pure Nothing

        onReceive clientId text = do
          
          let (root, path) = T.span (\c -> c /= ':') text

          debug $ "Received " <> (T.unpack root) <> "  @ " <> (T.unpack (T.drop 1 path))
          
          eitherStatusJson <- Watchtower.Compile.compileToJson ((T.unpack root) <> "/") (T.unpack (T.drop 1 path))
          case eitherStatusJson of
            Left errors -> do
              debug $ "got errors"-- <> (T.unpack $ T.decodeUtf8 errors)
              Watchtower.Websocket.broadcastImpl mClients (builderToString (encodeOutgoing (ElmStatus errors)))

            Right jsoutput -> do
              debug $ "success!"
              -- Watchtower.Websocket.broadcastImpl mClients ("{\"status\": \"ok\"}")
              Watchtower.Websocket.broadcastImpl mClients (builderToString (encodeOutgoing (ElmStatus jsoutput)))

      Watchtower.Websocket.runWebSocketsSnap $ Watchtower.Websocket.socketHandler mClients onJoined onReceive (T.decodeUtf8 key)

    Nothing ->
      error404

builderToString =
  T.decodeUtf8 . Data.ByteString.Lazy.toStrict . Data.ByteString.Builder.toLazyByteString



error404 :: Snap ()
error404 =
  do  modifyResponse $ setResponseStatus 404 "Not Found"
      modifyResponse $ setContentType "text/html; charset=utf-8"
      writeBuilder $ Develop.Generate.Help.makePageHtml "NotFound" Nothing



{- Messages!

So, we have two different kinds of messages.

If there's a better way to square this, I'd love to hear it.


1. Messages being forwarded from one client to another.
    I.e. the editor saying a file is visible or focused.

2. An idea of a status subscription which you can ask for.


There are also one-off questions that can be asked via normal GET requests handeld directly by `Watchtower`.



-}
data Incoming
    -- forwarding information from a source to somewhere else
    = EditorsVisible [ Watchtower.Details.Location ]
    | EditorActive Watchtower.Details.Location (Maybe Watchtower.Details.Position)
    | JumpTo Watchtower.Details.Location

    -- status subscription
    -- remember who calls this and send them status events as they change.
    | ElmStatusSubscriptionPlease


data Outgoing
    -- forwarding information
    = FwdEditorsVisible [ Watchtower.Details.Location ]
    | FwdEditorActive Watchtower.Details.Location (Maybe Watchtower.Details.Position)
    | FwdJumpTo Watchtower.Details.Location

    -- new information is available
    | ElmStatus Json.Encode.Value


encodeOutgoing :: Outgoing -> Data.ByteString.Builder.Builder
encodeOutgoing out =
  Json.Encode.encodeUgly $
    case out of
      FwdEditorsVisible locs ->
        Json.Encode.null

      FwdEditorActive loc pos ->
        Json.Encode.null

      FwdJumpTo loc ->
        Json.Encode.null

      ElmStatus js ->
        Json.Encode.object
            [ "msg" ==> Json.Encode.string (Json.String.fromChars "Status")
            , "details" ==> js
            ]
