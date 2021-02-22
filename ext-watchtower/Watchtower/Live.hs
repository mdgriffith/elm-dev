{-# LANGUAGE OverloadedStrings #-}

module Watchtower.Live where

import Control.Concurrent.STM
import Control.Applicative ((<|>), (<$>), (<*>))
import System.IO (hFlush, hPutStr, hPutStrLn, stderr, stdout)
import Snap.Core hiding (path)
import Snap.Http.Server
import Snap.Util.FileServe
import Control.Monad.Trans (liftIO)
import Control.Monad (guard)
import Control.Concurrent.STM

import qualified Data.ByteString.Builder
import qualified Data.ByteString.Lazy
import qualified Data.Text as T
import qualified Data.Text.Encoding as T


import qualified Network.WebSockets      as WS
import qualified Network.WebSockets.Snap as WS

import qualified Develop.Generate.Help
import qualified Json.Encode
import qualified Json.String
import Json.Encode ((==>))
import qualified Json.Decode
import qualified Reporting.Annotation as Ann
import qualified Watchtower.StaticAssets
import qualified Watchtower.Details
import qualified Ext.Sentry

import qualified Watchtower.Websocket
import qualified Watchtower.Compile

import Ext.Common



data State =
        State
            { cache :: Ext.Sentry.Cache
            , clients :: (TVar [Client])
            }

type ClientId = T.Text

type Client = (ClientId, WS.Connection)


init :: IO State
init =
    State
      <$> Ext.Sentry.init
      <*> Watchtower.Websocket.clientsInit


recompile :: Watchtower.Live.State -> [String] -> IO ()
recompile (Watchtower.Live.State sentryCache mClients) filenames = do
  debug $ "🛫  recompile starting: " ++ show filenames
  trackedForkIO $ do
    Ext.Sentry.updateCompileResult sentryCache $ do

      -- @TODO what do we do with multiple filenames?
      eitherStatusJson <- Watchtower.Compile.compileToJson (head filenames)
      Watchtower.Websocket.broadcastImpl mClients $ eitherStatusToText eitherStatusJson
      pure eitherStatusJson


  debug "🛬  recompile done... "


websocket :: State -> Snap ()
websocket state =
  route
      [ ("/ws", websocket_ state)
      ]


websocket_ :: State -> Snap ()
websocket_ (state@(State cache mClients)) = do
  mKey <- getHeader "sec-websocket-key" <$> getRequest
  case mKey of
    Just key -> do
      let
        onJoined clientId totalClients = do
          debug (T.unpack ("Total clients " <> T.pack (show totalClients)))
          eitherStatusJson <- Ext.Sentry.getCompileResult cache
          pure $ Just $ eitherStatusToText eitherStatusJson

      Watchtower.Websocket.runWebSocketsSnap
        $ Watchtower.Websocket.socketHandler
            mClients onJoined (receive state) (T.decodeUtf8 key)

    Nothing ->
      error404

builderToString =
  T.decodeUtf8 . Data.ByteString.Lazy.toStrict . Data.ByteString.Builder.toLazyByteString



receive state clientId text = do
  debug $ (T.unpack "RECVD" <> T.unpack text)
  case Json.Decode.fromByteString decodeIncoming (T.encodeUtf8 text) of
    Left err -> do
      debug $ (T.unpack "Error decoding!" <> T.unpack text)
      --
      pure ()

    Right action -> do
      debug $ (T.unpack "Action!" <> T.unpack text)
      receiveAction state clientId action


receiveAction state@(State cache mClients) clientId incoming =
  case incoming of
    Visible visible -> do
       debug $ "forwarding visibility"
       Watchtower.Websocket.broadcastImpl
          mClients
          (builderToString (encodeOutgoing (FwdVisible visible)))

    JumpTo location -> do
      debug $ "forwarding jump"
      Watchtower.Websocket.broadcastImpl
          mClients
          (builderToString (encodeOutgoing (FwdJumpTo location)))

    ElmStatusPlease file -> do
      -- eitherStatusJson <- Watchtower.Compile.compileToJson (T.unpack file)
      -- @TODO do we need an explicit req/resp if we're always going to push instead...?
      eitherStatusJson <- Ext.Sentry.getCompileResult cache
      Watchtower.Websocket.broadcastImpl mClients $ eitherStatusToText eitherStatusJson


eitherStatusToText :: Either Json.Encode.Value Json.Encode.Value -> T.Text
eitherStatusToText eitherStatusJson =
  case eitherStatusJson of
    Left errors -> do
      builderToString (encodeOutgoing (ElmStatus errors))

    Right jsoutput -> do
      builderToString (encodeOutgoing (ElmStatus jsoutput))


decodeIncoming :: Json.Decode.Decoder T.Text Incoming
decodeIncoming =
  Json.Decode.field "msg" Json.Decode.string
    >>= (\msg ->
            case msg of
              "StatusPlease" ->
                Json.Decode.field "details"
                  (ElmStatusPlease
                    <$> Json.Decode.field "file" ((T.pack . Json.String.toChars) <$> Json.Decode.string)
                  )

              "Visible" ->
                  Visible <$> (Json.Decode.field "details" Watchtower.Details.decodeVisible)

              "Jump" ->
                  JumpTo <$> (Json.Decode.field "details" Watchtower.Details.decodeLocation)

              _ ->
                  Json.Decode.failure "Unknown msg"

        )



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
    = Visible Watchtower.Details.Visible
    | JumpTo Watchtower.Details.Location
    | ElmStatusPlease T.Text


data Outgoing
    -- forwarding information
    = FwdVisible Watchtower.Details.Visible
    | FwdJumpTo Watchtower.Details.Location

    -- new information is available
    | ElmStatus Json.Encode.Value


encodeOutgoing :: Outgoing -> Data.ByteString.Builder.Builder
encodeOutgoing out =
  Json.Encode.encodeUgly $
    case out of
      FwdVisible visible ->
        Json.Encode.object
            [ "msg" ==> Json.Encode.string (Json.String.fromChars "Visible")
            , "details" ==> Watchtower.Details.encodeVisible visible
            ]

      FwdJumpTo loc ->
        Json.Encode.object
            [ "msg" ==> Json.Encode.string (Json.String.fromChars "Jump")
            , "details" ==> Watchtower.Details.encodeLocation loc
            ]

      ElmStatus js ->
        Json.Encode.object
            [ "msg" ==> Json.Encode.string (Json.String.fromChars "Status")
            , "details" ==> js
            ]
