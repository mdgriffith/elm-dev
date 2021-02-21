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



websocket :: State -> Snap ()
websocket state =
  route
      [ ("/ws", websocket_ state)
      ]


websocket_ :: State -> Snap ()
websocket_ (State cache mClients) = do
  mKey <- getHeader "sec-websocket-key" <$> getRequest
  case mKey of
    Just key -> do
      let
        onJoined clientId totalClients = do
          debug (T.unpack ("Total clients " <> T.pack (show totalClients)))
          pure Nothing

        -- onReceive clientId text = do
          
        --   let (root, path) = T.span (\c -> c /= ':') text

        --   debug $ "Received " <> (T.unpack root) <> "  @ " <> (T.unpack (T.drop 1 path))
          
        --   eitherStatusJson <- Watchtower.Compile.compileToJson ((T.unpack root) <> "/") (T.unpack (T.drop 1 path))
        --   case eitherStatusJson of
        --     Left errors -> do
        --       debug $ "got errors"-- <> (T.unpack $ T.decodeUtf8 errors)
        --       Watchtower.Websocket.broadcastImpl mClients (builderToString (encodeOutgoing (ElmStatus errors)))

        --     Right jsoutput -> do
        --       debug $ "success!"
        --       -- Watchtower.Websocket.broadcastImpl mClients ("{\"status\": \"ok\"}")
        --       Watchtower.Websocket.broadcastImpl mClients (builderToString (encodeOutgoing (ElmStatus jsoutput)))

      Watchtower.Websocket.runWebSocketsSnap 
          $ Watchtower.Websocket.socketHandler
                  mClients onJoined (receive mClients) (T.decodeUtf8 key)

    Nothing ->
      error404

builderToString =
  T.decodeUtf8 . Data.ByteString.Lazy.toStrict . Data.ByteString.Builder.toLazyByteString



receive mClients clientId text =
  case Json.Decode.fromByteString decodeIncoming (T.encodeUtf8 text) of
    Left err ->
      pure ()
    Right action ->
      receiveAction mClients clientId action




receiveAction mClients clientId incoming =
  case incoming of
    Visible visible ->
       Watchtower.Websocket.broadcastImpl 
          mClients 
          (builderToString (encodeOutgoing (FwdVisible visible)))
    
    JumpTo location ->
      Watchtower.Websocket.broadcastImpl 
          mClients 
          (builderToString (encodeOutgoing (FwdJumpTo location)))

    ElmStatusPlease root file -> do
        eitherStatusJson <- Watchtower.Compile.compileToJson (T.unpack root) (T.unpack file)
        case eitherStatusJson of
          Left errors -> do
            debug $ "got errors"-- <> (T.unpack $ T.decodeUtf8 errors)
            Watchtower.Websocket.broadcastImpl 
              mClients 
              (builderToString (encodeOutgoing (ElmStatus errors)))

          Right jsoutput -> do
            debug $ "success!"
            Watchtower.Websocket.broadcastImpl
              mClients
              (builderToString (encodeOutgoing (ElmStatus jsoutput)))




decodeIncoming :: Json.Decode.Decoder T.Text Incoming
decodeIncoming =
  Json.Decode.field "msg" Json.Decode.string
    >>= (\msg ->
            case msg of
              "StatusPlease" ->
                  ElmStatusPlease 
                    <$> Json.Decode.field "root" ((T.pack . Json.String.toChars) <$> Json.Decode.string)
                    <*> Json.Decode.field "file" ((T.pack . Json.String.toChars) <$> Json.Decode.string)

              "Visible" ->
                  Visible <$> Watchtower.Details.decodeVisible
              
              "Jump" ->
                  JumpTo <$> Watchtower.Details.decodeLocation

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
    | ElmStatusPlease T.Text T.Text


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
