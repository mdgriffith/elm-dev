{-# LANGUAGE OverloadedStrings #-}

module Watchtower.Live where

import Control.Applicative ((<|>))
import System.IO (hFlush, hPutStr, hPutStrLn, stderr, stdout)
import Snap.Core hiding (path)
import Snap.Http.Server
import Snap.Util.FileServe


import qualified Develop.Generate.Help
import qualified Json.Encode
import qualified Reporting.Annotation as Ann
import qualified Watchtower.StaticAssets
import qualified Watchtower.Details

import qualified Network.WebSockets
import qualified Network.WebSockets.Snap



data State = State
        -- (TVar [Client], TVar (Maybe ClientId), BroadcastChan In Text, TVar Text)


init :: IO State
init =
    pure State


websocket :: State -> Snap ()
websocket _ =
--   do  file <- getSafePath
--       guard (file == "_w")
--       mKey <- getHeader "sec-websocket-key" <$> getRequest
--       mSid <- getCookie "sid"

--       randBytes <- liftIO $ getEntropy 20
--       let newSid = BSL.toStrict $ B.toLazyByteString $ B.byteStringHex randBytes

--       sessionId <-
--         case mSid of
--           Nothing -> do
--             let cookie = Cookie "sid" newSid Nothing Nothing Nothing False False
--             modifyResponse $ addResponseCookie cookie

--             pure $ T.decodeUtf8 $ newSid

--           Just sid_ ->
--             pure $ T.decodeUtf8 $ cookieValue sid_

--       case mKey of
--         Just key -> do
--           let onJoined clientId totalClients = do
--                 leaderChanged <- atomically $ do
--                   leader <- readTVar mLeader
--                   case leader of
--                     Just leaderId ->
--                       -- No change
--                       pure False

--                     Nothing -> do
--                       -- If there's no leader, become the leader
--                       writeTVar mLeader (Just clientId)
--                       pure True

--                 onlyWhen leaderChanged $ do
--                   sendToLeader mClients mLeader (\leader -> do
--                       -- Tell the new leader about the backend state they need
--                       atomically $ readTVar beState
--                     )
--                   -- Tell everyone about the new leader (also causes actual leader to go active as leader)
--                   broadcastLeader mClients mLeader

--                 SocketServer.broadcastImpl mClients $ "{\"t\":\"c\",\"s\":\"" <> sessionId <> "\",\"c\":\""<> clientId <> "\"}"

--                 leader <- atomically $ readTVar mLeader
--                 case leader of
--                   Just leaderId ->
--                     pure $ Just $ "{\"t\":\"s\",\"c\":\"" <> clientId <> "\",\"l\":\"" <> leaderId <> "\"}"

--                   Nothing ->
--                     -- Impossible
--                     pure Nothing

--               onReceive clientId text = do
--                 if Text.isPrefixOf "{\"t\":\"envMode\"," text
--                   then do
--                     root <- liftIO $ getProjectRoot
--                     -- This is a bit dodge, but avoids needing to pull in all of Aeson
--                     setEnvMode root $ (Text.splitOn "\"" text) !! 7

--                     -- Touch the src/Env.elm file to make sure it gets recompiled
--                     touch $ root </> "src" </> "Env.elm"

--                     -- Mode has changed, force a refresh
--                     -- Actually not needed, because the touch will do this for us!
--                     -- SocketServer.broadcastImpl mClients "{\"t\":\"r\"}"

--                   else if Text.isSuffixOf "\"t\":\"p\"}" text
--                     then do
--                       debug "[backendSt] 💾"
--                       atomically $ writeTVar beState text
--                       onlyWhen (textContains "force" text) $ do
--                         debug "[refresh  ] 🔄 "
--                         -- Force due to backend reset, force a refresh
--                         SocketServer.broadcastImpl mClients "{\"t\":\"r\"}"

--                     else if Text.isPrefixOf "{\"t\":\"ToBackend\"," text

--                       then do
--                         sendToLeader mClients mLeader (\l -> pure text)


--                     else if Text.isPrefixOf "{\"t\":\"qr\"," text

--                       then do

--                         debugT $ "🍕  rpc response:" <> text
--                         -- Query response, send it to the chan for pickup by awaiting HTTP endpoint
--                         liftIO $ writeBChan mChan text
--                         pure ()

--                     else
--                       SocketServer.broadcastImpl mClients text

--           Network.WebSockets.Snap.runWebSocketsSnap $ SocketServer.socketHandler mClients mLeader beState onJoined onReceive (T.decodeUtf8 key) sessionId


--         Nothing ->
          error404


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
    | ElmStatus Watchtower.Details.Status



