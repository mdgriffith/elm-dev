{-# LANGUAGE OverloadedStrings #-}

-- Ported from https://github.com/supermario/hilt/blob/master/src/Hilt/SocketServer.hs
-- Modified to remove managed and have clientId injection rather than auto-gen

module Watchtower.Websocket (clientsInit, broadcastWith, Client, socketHandler, runWebSocketsSnap) where

import Control.Concurrent.STM
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Control.Monad.Trans (liftIO)
import Data.List (find)
import qualified Data.List as List
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Ext.Common
import Llamadera
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Snap as WS
import Snap.Core (MonadSnap)

runWebSocketsSnap :: MonadSnap m => WS.ServerApp -> m ()
runWebSocketsSnap = WS.runWebSocketsSnap

clientsInit :: IO (TVar [Client])
clientsInit = newTVarIO []

leaderInit :: IO (TVar (Maybe ClientId))
leaderInit = newTVarIO Nothing

socketHandler :: TVar [Client] -> OnJoined -> OnReceive -> T.Text -> WS.ServerApp
socketHandler mClients onJoined onReceive clientId pending = do
  debug $ "[websocket] ❇️  " <> T.unpack clientId
  conn <- WS.acceptRequest pending

  let client = (clientId, conn)
      disconnect = do
        atomically $ do
          clients <- readTVar mClients
          let remainingClients = removeClient client clients
          writeTVar mClients remainingClients
        debug $ "[websocket] 🚫 " <> T.unpack clientId

  flip finally disconnect $ do
    clientCount <- atomically $ do
      clients <- readTVar mClients
      writeTVar mClients $ addClient client clients
      pure $ length clients

    initText <- onJoined clientId clientCount
    case initText of
      Just text -> sendImpl mClients clientId text
      Nothing -> do
        -- @TODO Should really be a NOTICE level log via a logger
        -- debug "[websocket:notice] No init message for new client was provided"
        pure ()

    talk onReceive conn mClients client

data Handle = Handle
  { send :: Int -> T.Text -> IO (),
    broadcast :: T.Text -> IO (),
    app :: WS.ServerApp
  }

-- OnJoined = clientId -> totalClients -> IO (Maybe (response message))
type OnJoined = ClientId -> Int -> IO (Maybe T.Text)

-- OnReceive = clientId -> receivedMessage -> IO ()
type OnReceive = ClientId -> T.Text -> IO ()

type ClientId = T.Text

type Client = (ClientId, WS.Connection)

sendImpl :: TVar [Client] -> ClientId -> T.Text -> IO ()
sendImpl mClients clientId message = do
  clients <- atomically $ readTVar mClients
  send_ clients clientId message

broadcastWith :: TVar [client] -> (client -> Maybe Client) -> T.Text -> IO ()
broadcastWith mClients toClient message = do
  clients <- atomically $ readTVar mClients
  broadcast_ (filterMap toClient clients) message

filterMap :: (a -> Maybe b) -> [a] -> [b]
filterMap toMaybe list =
  List.foldl'
    ( \gathered a ->
        case toMaybe a of
          Nothing ->
            gathered
          Just item ->
            item : gathered
    )
    []
    list
    & List.reverse

broadcastImpl :: TVar [Client] -> T.Text -> IO ()
broadcastImpl mClients message = do
  clients <- atomically $ readTVar mClients
  broadcast_ clients message

talk :: OnReceive -> WS.Connection -> TVar [Client] -> Client -> IO ()
talk onReceive conn _ (clientId, _) = forever $ do
  msg <- WS.receiveData conn
  debug $ T.unpack $ "[websocket] ▶️  " <> T.pack (show clientId) <> ":" <> T.take 130 msg
  onReceive clientId msg

addClient :: Client -> [Client] -> [Client]
addClient client clients = client : clients

removeClient :: Client -> [Client] -> [Client]
removeClient client = filter ((/= fst client) . fst)

removeClientId :: ClientId -> [Client] -> [Client]
removeClientId clientId = filter ((/= clientId) . fst)

findClient :: [Client] -> ClientId -> Maybe Client
findClient clients clientId = find ((== clientId) . fst) clients

send_ :: [Client] -> ClientId -> T.Text -> IO ()
send_ clients clientId text =
  case findClient clients clientId of
    Just (_, conn) -> WS.sendTextData conn text
    Nothing -> pure ()

broadcast_ :: [Client] -> T.Text -> IO ()
broadcast_ clients message = do
  debug (T.unpack ("[websocket] ◀️  " <> T.pack (show $ fmap fst clients) <> ":" <> T.take 130 message))
  forM_ clients $ \(_, conn) -> WS.sendTextData conn message
