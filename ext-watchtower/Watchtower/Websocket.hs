{-# LANGUAGE OverloadedStrings #-}

-- Ported from https://github.com/supermario/hilt/blob/master/src/Hilt/SocketServer.hs
-- Modified to remove managed and have clientId injection rather than auto-gen

module Watchtower.Websocket
  ( clientsInit,
    broadcastWith,
    updateClientData,
    clientData,
    Client,
    socketHandler,
    runWebSocketsSnap,
    matchId,
  )
where

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

clientsInit :: IO (TVar [Client clientData])
clientsInit = newTVarIO []

leaderInit :: IO (TVar (Maybe ClientId))
leaderInit = newTVarIO Nothing

socketHandler :: TVar [Client clientData] -> OnJoined -> OnReceive -> T.Text -> clientData -> WS.ServerApp
socketHandler mClients onJoined onReceive clientId initialClientData pending = do
  debug $ "[websocket] ❇️  " <> T.unpack clientId
  conn <- WS.acceptRequest pending

  let client = Client clientId conn initialClientData
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

data Client clientData = Client
  { clientId :: ClientId,
    connection :: WS.Connection,
    state :: clientData
  }

matchId :: ClientId -> Client clientData -> Bool
matchId targetId (Client id _ _) =
  id == targetId

updateClientData :: ClientId -> (clientData -> clientData) -> Client clientData -> Client clientData
updateClientData clientId toNewState client@(Client cid conn state) =
  if clientId == cid
    then Client cid conn (toNewState state)
    else client

clientData :: Client clientData -> clientData
clientData (Client _ _ d) =
  d

sendImpl :: TVar [Client clientData] -> ClientId -> T.Text -> IO ()
sendImpl mClients clientId message = do
  clients <- atomically $ readTVar mClients
  send_ clients clientId message

broadcastWith :: TVar [Client clientData] -> (Client clientData -> Bool) -> T.Text -> IO ()
broadcastWith mClients filterTo message = do
  clients <- atomically $ readTVar mClients
  broadcast_ (List.filter filterTo clients) message

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

broadcastImpl :: TVar [Client clientData] -> T.Text -> IO ()
broadcastImpl mClients message = do
  clients <- atomically $ readTVar mClients
  broadcast_ clients message

talk :: OnReceive -> WS.Connection -> TVar [Client clientData] -> Client clientData -> IO ()
talk onReceive conn _ (Client clientId _ _) = forever $ do
  msg <- WS.receiveData conn
  -- debug $ T.unpack $ "[websocket] ▶️  " <> T.pack (show clientId) <> ":" <> T.take 130 msg
  onReceive clientId msg

addClient :: Client clientData -> [Client clientData] -> [Client clientData]
addClient client clients = client : clients

removeClient :: Client clientData -> [Client clientData] -> [Client clientData]
removeClient (Client clientId _ _) = filter (\(Client otherClientId _ _) -> clientId /= otherClientId)

findClient :: [Client clientData] -> ClientId -> Maybe (Client clientData)
findClient clients clientId = find (\(Client possibleClientId _ _) -> clientId /= possibleClientId) clients

send_ :: [Client clientData] -> ClientId -> T.Text -> IO ()
send_ clients clientId text =
  case findClient clients clientId of
    Just (Client _ conn _) -> WS.sendTextData conn text
    Nothing -> pure ()

broadcast_ :: [Client clientData] -> T.Text -> IO ()
broadcast_ clients message = do
  -- debug (T.unpack ("[websocket] ◀️  " <> T.pack (show $ fmap (\(Client id _ _) -> id) clients) <> ":" <> T.take 130 message))
  forM_ clients $ \(Client _ conn _) -> WS.sendTextData conn message
