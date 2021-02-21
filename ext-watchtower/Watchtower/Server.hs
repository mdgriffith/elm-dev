{-# LANGUAGE OverloadedStrings #-}

module Watchtower.Server (Flags(..),serve) where


import Control.Applicative ((<|>))
import Control.Monad.Trans (MonadIO(liftIO))
import System.IO (hFlush, hPutStr, hPutStrLn, stderr, stdout)
import Snap.Core hiding (path)
import Snap.Http.Server
import Snap.Util.FileServe

import qualified Data.Text as Text
import qualified Data.Text.Encoding as T

import Control.Concurrent.STM
import Control.Monad (guard)
import Control.Applicative ((<|>))
import Text.Read (readMaybe)


import Snap.Core
import Snap.Http.Server
import Snap.Util.FileServe

import qualified Watchtower.Websocket as Websocket
import Watchtower.Duplicates


import qualified Develop.Generate.Help
import qualified Json.Encode
import qualified Reporting.Annotation as Ann
import qualified Watchtower.StaticAssets
import qualified Watchtower.Live
import qualified Watchtower.Details
import qualified Watchtower.Questions

data Flags =
  Flags
    { _port :: Maybe Int
    }

serve :: Flags -> IO ()
serve (Flags maybePort) =
  do  let port = maybe 9000 id maybePort
      putStrLn $ "Go to http://localhost:" ++ show port ++ " to see your project dashboard."
      liveState <- liftIO $ Watchtower.Live.init
      httpServe (config port) $
        serveAssets
            <|> Watchtower.Live.websocket liveState
            <|> Watchtower.Questions.serve
            <|> error404




config :: Int -> Config Snap a
config port =
  setVerbose False $ setPort port $
    setAccessLog ConfigNoLog $ setErrorLog ConfigNoLog $ defaultConfig


-- SERVE STATIC ASSETS


serveAssets :: Snap ()
serveAssets =
  do  path <- getSafePath
      case Watchtower.StaticAssets.lookup path of
        Nothing ->
          pass

        Just (content, mimeType) ->
          do  modifyResponse (setContentType (mimeType <> ";charset=utf-8"))
              writeBS content


-- NOT FOUND


run :: () -> Flags -> IO ()
run _ (Flags port) = do
  httpServe (config (withDefault 9000 port)) $
    serveWebsockets
    <|> error404


config :: Int -> Config Snap a
config port =
  setVerbose False $ setPort port $
    setAccessLog ConfigNoLog $ setErrorLog ConfigNoLog $ defaultConfig


serveWebsockets = do
  file <- getSafePath
  guard (file == "_w")
  mKey <- getHeader "sec-websocket-key" <$> getRequest

  case mKey of
    Just key -> do
      mClients <- liftIO $ newTVarIO []

      let
        onJoined clientId totalClients = do
          putStrLn "onJoined todo"
          pure Nothing

        onReceive clientId text = do
          putStrLn "onReceive todo"

      Websocket.runWebSocketsSnap $ Websocket.socketHandler mClients onJoined onReceive (T.decodeUtf8 key)

    Nothing ->
      error404


error404 :: Snap ()
error404 =
  do  modifyResponse $ setResponseStatus 404 "Not Found"
      modifyResponse $ setContentType "text/html; charset=utf-8"
      writeBuilder $ "404 page not found!"
