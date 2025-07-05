{-# LANGUAGE OverloadedStrings #-}

module Watchtower.Server.Run where

import Control.Concurrent (forkIO)
import Control.Exception (IOException, try)
import Control.Monad (forever, void)
import Control.Monad.Trans (MonadIO (liftIO))
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Snap.Core hiding (method)
import qualified Snap.Http.Server as Server
import qualified Snap.Util.CORS as CORS
import System.IO (hFlush, stdin, stdout)
import qualified System.IO as IO
import qualified Watchtower.Live as Live
import qualified Watchtower.Server.JSONRPC as JSONRPC

-- | Type for handling JSON-RPC requests
type Handler = Live.State -> JSONRPC.Request -> IO (Either JSONRPC.Error JSONRPC.Response)

handleRequest :: Live.State -> Handler -> JSONRPC.Request -> IO JSONRPC.Message
handleRequest st hdlr req = do
  result <- hdlr st req
  case result of
    Left err ->
      return $ JSONRPC.ErrorMessage err
    Right response ->
      return $ JSONRPC.ResponseMessage response

handleNotification :: Live.State -> JSONRPC.Notification -> IO ()
handleNotification _st _notif = do
  -- Handle notifications here if needed
  return ()

-- | Run JSON-RPC server over stdio
runStdIO :: Live.State -> Handler -> IO ()
runStdIO state handler = do
  IO.hSetBuffering stdin IO.LineBuffering
  IO.hSetBuffering stdout IO.LineBuffering
  forever $ do
    line <- getLine
    case JSON.eitherDecode (LBS.pack line) of
      Left err -> do
        -- Send parse error response
        putStrLn $ LBS.unpack $ JSON.encode (JSONRPC.parseError (T.pack err))
        hFlush stdout
      Right jsonMsg -> do
        case jsonMsg of
          JSONRPC.RequestMessage req -> do
            response <- handleRequest state handler req
            putStrLn $ LBS.unpack $ JSON.encode response
            hFlush stdout
          JSONRPC.NotificationMessage notif -> do
            -- Notifications don't expect responses
            handleNotification state notif
          JSONRPC.ResponseMessage _ -> do
            -- We shouldn't receive responses when acting as a server
            return ()
          JSONRPC.ErrorMessage _ -> do
            -- We shouldn't receive errors when acting as a server
            return ()

-- | Run JSON-RPC server over HTTP using Snap
runHttp :: Live.State -> Handler -> Snap ()
runHttp state handler = do
  route [("", jsonRPCHandler)]
  where
    jsonRPCHandler :: Snap ()
    jsonRPCHandler = do
      method <- rqMethod <$> getRequest
      case method of
        POST -> do
          body <- readRequestBody 65536 -- 64KB limit
          case JSON.eitherDecode body of
            Left err -> do
              writeJSON $ JSONRPC.parseError (T.pack err)
            Right jsonMsg -> do
              case jsonMsg of
                JSONRPC.RequestMessage req -> do
                  response <- liftIO $ handleRequest state handler req
                  writeJSON response
                JSONRPC.NotificationMessage notif -> do
                  liftIO $ handleNotification state notif
                  writeBS "OK"
                JSONRPC.ResponseMessage _ -> do
                  -- We shouldn't receive responses when acting as a server
                  writeBS "Invalid: received response"
                JSONRPC.ErrorMessage _ -> do
                  -- We shouldn't receive errors when acting as a server
                  writeBS "Invalid: received error"
        _ -> do
          writeBS "Only POST method is supported"

    writeJSON :: (JSON.ToJSON a) => a -> Snap ()
    writeJSON obj = do
      modifyResponse $ setContentType "application/json"
      writeLBS $ JSON.encode obj
