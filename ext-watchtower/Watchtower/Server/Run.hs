{-# LANGUAGE OverloadedStrings #-}

module Watchtower.Server.Run where

import Control.Concurrent (forkIO)
import qualified Control.Concurrent.STM as STM
import Control.Exception (IOException, try)
import Control.Monad (forever, void)
import Control.Monad.Trans (MonadIO (liftIO))
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS.Char8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Ext.Common
import qualified Ext.Log
import Snap.Core hiding (method)
import qualified Snap.Http.Server as Server
import qualified Snap.Util.CORS as CORS
import System.IO (hFlush, hPutStrLn, stderr, stdin, stdout)
import qualified System.IO as IO
import qualified Watchtower.Live as Live
import qualified Watchtower.Server.JSONRPC as JSONRPC
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.IO.Unsafe (unsafePerformIO)

data Mode = StdIO | HTTP (Maybe Int)

-- | Event emitter for sending server-initiated JSON-RPC notifications
type EventEmitter = JSONRPC.EventEmitter

-- Global emitter for ad-hoc signals (set when the server starts)
{-# NOINLINE globalEmitterRef #-}
globalEmitterRef :: IORef (Maybe EventEmitter)
globalEmitterRef = unsafePerformIO (newIORef Nothing)

-- | Emit a server-initiated notification from anywhere
emit :: JSONRPC.Outbound -> IO ()
emit notif = do
  Ext.Log.log Ext.Log.LSP $ "Emitting notification: " ++ outboudnName notif
  maybeEmit <- readIORef globalEmitterRef
  case maybeEmit of
    Just f -> f notif
    Nothing -> pure ()

outboudnName :: JSONRPC.Outbound -> String
outboudnName (JSONRPC.OutboundNotification (JSONRPC.Notification _ method _)) = "notification: " ++ T.unpack method
outboudnName (JSONRPC.OutboundRequest method _) = "req: " ++ T.unpack method

run :: Live.State -> Mode -> Handler -> NotificationHandler -> IO ()
run state mode handler notificationHandler =
  case mode of
    StdIO -> runStdIO state handler notificationHandler
    HTTP maybePort -> do
      let port = Ext.Common.withDefault 51213 maybePort
      debug <- Ext.Log.isActive Ext.Log.VerboseServer
      Ext.Common.atomicPutStrLn $ "elm-dev is now running at http://localhost:" ++ show port
      Server.httpServe (config port debug) $
        runHttp state handler notificationHandler

config :: Int -> Bool -> Server.Config Snap a
config port isDebug =
  Server.setVerbose isDebug $
    Server.setPort port $
      Server.setAccessLog (Server.ConfigIoLog logger) $
        Server.setErrorLog
          (Server.ConfigIoLog logger)
          Server.defaultConfig

logger bs =
  Ext.Log.log Ext.Log.VerboseServer $ T.unpack $ T.decodeUtf8 bs

-- | Type for handling JSON-RPC requests
type Handler = Live.State -> EventEmitter -> JSONRPC.Request -> IO (Either JSONRPC.Error JSONRPC.Response)

-- | Type for handling JSON-RPC notifications
type NotificationHandler = Live.State -> EventEmitter -> JSONRPC.Notification -> IO ()

handleRequest :: Live.State -> EventEmitter -> Handler -> JSONRPC.Request -> IO JSONRPC.Message
handleRequest st emitEvent hdlr req = do
  result <- hdlr st emitEvent req
  case result of
    Left err ->
      return $ JSONRPC.ErrorMessage err
    Right response ->
      return $ JSONRPC.ResponseMessage response



-- | Parse Content-Length header
parseContentLength :: String -> Maybe Int
parseContentLength line = 
  case break (== ':') line of
    ("Content-Length", ':':' ':rest) -> readMaybe (takeWhile (\c -> c /= '\r' && c /= '\n') rest)
    ("Content-Length", ':':rest) -> readMaybe (takeWhile (\c -> c /= '\r' && c /= '\n') (dropWhile (== ' ') rest))
    _ -> Nothing
  where
    readMaybe s = case reads s of
      [(x, "")] -> Just x
      _ -> Nothing

-- | Read stdin message with Content-Length header
readStdInMessage :: IO (Maybe LBS.ByteString)
readStdInMessage = do
  headerLine <- getLine
  let cleanHeaderLine = filter (/= '\r') headerLine  -- Remove any remaining \r
  case parseContentLength cleanHeaderLine of
    Nothing -> do
      IO.hPutStrLn IO.stderr $ "Invalid Content-Length header: " ++ cleanHeaderLine
      IO.hFlush IO.stderr
      return Nothing  -- Invalid header
    Just contentLength -> do
      -- Read the empty line separator
      emptyLine <- getLine      
      -- Read the exact number of content bytes
      content <- LBS.hGet stdin contentLength

      return (Just content)

-- | Send JSON RPC message with Content-Length header  
sendWithContentLength :: LBS.ByteString -> IO ()
sendWithContentLength content = do
  let contentLength = LBS.length content
  -- Use proper CRLF line endings for stdio based JSON RPC.
  putStr $ "Content-Length: " ++ show contentLength ++ "\r\n"
  putStr "\r\n"  -- Empty line separator with CRLF
  LBS.putStr content
  hFlush stdout


-- | Run JSON-RPC server over stdio
runStdIO :: Live.State -> Handler -> NotificationHandler -> IO ()
runStdIO state handler notificationHandler = do
  -- Set line buffering for better debugging
  IO.hSetBuffering stdin IO.LineBuffering
  IO.hSetBuffering stdout IO.LineBuffering
  IO.hSetBuffering IO.stderr IO.LineBuffering
  
  -- Create an event channel and a background thread to forward events to stdout
  eventsChan <- STM.newTChanIO :: IO (STM.TChan JSONRPC.Outbound)
  let emitEvent :: EventEmitter
      emitEvent notif = STM.atomically (STM.writeTChan eventsChan notif)

  -- Expose global emitter for ad-hoc signals
  writeIORef globalEmitterRef (Just emitEvent)

  _ <- forkIO $ forever $ do
    outbound <- STM.atomically (STM.readTChan eventsChan)
    case outbound of
      JSONRPC.OutboundNotification notif -> do
        let msg = JSON.encode (JSONRPC.NotificationMessage notif)
        sendWithContentLength msg
      JSONRPC.OutboundRequest method params -> do
        -- For stdio, encode as a JSON-RPC request without id (notification) since there is no reply path here
        let notif = JSONRPC.Notification "2.0" method params
        let msg = JSON.encode (JSONRPC.NotificationMessage notif)
        sendWithContentLength msg
  
  forever $ do
    maybeMessage <- readStdInMessage
    case maybeMessage of
      Nothing -> do
        IO.hPutStrLn IO.stderr "Failed to read message (invalid header)"
        IO.hFlush IO.stderr
      Just messageContent -> do
        case JSON.eitherDecode messageContent of
          Left err -> do
            -- Send parse error response
            let errorResponse = JSON.encode (JSONRPC.parseError (T.pack err))
            sendWithContentLength errorResponse
          Right jsonMsg -> do
            case jsonMsg of
              JSONRPC.RequestMessage req@(JSONRPC.Request _ _ reqMethod _) -> do
                response <- handleRequest state emitEvent handler req
                sendWithContentLength (JSON.encode response)
              JSONRPC.NotificationMessage notif@(JSONRPC.Notification _ notifMethod _) -> do
                notificationHandler state emitEvent notif
              JSONRPC.ResponseMessage _ ->
                return ()
              JSONRPC.ErrorMessage _ -> do
                return ()

-- | Run JSON-RPC server over HTTP using Snap
runHttp :: Live.State -> Handler -> NotificationHandler -> Snap ()
runHttp state handler notificationHandler = do
  -- Create an event channel per server instance
  eventsChan <- liftIO (STM.newTChanIO :: IO (STM.TChan JSONRPC.Outbound))
  let emitEvent :: EventEmitter
      emitEvent notif = STM.atomically (STM.writeTChan eventsChan notif)

  -- Expose global emitter for ad-hoc signals
  liftIO $ writeIORef globalEmitterRef (Just emitEvent)

  route [("", jsonRPCHandler emitEvent), ("/events", sseHandler eventsChan)]
  where
    jsonRPCHandler :: EventEmitter -> Snap ()
    jsonRPCHandler emitEvent = do
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
                  response <- liftIO $ handleRequest state emitEvent handler req
                  writeJSON response
                JSONRPC.NotificationMessage notif -> do
                  liftIO $ notificationHandler state emitEvent notif
                  writeBS "OK"
                JSONRPC.ResponseMessage _ -> do
                  -- We shouldn't receive responses when acting as a server
                  writeBS "Invalid: received response"
                JSONRPC.ErrorMessage _ -> do
                  -- We shouldn't receive errors when acting as a server
                  writeBS "Invalid: received error"
        _ -> do
          writeBS "Only POST method is supported"

    sseHandler :: STM.TChan JSONRPC.Outbound -> Snap ()
    sseHandler chan = do
      -- Allow cross-origin access for local tooling
      CORS.applyCORS CORS.defaultOptions $ do
        modifyResponse $ setContentType "text/event-stream"
        modifyResponse $ addHeader "Cache-Control" "no-cache"
        modifyResponse $ addHeader "Connection" "keep-alive"
        -- Each client gets its own read end of the channel
        dup <- liftIO $ STM.atomically (STM.dupTChan chan)
        let loop = do
              outbound <- liftIO $ STM.atomically (STM.readTChan dup)
              let bytes = case outbound of
                             JSONRPC.OutboundNotification notif -> JSON.encode (JSONRPC.NotificationMessage notif)
                             JSONRPC.OutboundRequest method params -> JSON.encode (JSONRPC.NotificationMessage (JSONRPC.Notification "2.0" method params))
              writeBS "data: "
              writeLBS bytes
              writeBS "\n\n"
              loop
        loop

    writeJSON :: (JSON.ToJSON a) => a -> Snap ()
    writeJSON obj = do
      modifyResponse $ setContentType "application/json"
      writeLBS $ JSON.encode obj
