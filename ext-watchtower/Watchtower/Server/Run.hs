{-# LANGUAGE OverloadedStrings #-}

module Watchtower.Server.Run where

import Control.Concurrent (forkIO)
import Control.Exception (IOException, try)
import Control.Monad (forever, void)
import Control.Monad.Trans (MonadIO (liftIO))
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS.Char8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Ext.Common
import qualified Ext.Log
import Snap.Core hiding (method)
import qualified Snap.Http.Server as Server
import qualified Snap.Util.CORS as CORS
import System.IO (hFlush, stdin, stdout)
import qualified System.IO as IO
import qualified Watchtower.Live as Live
import qualified Watchtower.Server.JSONRPC as JSONRPC

data Mode = StdIO | HTTP (Maybe Int)

run :: Live.State -> Mode -> Handler -> IO ()
run state mode handler =
  case mode of
    StdIO -> runStdIO state handler
    HTTP maybePort -> do
      let port = Ext.Common.withDefault 51213 maybePort
      debug <- Ext.Log.isActive Ext.Log.VerboseServer
      Ext.Common.atomicPutStrLn $ "elm-dev is now running at http://localhost:" ++ show port
      Server.httpServe (config port debug) $
        runHttp state handler

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

-- | Parse LSP Content-Length header
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

-- | Read LSP message with Content-Length header
readLSPMessage :: IO (Maybe LBS.ByteString)
readLSPMessage = do
  headerLine <- getLine
  let cleanHeaderLine = filter (/= '\r') headerLine  -- Remove any remaining \r
  -- IO.hPutStrLn IO.stderr $ "Read header: " ++ show headerLine
  -- IO.hPutStrLn IO.stderr $ "Clean header: " ++ show cleanHeaderLine
  -- IO.hFlush IO.stderr
  
  case parseContentLength cleanHeaderLine of
    Nothing -> do
      IO.hPutStrLn IO.stderr $ "Invalid Content-Length header: " ++ cleanHeaderLine
      IO.hFlush IO.stderr
      return Nothing  -- Invalid header
    Just contentLength -> do
      -- IO.hPutStrLn IO.stderr $ "Content-Length: " ++ show contentLength
      -- IO.hFlush IO.stderr
      
      -- Read the empty line separator
      emptyLine <- getLine
      -- let cleanEmptyLine = filter (/= '\r') emptyLine
      -- IO.hPutStrLn IO.stderr $ "Read separator line: " ++ show emptyLine
      -- IO.hPutStrLn IO.stderr $ "Clean separator line: " ++ show cleanEmptyLine
      -- IO.hFlush IO.stderr
      
      -- Read the exact number of content bytes
      content <- LBS.hGet stdin contentLength
      -- IO.hPutStrLn IO.stderr $ "Read content: " ++ show (LBS.length content) ++ " bytes"
      -- IO.hFlush IO.stderr
      
      return (Just content)

-- | Send LSP message with Content-Length header  
sendLSPMessage :: LBS.ByteString -> IO ()
sendLSPMessage content = do
  let contentLength = LBS.length content
  -- IO.hPutStrLn IO.stderr $ "Sending message: " ++ show contentLength ++ " bytes"
  -- IO.hPutStrLn IO.stderr $ "Content: " ++ LBS.Char8.unpack content
  -- IO.hFlush IO.stderr
  
  -- Use proper CRLF line endings for LSP protocol
  putStr $ "Content-Length: " ++ show contentLength ++ "\r\n"
  putStr "\r\n"  -- Empty line separator with CRLF
  LBS.putStr content
  hFlush stdout
  
  -- IO.hPutStrLn IO.stderr "Message sent and flushed"
  -- IO.hFlush IO.stderr

-- | Run JSON-RPC server over stdio
runStdIO :: Live.State -> Handler -> IO ()
runStdIO state handler = do
  -- Set line buffering for better debugging
  IO.hSetBuffering stdin IO.LineBuffering
  IO.hSetBuffering stdout IO.LineBuffering
  IO.hSetBuffering IO.stderr IO.LineBuffering
  
  -- Log startup
  -- IO.hPutStrLn IO.stderr "LSP Server starting via stdio..."
  -- IO.hFlush IO.stderr
  
  forever $ do
    -- IO.hPutStrLn IO.stderr "Waiting for LSP message..."
    -- IO.hFlush IO.stderr
    
    maybeMessage <- readLSPMessage
    case maybeMessage of
      Nothing -> do
        IO.hPutStrLn IO.stderr "Failed to read LSP message (invalid header)"
        IO.hFlush IO.stderr
      Just messageContent -> do
        -- IO.hPutStrLn IO.stderr $ "Received message: " ++ show (LBS.length messageContent) ++ " bytes"
        -- IO.hFlush IO.stderr
        
        case JSON.eitherDecode messageContent of
          Left err -> do
            -- IO.hPutStrLn IO.stderr $ "JSON parse error: " ++ err
            -- IO.hFlush IO.stderr
            -- Send parse error response
            let errorResponse = JSON.encode (JSONRPC.parseError (T.pack err))
            sendLSPMessage errorResponse
          Right jsonMsg -> do
            -- IO.hPutStrLn IO.stderr $ "Parsed JSON message successfully"
            -- IO.hFlush IO.stderr
            
            case jsonMsg of
              JSONRPC.RequestMessage req@(JSONRPC.Request _ _ reqMethod _) -> do
                -- IO.hPutStrLn IO.stderr $ "Handling request: " ++ T.unpack reqMethod
                -- IO.hFlush IO.stderr
                response <- handleRequest state handler req
                -- IO.hPutStrLn IO.stderr $ "Sending response: " ++ show response
                -- IO.hFlush IO.stderr
                sendLSPMessage (JSON.encode response)
              JSONRPC.NotificationMessage notif@(JSONRPC.Notification _ notifMethod _) -> do
                -- IO.hPutStrLn IO.stderr $ "Handling notification: " ++ T.unpack notifMethod
                -- IO.hFlush IO.stderr
                -- Notifications don't expect responses
                handleNotification state notif
              JSONRPC.ResponseMessage _ ->
                -- IO.hPutStrLn IO.stderr "Warning: received response message (unexpected for server)"
                -- IO.hFlush IO.stderr
                return ()
              JSONRPC.ErrorMessage _ -> do
                -- IO.hPutStrLn IO.stderr "Warning: received error message (unexpected for server)"
                -- IO.hFlush IO.stderr
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
