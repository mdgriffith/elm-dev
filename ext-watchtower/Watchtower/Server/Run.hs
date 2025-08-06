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
import System.IO (hFlush, hPutStrLn, stderr, stdin, stdout)
import qualified System.IO as IO
import qualified Watchtower.Live as Live
import qualified Watchtower.Server.JSONRPC as JSONRPC

data Mode = StdIO | HTTP (Maybe Int)

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
type Handler = Live.State -> JSONRPC.Request -> IO (Either JSONRPC.Error JSONRPC.Response)

-- | Type for handling JSON-RPC notifications
type NotificationHandler = Live.State -> JSONRPC.Notification -> IO ()

handleRequest :: Live.State -> Handler -> JSONRPC.Request -> IO JSONRPC.Message
handleRequest st hdlr req = do
  result <- hdlr st req
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
                response <- handleRequest state handler req
                sendWithContentLength (JSON.encode response)
              JSONRPC.NotificationMessage notif@(JSONRPC.Notification _ notifMethod _) -> do
                notificationHandler state notif
              JSONRPC.ResponseMessage _ ->
                return ()
              JSONRPC.ErrorMessage _ -> do
                return ()

-- | Run JSON-RPC server over HTTP using Snap
runHttp :: Live.State -> Handler -> NotificationHandler -> Snap ()
runHttp state handler notificationHandler = do
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
                  liftIO $ notificationHandler state notif
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
