module Watchtower.Server.MCP (serve) where

import qualified Data.Aeson as JSON
import qualified Data.Text as Text
import qualified Watchtower.Live as Live
import qualified Watchtower.Server.JSONRPC as JSONRPC

success reqId result =
  Right (JSONRPC.success reqId result)

err reqId str =
  Left (JSONRPC.err reqId (Text.pack str))

-- | Example handler that demonstrates the interface
serve :: Live.State -> JSONRPC.Request -> IO (Either JSONRPC.Error JSONRPC.Response)
serve _state req@(JSONRPC.Request _ reqId method params) = do
  case Text.unpack method of
    "ping" -> do
      return $ success reqId (JSON.String (Text.pack "pong"))
    "echo" -> do
      let result = case params of
            Just p -> p
            Nothing -> JSON.Null
      return $ success reqId result
    _ -> do
      return $
        (Left (JSONRPC.errorMethodNotFound reqId method))
