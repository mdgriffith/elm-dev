{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Watchtower.Server.JSONRPC where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text, pack)
import GHC.Generics

-- * Core JSON-RPC Types

-- | Request ID can be either a string or number
data RequestId
  = StringId Text
  | NumberId Int
  deriving stock (Show, Eq, Generic)

instance ToJSON RequestId where
  toJSON (StringId s) = toJSON s
  toJSON (NumberId n) = toJSON n

instance FromJSON RequestId where
  parseJSON v = (StringId <$> parseJSON v) <|> (NumberId <$> parseJSON v)

-- | JSON-RPC error information
data ErrorInfo = ErrorInfo
  { code :: Int,
    message :: Text,
    errorData :: Maybe Value
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON ErrorInfo where
  toJSON (ErrorInfo c m d) =
    object $
      [ "code" .= c,
        "message" .= m
      ]
        ++ maybe [] (\ed -> ["data" .= ed]) d

instance FromJSON ErrorInfo where
  parseJSON = withObject "ErrorInfo" $ \o ->
    ErrorInfo <$> o .: "code" <*> o .: "message" <*> o .:? "data"

-- | A JSON-RPC request that expects a response
data Request = Request
  { jsonrpc :: Text, -- Always "2.0"
    id :: RequestId,
    method :: Text,
    params :: Maybe Value
  }
  deriving stock (Show, Eq, Generic)

$(deriveJSON defaultOptions {omitNothingFields = True} ''Request)

toRequestId :: Request -> RequestId
toRequestId (Request _ reqId _ _) = reqId

-- | A successful JSON-RPC response
data Response = Response
  { jsonrpc :: Text, -- Always "2.0"
    id :: RequestId,
    result :: Value
  }
  deriving stock (Show, Eq, Generic)

$(deriveJSON defaultOptions ''Response)

-- | A JSON-RPC error response
data Error = Error
  { jsonrpc :: Text, -- Always "2.0"
    id :: RequestId,
    error :: ErrorInfo
  }
  deriving stock (Show, Eq, Generic)

$(deriveJSON defaultOptions ''Error)

-- | A JSON-RPC notification (no response expected)
data Notification = Notification
  { jsonrpc :: Text, -- Always "2.0"
    method :: Text,
    params :: Maybe Value
  }
  deriving stock (Show, Eq, Generic)

$(deriveJSON defaultOptions {omitNothingFields = True} ''Notification)

-- | Any JSON-RPC message
data Message
  = RequestMessage Request
  | ResponseMessage Response
  | ErrorMessage Error
  | NotificationMessage Notification
  deriving stock (Show, Eq, Generic)

instance ToJSON Message where
  toJSON (RequestMessage r) = toJSON r
  toJSON (ResponseMessage r) = toJSON r
  toJSON (ErrorMessage e) = toJSON e
  toJSON (NotificationMessage n) = toJSON n

instance FromJSON Message where
  parseJSON v =
    (RequestMessage <$> parseJSON v)
      <|> (ResponseMessage <$> parseJSON v)
      <|> (ErrorMessage <$> parseJSON v)
      <|> (NotificationMessage <$> parseJSON v)

-- * Helper Functions

-- | Create a successful JSON-RPC response
success :: RequestId -> Value -> Response
success reqId result =
  Response
    { jsonrpc = "2.0",
      id = reqId,
      result = result
    }

err :: RequestId -> Text -> Error
err reqId body =
  Watchtower.Server.JSONRPC.Error
    { jsonrpc = "2.0",
      id = reqId,
      error = ErrorInfo internalErrorCode body Nothing
    }

parseError :: Text -> Error
parseError body =
  Watchtower.Server.JSONRPC.Error
    { jsonrpc = "2.0",
      id = NumberId 0, -- we failed to parse the request, so we don't have an id
      error = ErrorInfo parseErrorCode body Nothing
    }

errorMethodNotFound :: RequestId -> Text -> Error
errorMethodNotFound reqId methodName =
  Watchtower.Server.JSONRPC.Error
    { jsonrpc = "2.0",
      id = reqId,
      error =
        ErrorInfo
          methodNotFoundCode
          (Data.Text.pack "Method not found")
          --   Not sure this is right, but no internet to check
          ( Just
              ( Data.Aeson.object
                  [ "method" .= methodName
                  ]
              )
          )
    }

-- * Error Codes

-- | Standard JSON-RPC error codes
parseErrorCode :: Int
parseErrorCode = -32700

invalidRequestCode :: Int
invalidRequestCode = -32600

methodNotFoundCode :: Int
methodNotFoundCode = -32601

invalidParamsCode :: Int
invalidParamsCode = -32602

internalErrorCode :: Int
internalErrorCode = -32603

serverErrorCode :: Int -> Int
serverErrorCode n = -32000 - n -- Server error codes from -32000 to -32099