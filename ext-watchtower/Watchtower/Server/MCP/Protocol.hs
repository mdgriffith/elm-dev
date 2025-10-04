{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Watchtower.Server.MCP.Protocol
  ( ServerCapabilities(..)
  , ClientCapabilities(..)
  , Implementation(..)
  , InitializeRequest(..)
  , InitializeResponse(..)
  , Tool(..)
  , ToolCallRequest(..)
  , ToolContent(..)
  , Annotations(..)
  , Audience(..)
  , Priority(..)
  , ToolResponseChunk(..)
  , ResourceLinkInfo(..)
  , ResourceEmbeddedInfo(..)
  , ToolCallResponse(..)
  , Resource(..)
  , ReadResourceRequest(..)
  , ResourceContent(..)
  , ReadResourceResponse(..)
  , Prompt(..)
  , PromptArgument(..)
  , serve
  ) where

import Data.Aeson ((.=))
import qualified Data.Aeson as JSON
import Data.Aeson.TH
import Data.List (find)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding
import qualified Ext.Common
import GHC.Generics
import qualified Watchtower.Live as Live
import qualified Watchtower.Server.JSONRPC as JSONRPC
import qualified Watchtower.Server.MCP.Uri as Uri

-- * MCP Core Types

data ServerCapabilities = ServerCapabilities
  { tools :: Maybe JSON.Value,
    resources :: Maybe JSON.Value,
    prompts :: Maybe JSON.Value
  }
  deriving stock (Show, Eq, Generic)

data ClientCapabilities = ClientCapabilities
  { clientSampling :: Maybe JSON.Value
  }
  deriving stock (Show, Eq, Generic)

data Implementation = Implementation
  { implName :: Text,
    implVersion :: Text
  }
  deriving stock (Show, Eq, Generic)

data InitializeRequest = InitializeRequest
  { initProtocolVersion :: Text,
    initCapabilities :: ClientCapabilities,
    initClientInfo :: Implementation
  }
  deriving stock (Show, Eq, Generic)

data InitializeResponse = InitializeResponse
  { responseProtocolVersion :: Text,
    responseCapabilities :: ServerCapabilities,
    responseServerInfo :: Implementation
  }
  deriving stock (Show, Eq, Generic)

-- * Tool Types

data Tool = Tool
  { toolName :: Text,
    toolDescription :: Text,
    toolInputSchema :: JSON.Value,
    toolOutputSchema :: Maybe JSON.Value,
    call :: JSON.Object -> Live.State -> JSONRPC.EventEmitter -> IO ToolCallResponse
  }

instance Show Tool where
  show tool =
    "Tool { toolName = "
      ++ show (toolName tool)
      ++ ", toolDescription = "
      ++ show (toolDescription tool)
      ++ ", toolInputSchema = "
      ++ show (toolInputSchema tool)
      ++ ", toolOutputSchema = "
      ++ show (toolOutputSchema tool)
      ++ ", call = <function> }"

instance Eq Tool where
  t1 == t2 =
    toolName t1 == toolName t2
      && toolDescription t1 == toolDescription t2
      && toolInputSchema t1 == toolInputSchema t2
      && toolOutputSchema t1 == toolOutputSchema t2

data ToolCallRequest = ToolCallRequest
  { callToolName :: Text,
    callToolArguments :: Maybe JSON.Value
  }
  deriving stock (Show, Eq, Generic)

data ToolContent = ToolContent
  { toolContentType :: Text,
    toolContentText :: Text
  }
  deriving stock (Show, Eq, Generic)

-- * Annotations

data Annotations = Annotations
  { annotationsAudience :: [Audience]
  , annotationsPriority :: Priority
  , annotationsLastModified :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

data Audience
  = AudienceUser
  | AudienceAssistant
  deriving stock (Show, Eq, Generic)

data Priority = High | Medium | Low
  deriving stock (Show, Eq, Generic)

data ResourceLinkInfo = ResourceLinkInfo
  { resourceLinkUri :: Text,
    resourceLinkName :: Text,
    resourceLinkDescription :: Maybe Text,
    resourceLinkMimeType :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

data ResourceEmbeddedInfo = ResourceEmbeddedInfo
  { resourceEmbeddedUri :: Text,
    resourceEmbeddedTitle :: Text,
    resourceEmbeddedMimeType :: Text,
    resourceEmbeddedText :: Text
  }
  deriving stock (Show, Eq, Generic)

data ToolResponseChunk
  = ToolResponseError (Maybe Annotations) Text
  | ToolResponseText (Maybe Annotations) Text
  | ToolResponseImage (Maybe Annotations) Text
  | ToolResponseAudio (Maybe Annotations) Text
  | ToolResponseResourceLink (Maybe Annotations) ResourceLinkInfo
  | ToolResponseResourceEmbedded (Maybe Annotations) ResourceEmbeddedInfo
  | ToolResponseStructured (Maybe Annotations) JSON.Value
  deriving stock (Show, Eq, Generic)

data ToolCallResponse = ToolCallResponse [ToolResponseChunk]
  deriving stock (Show, Eq, Generic)

-- * Resource Types

data Resource = Resource
  { resourceUri :: Uri.Pattern,
    resourceName :: Text,
    resourceDescription :: Maybe Text,
    resourceMimeType :: Maybe Text,
    resourceAnnotations :: Maybe Annotations,
    read :: ReadResourceRequest -> Live.State -> JSONRPC.EventEmitter -> IO ReadResourceResponse
  }
  deriving stock (Generic)

instance Show Resource where
  show r =
    "Resource { resourceUri = " ++ show (Uri.renderPattern (resourceUri r))
      ++ ", resourceName = " ++ show (resourceName r)
      ++ ", resourceDescription = " ++ show (resourceDescription r)
      ++ ", resourceMimeType = " ++ show (resourceMimeType r)
      ++ ", resourceAnnotations = " ++ show (resourceAnnotations r)
      ++ ", read = <function> }"

instance Eq Resource where
  a == b =
    Uri.renderPattern (resourceUri a) == Uri.renderPattern (resourceUri b)
      && resourceName a == resourceName b
      && resourceDescription a == resourceDescription b
      && resourceMimeType a == resourceMimeType b
      && resourceAnnotations a == resourceAnnotations b

data ReadResourceRequest = ReadResourceRequest
  { readResourceUri :: Text
  }
  deriving stock (Show, Eq, Generic)

data ResourceContent = ResourceContent
  { resourceContentUri :: Text,
    resourceContentMimeType :: Text,
    resourceContentText :: Text,
    resourceContentAnnotations :: Maybe Annotations
  }
  deriving stock (Show, Eq, Generic)

data ReadResourceResponse = ReadResourceResponse
  { readResourceContents :: [ResourceContent]
  }
  deriving stock (Show, Eq, Generic)

-- * Prompt Types

data Prompt = Prompt
  { promptName :: Text,
    promptDescription :: Text,
    promptArguments :: Maybe [PromptArgument]
  }
  deriving stock (Show, Eq, Generic)

data PromptArgument = PromptArgument
  { promptArgName :: Text,
    promptArgDescription :: Text,
    promptArgRequired :: Maybe Bool
  }
  deriving stock (Show, Eq, Generic)

-- * JSON Instances

$(deriveJSON defaultOptions {omitNothingFields = True} ''ServerCapabilities)
$(deriveJSON defaultOptions {fieldLabelModifier = Ext.Common.removePrefixAndDecapitalize "client", omitNothingFields = True} ''ClientCapabilities)
$(deriveJSON defaultOptions {fieldLabelModifier = Ext.Common.removePrefixAndDecapitalize "impl"} ''Implementation)
$(deriveJSON defaultOptions {fieldLabelModifier = Ext.Common.removePrefixAndDecapitalize "init"} ''InitializeRequest)
$(deriveJSON defaultOptions {fieldLabelModifier = Ext.Common.removePrefixAndDecapitalize "response"} ''InitializeResponse)
$(deriveJSON defaultOptions {fieldLabelModifier = Ext.Common.removePrefixAndDecapitalize "annotations", omitNothingFields = True} ''Annotations)

instance JSON.ToJSON Audience where
  toJSON a = case a of
    AudienceUser -> JSON.String "user"
    AudienceAssistant -> JSON.String "assistant"

instance JSON.FromJSON Audience where
  parseJSON = JSON.withText "Audience" $ \t -> case Text.toLower t of
    "user" -> pure AudienceUser
    "assistant" -> pure AudienceAssistant
    _ -> fail "Audience must be 'user' or 'assistant'"

instance JSON.ToJSON Priority where
  toJSON p = case p of
    High -> JSON.toJSON (0.9 :: Double)
    Medium -> JSON.toJSON (0.7 :: Double)
    Low -> JSON.toJSON (0.5 :: Double)

instance JSON.FromJSON Priority where
  parseJSON = JSON.withScientific "Priority" $ \s -> case s of
    0.9 -> pure High
    0.7 -> pure Medium
    0.5 -> pure Low
    _ -> pure Low


instance JSON.ToJSON Tool where
  toJSON tool =
    let baseFields =
          [ "name" .= toolName tool,
            "description" .= toolDescription tool,
            "inputSchema" .= toolInputSchema tool
          ]
        outputSchemaField = case toolOutputSchema tool of
          Just schema -> ["outputSchema" .= schema]
          Nothing -> []
     in JSON.object (baseFields ++ outputSchemaField)

$(deriveJSON defaultOptions {fieldLabelModifier = Ext.Common.removePrefixAndDecapitalize "callTool", omitNothingFields = True} ''ToolCallRequest)
$(deriveJSON defaultOptions {fieldLabelModifier = Ext.Common.removePrefixAndDecapitalize "toolContent"} ''ToolContent)

encodeChunk :: ToolResponseChunk -> JSON.Value
encodeChunk chunk = case chunk of
  ToolResponseError ann msg ->
    JSON.object
      ( [ "type" .= ("text" :: Text)
        , "text" .= msg
        ] ++ maybe [] (\a -> ["annotations" .= a]) ann
      )
  ToolResponseText ann txt ->
    JSON.object (["type" .= ("text" :: Text), "text" .= txt] ++ maybe [] (\a -> ["annotations" .= a]) ann)
  ToolResponseImage ann img ->
    JSON.object
      ( [ "type" .= ("image" :: Text)
        , "mimeType" .= ("image/png" :: Text)
        , "data" .= img
        ] ++ maybe [] (\a -> ["annotations" .= a]) ann
      )
  ToolResponseAudio ann audio ->
    JSON.object
      ( [ "type" .= ("audio" :: Text)
        , "mimeType" .= ("audio/wav" :: Text)
        , "data" .= audio
        ] ++ maybe [] (\a -> ["annotations" .= a]) ann
      )
  ToolResponseResourceLink ann linkInfo ->
    let baseFields =
          [ "type" .= ("resource_link" :: Text)
          , "uri" .= resourceLinkUri linkInfo
          , "name" .= resourceLinkName linkInfo
          ]
        descField = case resourceLinkDescription linkInfo of
          Just desc -> ["description" .= desc]
          Nothing -> []
        mimeField = case resourceLinkMimeType linkInfo of
          Just mime -> ["mimeType" .= mime]
          Nothing -> []
        annField = case ann of
          Just a -> ["annotations" .= a]
          Nothing -> []
    in JSON.object (baseFields ++ descField ++ mimeField ++ annField)
  ToolResponseResourceEmbedded ann embeddedInfo ->
    JSON.object
      ( [ "type" .= ("resource_embedded" :: Text)
        , "uri" .= resourceEmbeddedUri embeddedInfo
        , "title" .= resourceEmbeddedTitle embeddedInfo
        , "mimeType" .= resourceEmbeddedMimeType embeddedInfo
        , "text" .= resourceEmbeddedText embeddedInfo
        ] ++ maybe [] (\a -> ["annotations" .= a]) ann
      )
  ToolResponseStructured ann val ->
    JSON.object
      ( [ "type" .= ("structured" :: Text)
        , "content" .= Data.Text.Encoding.decodeUtf8 (LBS.toStrict (JSON.encode val))
        ] ++ maybe [] (\a -> ["annotations" .= a]) ann
      )

isErrorChunk :: ToolResponseChunk -> Bool
isErrorChunk chunk = case chunk of
  ToolResponseError _ _ -> True
  _ -> False

instance JSON.ToJSON ToolCallResponse where
  toJSON (ToolCallResponse chunks) =
    let contentBlocks = fmap encodeChunk chunks
        hasError = any isErrorChunk chunks
        baseFields = ["content" .= contentBlocks]
        errorField = ["isError" .= hasError]
        structuredField = case chunks of
          [ToolResponseStructured _ val] -> ["structuredContent" .= val]
          _ -> []
    in JSON.object (baseFields ++ errorField ++ structuredField)

-- Custom ToJSON instance for Resource to omit function field
instance JSON.ToJSON Resource where
  toJSON r = JSON.object $
    [ "uri" .= Uri.renderPattern (resourceUri r)
    , "name" .= resourceName r
    ]
    ++ maybe [] (\d -> ["description" .= d]) (resourceDescription r)
    ++ maybe [] (\m -> ["mimeType" .= m]) (resourceMimeType r)
    ++ maybe [] (\a -> ["annotations" .= a]) (resourceAnnotations r)
$(deriveJSON defaultOptions {fieldLabelModifier = Ext.Common.removePrefixAndDecapitalize "readResource"} ''ReadResourceRequest)
$(deriveJSON defaultOptions {fieldLabelModifier = Ext.Common.removePrefixAndDecapitalize "resourceContent", omitNothingFields = True} ''ResourceContent)
$(deriveJSON defaultOptions {fieldLabelModifier = Ext.Common.removePrefixAndDecapitalize "readResource"} ''ReadResourceResponse)
$(deriveJSON defaultOptions {fieldLabelModifier = Ext.Common.removePrefixAndDecapitalize "promptArg", omitNothingFields = True} ''PromptArgument)
$(deriveJSON defaultOptions {fieldLabelModifier = Ext.Common.removePrefixAndDecapitalize "prompt", omitNothingFields = True} ''Prompt)
$(deriveJSON defaultOptions {fieldLabelModifier = Ext.Common.removePrefixAndDecapitalize "resourceLink", omitNothingFields = True} ''ResourceLinkInfo)
$(deriveJSON defaultOptions {fieldLabelModifier = Ext.Common.removePrefixAndDecapitalize "resourceEmbedded", omitNothingFields = True} ''ResourceEmbeddedInfo)

-- * Helper Functions

success :: JSONRPC.RequestId -> JSON.Value -> Either JSONRPC.Error JSONRPC.Response
success reqId result =
  Right (JSONRPC.success reqId result)

err :: JSONRPC.RequestId -> String -> Either JSONRPC.Error JSONRPC.Response
err reqId str =
  Left (JSONRPC.err reqId (Text.pack str))

-- * Server

serve :: [Tool] -> [Resource] -> [Prompt] -> Live.State -> JSONRPC.EventEmitter -> JSONRPC.Request -> IO (Either JSONRPC.Error JSONRPC.Response)
serve tools resources prompts state emit req@(JSONRPC.Request _ reqId method params) = do
  case Text.unpack method of
    "initialize" -> do
      let initResponse =
            InitializeResponse
              { responseProtocolVersion = "2025-06-18",
                responseCapabilities =
                  ServerCapabilities
                    { tools = Just $ JSON.object ["listChanged" .= True],
                      resources = Just $ JSON.object ["subscribe" .= False, "listChanged" .= True],
                      prompts = Just $ JSON.object ["listChanged" .= True]
                    },
                responseServerInfo =
                  Implementation
                    { implName = "elm-dev",
                      implVersion = "1.0.0"
                    }
              }
      return $ success reqId (JSON.toJSON initResponse)
    "tools/list" -> do
      return $ success reqId (JSON.object ["tools" .= tools])
    "tools/call" -> do
      case params of
        Just p -> do
          case JSON.fromJSON p of
            JSON.Success (callReq :: ToolCallRequest) -> do
              let toolName' = callToolName callReq
              case [t | t <- tools, toolName t == toolName' ] of
                (tool' : _) -> do
                  let args = maybe mempty (\argsVal -> case argsVal of JSON.Object obj -> obj; _ -> mempty) (callToolArguments callReq)
                  response <- call tool' args state emit
                  return $ success reqId (JSON.toJSON response)
                [] -> do
                  let response = ToolCallResponse [ToolResponseError Nothing ("Unknown tool: " <> toolName')]
                  return $ success reqId (JSON.toJSON response)
            JSON.Error e -> return $ err reqId ("Invalid tool call request: " ++ e)
        Nothing -> return $ err reqId "Missing parameters for tools/call"
    "resources/list" -> do
      return $ success reqId (JSON.object ["resources" .= resources])
    "resources/read" -> do
      case params of
        Just p -> do
          case JSON.fromJSON p of
            JSON.Success (readReq :: ReadResourceRequest) -> do
              let matched = [ r | r <- resources, Uri.match (resourceUri r) (readResourceUri readReq) /= Nothing ]
              case matched of
                (r:_) -> do
                  response <- Watchtower.Server.MCP.Protocol.read r readReq state emit
                  return $ success reqId (JSON.toJSON response)
                [] -> do
                  let response =
                        ReadResourceResponse
                          { readResourceContents =
                              [ ResourceContent
                                  { resourceContentUri = readResourceUri readReq
                                  , resourceContentMimeType = "text/plain"
                                  , resourceContentText = "Unknown resource URI"
                                  , resourceContentAnnotations = Nothing
                                  }
                              ]
                          }
                  return $ success reqId (JSON.toJSON response)
            JSON.Error e -> return $ err reqId ("Invalid resource read request: " ++ e)
        Nothing -> return $ err reqId "Missing parameters for resources/read"
    "prompts/list" -> do
      return $ success reqId (JSON.object ["prompts" .= prompts])
    "prompts/get" -> do
      return $ success reqId (JSON.object ["messages" .= ([] :: [JSON.Value])])
    "ping" -> do
      return $ success reqId (JSON.String "pong")
    "echo" -> do
      let result = case params of
            Just p -> p
            Nothing -> JSON.Null
      return $ success reqId result
    _ -> do
      return $ Left (JSONRPC.errorMethodNotFound reqId method)
