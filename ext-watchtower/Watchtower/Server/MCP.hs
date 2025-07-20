{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Watchtower.Server.MCP (serve) where

{-
Can test via https://modelcontextprotocol.io/docs/tools/inspector
  
Run: `npx @modelcontextprotocol/inspector`.

In the app, select STDIO and put `elm-dev` with the arg `mcp` and then hit connect.

-}

import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson ((.=))
import qualified Data.Aeson as JSON
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.TH
import qualified Data.ByteString.Lazy as LBS
import Data.Char (toLower)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding
import qualified Ext.Common
import GHC.Generics
import System.Process (readProcess)
import qualified Watchtower.Live as Live
import qualified Watchtower.Server.JSONRPC as JSONRPC

-- * MCP Core Types

-- | Server capabilities
data ServerCapabilities = ServerCapabilities
  { tools :: Maybe JSON.Value,
    resources :: Maybe JSON.Value,
    prompts :: Maybe JSON.Value
  }
  deriving stock (Show, Eq, Generic)

-- | Client capabilities (received during initialization)
data ClientCapabilities = ClientCapabilities
  { clientSampling :: Maybe JSON.Value
  }
  deriving stock (Show, Eq, Generic)

-- | Implementation info
data Implementation = Implementation
  { implName :: Text,
    implVersion :: Text
  }
  deriving stock (Show, Eq, Generic)

-- | Initialize request
data InitializeRequest = InitializeRequest
  { initProtocolVersion :: Text,
    initCapabilities :: ClientCapabilities,
    initClientInfo :: Implementation
  }
  deriving stock (Show, Eq, Generic)

-- | Initialize response
data InitializeResponse = InitializeResponse
  { responseProtocolVersion :: Text,
    responseCapabilities :: ServerCapabilities,
    responseServerInfo :: Implementation
  }
  deriving stock (Show, Eq, Generic)

-- * Tool Types

-- | Tool definition
data Tool = Tool
  { toolName :: Text,
    toolDescription :: Text,
    toolInputSchema :: JSON.Value,
    toolOutputSchema :: Maybe JSON.Value,
    call :: JSON.Object -> Live.State -> IO ToolCallResponse
  }

-- Remove the deriving clause since functions can't derive Show, Eq
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

-- Note: can't compare functions, so we compare everything else

-- | Tool call request
data ToolCallRequest = ToolCallRequest
  { callToolName :: Text,
    callToolArguments :: Maybe JSON.Value
  }
  deriving stock (Show, Eq, Generic)

-- | Tool result content
data ToolContent = ToolContent
  { toolContentType :: Text,
    toolContentText :: Text
  }
  deriving stock (Show, Eq, Generic)

-- | Resource link details
data ResourceLinkInfo = ResourceLinkInfo
  { resourceLinkUri :: Text,
    resourceLinkName :: Text,
    resourceLinkDescription :: Maybe Text,
    resourceLinkMimeType :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

-- | Embedded resource details
data ResourceEmbeddedInfo = ResourceEmbeddedInfo
  { resourceEmbeddedUri :: Text,
    resourceEmbeddedTitle :: Text,
    resourceEmbeddedMimeType :: Text,
    resourceEmbeddedText :: Text
  }
  deriving stock (Show, Eq, Generic)

-- | Tool call response
data ToolCallResponse
  = ToolCallError Text
  | ToolCallText Text
  | ToolCallImage Text -- base64 encoded or URL
  | ToolCallAudio Text -- base64 encoded or URL
  | ToolCallResourceLink ResourceLinkInfo
  | ToolCallResourceEmbedded ResourceEmbeddedInfo
  | ToolCallStructured JSON.Value
  deriving stock (Show, Eq, Generic)

-- * Resource Types

-- | Resource definition
data Resource = Resource
  { resourceUri :: Text,
    resourceName :: Text,
    resourceDescription :: Maybe Text,
    resourceMimeType :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

-- | Resource read request
data ReadResourceRequest = ReadResourceRequest
  { readResourceUri :: Text
  }
  deriving stock (Show, Eq, Generic)

-- | Resource content
data ResourceContent = ResourceContent
  { resourceContentUri :: Text,
    resourceContentMimeType :: Text,
    resourceContentText :: Text
  }
  deriving stock (Show, Eq, Generic)

-- | Resource read response
data ReadResourceResponse = ReadResourceResponse
  { readResourceContents :: [ResourceContent]
  }
  deriving stock (Show, Eq, Generic)

-- * Prompt Types

-- | Prompt definition
data Prompt = Prompt
  { promptName :: Text,
    promptDescription :: Text,
    promptArguments :: Maybe [PromptArgument]
  }
  deriving stock (Show, Eq, Generic)

-- | Prompt argument
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

-- Manual JSON instances for Tool (excluding the function field)
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

toToolError :: [JSON.Value] -> JSON.Value
toToolError content =
  JSON.object
    [ "isError" .= True,
      "content" .= content
    ]

toToolContent :: [JSON.Value] -> JSON.Value
toToolContent content =
  JSON.object
    [ "isError" .= False,
      "content" .= content
    ]

toToolStructured :: JSON.Value -> [JSON.Value] -> JSON.Value
toToolStructured structured content =
  JSON.object
    [ "content" .= content,
      "structuredContent" .= structured
    ]



-- Custom JSON instance for ToolCallResponse tagged union
instance JSON.ToJSON ToolCallResponse where
  toJSON response = case response of
    ToolCallError msg ->
      toToolError
        [ JSON.object
          [ "type" .= ("text" :: Text),
            "text" .= msg
          ]
        ]
    ToolCallText txt ->
      toToolContent [JSON.object ["type" .= ("text" :: Text), "text" .= txt]]
    ToolCallImage img ->
      toToolContent
        [JSON.object
          [ "type" .= ("image" :: Text),
            "mimeType" .= ("image/png" :: Text),
            "data" .= img
          ]
        ]
    ToolCallAudio audio ->
      toToolContent
        [ JSON.object
            [ "type" .= ("audio" :: Text),
              "mimeType" .= ("audio/wav" :: Text),
              "data" .= audio
           ]
        ]
    ToolCallResourceLink linkInfo ->
      let baseFields =
            [ "type" .= ("resource_link" :: Text),
              "uri" .= resourceLinkUri linkInfo,
              "name" .= resourceLinkName linkInfo
            ]
          descField = case resourceLinkDescription linkInfo of
            Just desc -> ["description" .= desc]
            Nothing -> []
          mimeField = case resourceLinkMimeType linkInfo of
            Just mime -> ["mimeType" .= mime]
            Nothing -> []
      in toToolContent [JSON.object (baseFields ++ descField ++ mimeField)]
    ToolCallResourceEmbedded embeddedInfo ->
      toToolContent
        [JSON.object
          [ "type" .= ("resource_embedded" :: Text),
            "uri" .= resourceEmbeddedUri embeddedInfo,
            "title" .= resourceEmbeddedTitle embeddedInfo,
            "mimeType" .= resourceEmbeddedMimeType embeddedInfo,
            "text" .= resourceEmbeddedText embeddedInfo
          ]
        ]
    ToolCallStructured val ->
      toToolStructured val 
        [ JSON.object
            [ "type" .= ("structured" :: Text),
              "content" .= Data.Text.Encoding.decodeUtf8 (LBS.toStrict (JSON.encode val))
            ]
        ]

$(deriveJSON defaultOptions {fieldLabelModifier = Ext.Common.removePrefixAndDecapitalize "resource", omitNothingFields = True} ''Resource)
$(deriveJSON defaultOptions {fieldLabelModifier = Ext.Common.removePrefixAndDecapitalize "readResource"} ''ReadResourceRequest)
$(deriveJSON defaultOptions {fieldLabelModifier = Ext.Common.removePrefixAndDecapitalize "resourceContent"} ''ResourceContent)
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

-- * Available Tools

availableTools :: [Tool]
availableTools =
  [
    Tool
      { toolName = "overview",
        toolDescription = "Get the overview of an Elm project.",
        toolInputSchema =
          JSON.object
            [ "type" .= ("object" :: Text),
              "properties"
                .= JSON.object
                  [ "dir"
                      .= JSON.object
                        [ "type" .= ("string" :: Text),
                          "description" .= ("The directory of the Elm project." :: Text)
                        ]
                  ],
              "required" .= (["dir"] :: [Text])
            ],
        toolOutputSchema = Nothing,
        call = \args _state -> do
          -- For now, return a simple success message
          -- In practice, this would extract the file path from the state or arguments
          return $ ToolCallText "file-read functionality available"
      }
  ]

-- * Available Resources

availableResources :: [Resource]
availableResources =
  [ Resource
      { resourceUri = "file://elm.json",
        resourceName = "Project Configuration",
        resourceDescription = Just "Elm project configuration file",
        resourceMimeType = Just "application/json"
      },
    Resource
      { resourceUri = "file://src/",
        resourceName = "Source Directory",
        resourceDescription = Just "Elm source code directory",
        resourceMimeType = Just "text/directory"
      }
  ]

-- * Available Prompts

availablePrompts :: [Prompt]
availablePrompts =
  [ Prompt
      { promptName = "elm_review",
        promptDescription = "Review Elm code for best practices and potential issues",
        promptArguments =
          Just
            [ PromptArgument
                { promptArgName = "code",
                  promptArgDescription = "The Elm code to review",
                  promptArgRequired = Just True
                }
            ]
      },
    Prompt
      { promptName = "elm_explain",
        promptDescription = "Explain how a piece of Elm code works",
        promptArguments =
          Just
            [ PromptArgument
                { promptArgName = "code",
                  promptArgDescription = "The Elm code to explain",
                  promptArgRequired = Just True
                }
            ]
      }
  ]

-- * Tool Implementations

-- | Main MCP server handler
serve :: Live.State -> JSONRPC.Request -> IO (Either JSONRPC.Error JSONRPC.Response)
serve _state req@(JSONRPC.Request _ reqId method params) = do
  case Text.unpack method of
    -- Core MCP methods
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
      return $ success reqId (JSON.object ["tools" .= availableTools])
    "tools/call" -> do
      case params of
        Just p -> do
          case JSON.fromJSON p of
            JSON.Success (callReq :: ToolCallRequest) -> do
              let toolName' = callToolName callReq
              case [t | t <- availableTools, toolName t == toolName'] of
                (tool : _) -> do
                  -- Extract arguments from the request
                  let args = maybe mempty (\argsVal -> case argsVal of JSON.Object obj -> obj; _ -> mempty) (callToolArguments callReq)
                  response <- call tool args _state
                  return $ success reqId (JSON.toJSON response)
                [] -> do
                  let response = ToolCallError ("Unknown tool: " <> toolName')
                  return $ success reqId (JSON.toJSON response)
            JSON.Error e -> return $ err reqId ("Invalid tool call request: " ++ e)
        Nothing -> return $ err reqId "Missing parameters for tools/call"
    "resources/list" -> do
      return $ success reqId (JSON.object ["resources" .= availableResources])
    "resources/read" -> do
      case params of
        Just p -> do
          case JSON.fromJSON p of
            JSON.Success (readReq :: ReadResourceRequest) -> do
              -- Simple implementation - in practice you'd handle different URI schemes
              let response =
                    ReadResourceResponse
                      { readResourceContents =
                          [ ResourceContent
                              { resourceContentUri = readResourceUri readReq,
                                resourceContentMimeType = "text/plain",
                                resourceContentText = "Resource content would be loaded here"
                              }
                          ]
                      }
              return $ success reqId (JSON.toJSON response)
            JSON.Error e -> return $ err reqId ("Invalid resource read request: " ++ e)
        Nothing -> return $ err reqId "Missing parameters for resources/read"
    "prompts/list" -> do
      return $ success reqId (JSON.object ["prompts" .= availablePrompts])
    "prompts/get" -> do
      return $ success reqId (JSON.object ["messages" .= ([] :: [JSON.Value])])

    -- Legacy test methods
    "ping" -> do
      return $ success reqId (JSON.String "pong")
    "echo" -> do
      let result = case params of
            Just p -> p
            Nothing -> JSON.Null
      return $ success reqId result
    _ -> do
      return $ Left (JSONRPC.errorMethodNotFound reqId method)
