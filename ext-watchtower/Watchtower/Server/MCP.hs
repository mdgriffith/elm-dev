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
import qualified Watchtower.Server.MCP.Protocol as MCP


availableTools :: [MCP.Tool]
availableTools =
  [
    MCP.Tool
      { MCP.toolName = "overview",
        MCP.toolDescription = "Get the overview of an Elm project.",
        MCP.toolInputSchema =
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
        MCP.toolOutputSchema = Nothing,
        MCP.call = \args _state -> do
          -- For now, return a simple success message
          -- In practice, this would extract the file path from the state or arguments
          return $ MCP.ToolCallText "file-read functionality available"
      }
  ]

-- * Available Resources

availableResources :: [MCP.Resource]
availableResources =
  [ MCP.Resource
      { MCP.resourceUri = "file://elm.json",
        MCP.resourceName = "Project Configuration",
        MCP.resourceDescription = Just "Elm project configuration file",
        MCP.resourceMimeType = Just "application/json"
      }
    -- MCP.Resource
    --   { resourceUri = "file://src/",
    --     resourceName = "Source Directory",
    --     resourceDescription = Just "Elm source code directory",
    --     resourceMimeType = Just "text/directory"
    --   }
  ]

-- * Available Prompts

availablePrompts :: [MCP.Prompt]
availablePrompts =
  [ MCP.Prompt
      { MCP.promptName = "elm_review",
        MCP.promptDescription = "Review Elm code for best practices and potential issues",
        MCP.promptArguments =
          Just
            [ MCP.PromptArgument
                { MCP.promptArgName = "code",
                  MCP.promptArgDescription = "The Elm code to review",
                  MCP.promptArgRequired = Just True
                }
            ]
      },
    MCP.Prompt
      { MCP.promptName = "elm_explain",
        MCP.promptDescription = "Explain how a piece of Elm code works",
        MCP.promptArguments =
          Just
            [ MCP.PromptArgument
                { MCP.promptArgName = "code",
                  MCP.promptArgDescription = "The Elm code to explain",
                  MCP.promptArgRequired = Just True
                }
            ]
      }
  ]

-- * Tool Implementations

-- | Main MCP server handler
serve :: Live.State -> JSONRPC.EventEmitter -> JSONRPC.Request -> IO (Either JSONRPC.Error JSONRPC.Response)
serve state emitter req = do
  MCP.serve availableTools availableResources availablePrompts state emitter req