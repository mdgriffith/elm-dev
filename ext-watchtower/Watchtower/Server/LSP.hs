{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Watchtower.Server.LSP (serve, handleNotification) where

{-
Language Server Protocol implementation for Elm development.

Can be tested with any LSP client. For VS Code, you can connect via the extension.
-}

import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Encode.Pretty
import qualified Data.Text as T
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.TH
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Builder
import Data.Char (toLower)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding
import qualified Ext.ElmFormat
import qualified Ext.Common
import qualified Ext.Dev
import qualified Ext.FileCache
import qualified Ext.Render.Type
import Data.Maybe (maybeToList)
import qualified Data.Maybe as Maybe
import qualified Data.Name as Name
import GHC.Generics
import qualified Reporting.Annotation as Ann
import qualified Reporting.Doc
import qualified Reporting.Render.Type
import qualified Reporting.Render.Type.Localizer
import qualified Reporting.Warning as Warning
import System.Process (readProcess)
import qualified Watchtower.Live as Live
import qualified Watchtower.Server.JSONRPC as JSONRPC
import qualified Watchtower.State.Discover as Discover
import qualified Watchtower.Live.Client
import qualified Watchtower.Live.Compile
import qualified Watchtower.State.Compile
import qualified Reporting.Error
import qualified Reporting.Exit
import qualified Reporting.Annotation
import qualified Reporting.Report
import qualified Reporting.Render.Code
import qualified Data.NonEmptyList
import qualified System.FilePath
import qualified Data.List
import qualified Data.Map as Map
import qualified Ext.CompileHelpers.Generic
import qualified Control.Concurrent.STM
import qualified Ext.Log
import qualified Ext.Reporting.Error
import qualified Elm.ModuleName
import qualified Elm.Package
import qualified Elm.Docs
import qualified Json.String
import qualified AST.Source as Src
import qualified AST.Canonical as Can
import qualified Watchtower.AST.Lookup
import qualified Watchtower.AST.Definition
import qualified Watchtower.AST.References
import qualified Ext.Dev.Project
import Watchtower.Server.LSP.Protocol
import qualified Watchtower.Server.LSP.Helpers as Helpers
import qualified Watchtower.Server.LSP.EditorsOpen as EditorsOpen
import qualified Watchtower.Live.Client as Client



-- * Default Server Capabilities

defaultServerCapabilities :: ServerCapabilities
defaultServerCapabilities = ServerCapabilities
  { serverCapabilitiesPositionEncoding = Just "utf-16",
    serverCapabilitiesTextDocumentSync = Just $ TextDocumentSyncOptions
      { textDocumentSyncOpenClose = True,
        textDocumentSyncChange = TextDocumentSyncIncremental,
        textDocumentSyncSave = Just $ SaveOptions { saveOptionsIncludeText = True }
      },
    serverCapabilitiesNotebookDocumentSync = Nothing,
    serverCapabilitiesCompletionProvider = Just $ JSON.object
      [ "resolveProvider" .= False,
        "triggerCharacters" .= ([".", "(", "["] :: [Text])
      ],
    serverCapabilitiesHoverProvider = Just True,
    serverCapabilitiesSignatureHelpProvider = Just $ JSON.object
      [ "triggerCharacters" .= (["(", ","] :: [Text]),
        "retriggerCharacters" .= ([")"] :: [Text])
      ],
    serverCapabilitiesDeclarationProvider = Just True,
    serverCapabilitiesDefinitionProvider = Just True,
    serverCapabilitiesTypeDefinitionProvider = Just True,
    serverCapabilitiesImplementationProvider = Just True,
    serverCapabilitiesReferencesProvider = Just True,
    serverCapabilitiesDocumentHighlightProvider = Nothing,
    serverCapabilitiesDocumentSymbolProvider = Nothing,
    serverCapabilitiesCodeActionProvider = Just $ JSON.object
      [ "codeActionKinds" .= (["quickfix", "refactor", "source"] :: [Text]),
        "resolveProvider" .= True
      ],
    serverCapabilitiesCodeLensProvider = Just $ JSON.object
      [ "resolveProvider" .= True
      ],
    serverCapabilitiesDocumentLinkProvider = Nothing,
    serverCapabilitiesColorProvider = Nothing,
    serverCapabilitiesDocumentFormattingProvider = Just True,
    serverCapabilitiesDocumentRangeFormattingProvider = Nothing,
    serverCapabilitiesDocumentOnTypeFormattingProvider = Nothing,
    serverCapabilitiesRenameProvider = Just $ JSON.object
      [ "prepareProvider" .= True
      ],
    serverCapabilitiesFoldingRangeProvider = Nothing,
    serverCapabilitiesExecuteCommandProvider = Nothing,
    serverCapabilitiesSelectionRangeProvider = Just True,
    serverCapabilitiesLinkedEditingRangeProvider = Nothing,
    serverCapabilitiesCallHierarchyProvider = Nothing,
    serverCapabilitiesSemanticTokensProvider = Nothing,
    serverCapabilitiesMonikerProvider = Nothing,
    serverCapabilitiesTypeHierarchyProvider = Just True,
    serverCapabilitiesInlineValueProvider = Nothing,
    serverCapabilitiesInlayHintProvider = Just $ JSON.object
      [ "resolveProvider" .= False
      ],
    serverCapabilitiesDiagnosticProvider = Just $ JSON.object
      [ "interFileDependencies" .= True,
        "workspaceDiagnostics" .= False
      ],
    serverCapabilitiesWorkspaceSymbolProvider = Nothing,
    serverCapabilitiesWorkspace = Nothing,
    serverCapabilitiesExperimental = Nothing
  }

-- * Feature Implementations

handleInitialize :: Live.State -> InitializeParams -> IO (Either String InitializeResult)
handleInitialize state initParams = do
  -- Prefer workspace folders; otherwise fall back to rootPath or rootUri (converted from file:// URI)
  let folderRoots = case initializeParamsWorkspaceFolders initParams of
        Just folders ->
          [ root
          | f <- folders
          , let uri = workspaceFolderUri f
          , Just root <- [uriToFilePath uri]
          ]
        Nothing -> []

  case folderRoots of
    r:rs -> do
      mapM_ (Discover.discover state) (r:rs)
    [] -> do
      case initializeParamsRootPath initParams of
        Just pathTxt -> do
          let root = Text.unpack pathTxt
          Discover.discover state root
        Nothing ->
          case initializeParamsRootUri initParams >>= uriToFilePath of
            Just root -> do
              Discover.discover state root
            Nothing -> do
              -- Log that no usable root was provided
              Ext.Log.log Ext.Log.LSP "LSP Initialize: No root path provided"
              pure ()
  
  -- If no root was provided and there are no projects registered, fail initialization
  let hadRootPath = Maybe.isJust (initializeParamsRootPath initParams)
  let hadRootUri = Maybe.isJust (initializeParamsRootUri initParams)
  let noRootProvided = null folderRoots && not hadRootPath && not hadRootUri
  let (Client.State _ mProjects _ _ _ _ _) = state
  projects <- Control.Concurrent.STM.readTVarIO mProjects

  let initResult = InitializeResult
        { initializeResultCapabilities = defaultServerCapabilities,
          initializeResultServerInfo = Just $ ServerInfo
            { serverInfoName = "elm-dev-lsp",
              serverInfoVersion = Just "1.0.0"
            }
        }
  if noRootProvided && null projects
    then return $ Left "No workspace root provided and no projects are registered"
    else return $ Right initResult

handleDidOpen :: Live.State -> DidOpenTextDocumentParams -> IO (Either String JSON.Value)
handleDidOpen state openParams = do
  -- Handle document open notification
  let doc = didOpenTextDocumentParamsTextDocument openParams
      uri = textDocumentItemUri doc
      languageId = textDocumentItemLanguageId doc
      version = textDocumentItemVersion doc
      text = textDocumentItemText doc
  
  -- On open: always update VFS with the provided text and recompile if we can resolve a file path
  case uriToFilePath uri of
    Nothing -> return $ Right JSON.Null
    Just filePath -> do
      -- Update in-memory cache with the full document text
      Ext.FileCache.insert filePath (Data.Text.Encoding.encodeUtf8 text)
      -- Track editor open
      let (Client.State _ _ _ _ _ _ mEditorsOpen) = state
      Control.Concurrent.STM.atomically $ do
        editors <- Control.Concurrent.STM.readTVar mEditorsOpen
        Control.Concurrent.STM.writeTVar mEditorsOpen (EditorsOpen.fileMarkedOpen filePath editors)
      -- Recompile relevant projects for this file
      Watchtower.State.Compile.compileRelevantProjects state [filePath]
      return $ Right JSON.Null

handleDidChange :: Live.State -> DidChangeTextDocumentParams -> IO (Either String JSON.Value)
handleDidChange state changeParams = do
  -- Handle document change notification
  let doc = didChangeTextDocumentParamsTextDocument changeParams
      uri = versionedTextDocumentIdentifierUri doc
      changes = didChangeTextDocumentParamsContentChanges changeParams
  
  -- Convert URI to file path
  case uriToFilePath uri of
    Nothing -> return $ Left $ "Failed to convert URI to file path: " ++ Text.unpack uri
    Just filePath -> do
      -- Convert LSP changes to FileCache edits
      let fileEdits = map lspChangeToFileEdit changes
      
      -- Apply edits to the file cache
      result <- Ext.FileCache.edit filePath fileEdits
      case result of
        Left (Ext.FileCache.FileNotFound path) -> 
          return $ Left $ "File not found: " ++ path
        Left (Ext.FileCache.InvalidEdit err) -> 
          return $ Left $ "Invalid edit: " ++ err
        Right () -> do
            Watchtower.State.Compile.compileRelevantProjects state [filePath]
            return $ Right JSON.Null

-- | Convert LSP TextDocumentContentChangeEvent to FileCache TextEdit
lspChangeToFileEdit :: TextDocumentContentChangeEvent -> Ext.FileCache.TextEdit
lspChangeToFileEdit change = 
  Ext.FileCache.TextEdit
    { Ext.FileCache.textEditRange = fmap lspRangeToFileRange (textDocumentContentChangeEventRange change),
      Ext.FileCache.textEditText = textDocumentContentChangeEventText change
    }

-- | Convert LSP Range to FileCache Range
lspRangeToFileRange :: Range -> Ext.FileCache.Range
lspRangeToFileRange lspRange =
  Ext.FileCache.Range
    { Ext.FileCache.rangeStart = lspPositionToFilePosition (rangeStart lspRange),
      Ext.FileCache.rangeEnd = lspPositionToFilePosition (rangeEnd lspRange)
    }

-- | Convert LSP Position to FileCache Position  
lspPositionToFilePosition :: Position -> Ext.FileCache.Position
lspPositionToFilePosition lspPos =
  Ext.FileCache.Position
    { Ext.FileCache.positionLine = positionLine lspPos,
      Ext.FileCache.positionCharacter = positionCharacter lspPos
    }

handleDidClose :: Live.State -> DidCloseTextDocumentParams -> IO (Either String JSON.Value)
handleDidClose state closeParams = do
  -- Handle document close notification
  let doc = didCloseTextDocumentParamsTextDocument closeParams
      uri = textDocumentIdentifierUri doc
  case uriToFilePath uri of
    Nothing -> return $ Right JSON.Null
    Just filePath -> do
      let (Client.State _ _ _ _ _ _ mEditorsOpen) = state
      Control.Concurrent.STM.atomically $ do
        editors <- Control.Concurrent.STM.readTVar mEditorsOpen
        Control.Concurrent.STM.writeTVar mEditorsOpen (EditorsOpen.fileMarkedClosed filePath editors)
      return $ Right JSON.Null

handleDidSave :: Live.State -> DidSaveTextDocumentParams -> IO (Either String JSON.Value)
handleDidSave state saveParams = do
  -- Handle document save notification
  let doc = didSaveTextDocumentParamsTextDocument saveParams
      uri = textDocumentIdentifierUri doc
      text = didSaveTextDocumentParamsText saveParams
  
  case uriToFilePath uri of
    Nothing -> return $ Right JSON.Null
    Just filePath -> do
      Watchtower.State.Compile.compileRelevantProjects state [filePath]
      return $ Right JSON.Null

handleHover :: Live.State -> HoverParams -> IO (Either String (Maybe Hover))
handleHover state hoverParams = do
  let lspPos = hoverParamsPosition hoverParams
      uri = textDocumentIdentifierUri (hoverParamsTextDocument hoverParams)
  case uriToFilePath uri of
    Nothing -> pure (Right Nothing)
    Just filePath -> do
      Helpers.showHover state filePath lspPos

handleCompletion :: Live.State -> CompletionParams -> IO (Either String CompletionList)
handleCompletion _state _completionParams = do
  -- For now, return some basic Elm completions
  -- In a real implementation, this would analyze the context and provide relevant completions
  let items = 
        [ CompletionItem
            { completionItemLabel = "String",
              completionItemLabelDetails = Nothing,
              completionItemKind = Just 7, -- Module
              completionItemTags = Nothing,
              completionItemDetail = Just "Built-in type",
              completionItemDocumentation = Just $ JSON.String "String type",
              completionItemDeprecated = Nothing,
              completionItemPreselect = Nothing,
              completionItemSortText = Nothing,
              completionItemFilterText = Nothing,
              completionItemInsertText = Just "String",
              completionItemInsertTextFormat = Nothing,
              completionItemInsertTextMode = Nothing,
              completionItemTextEdit = Nothing,
              completionItemAdditionalTextEdits = Nothing,
              completionItemCommitCharacters = Nothing,
              completionItemCommand = Nothing,
              completionItemData = Nothing
            },
          CompletionItem
            { completionItemLabel = "Int",
              completionItemLabelDetails = Nothing,
              completionItemKind = Just 7, -- Module
              completionItemTags = Nothing,
              completionItemDetail = Just "Built-in type",
              completionItemDocumentation = Just $ JSON.String "Integer type",
              completionItemDeprecated = Nothing,
              completionItemPreselect = Nothing,
              completionItemSortText = Nothing,
              completionItemFilterText = Nothing,
              completionItemInsertText = Just "Int",
              completionItemInsertTextFormat = Nothing,
              completionItemInsertTextMode = Nothing,
              completionItemTextEdit = Nothing,
              completionItemAdditionalTextEdits = Nothing,
              completionItemCommitCharacters = Nothing,
              completionItemCommand = Nothing,
              completionItemData = Nothing
            }
        ]
  
  return $ Right $ CompletionList
    { completionListIsIncomplete = False,
      completionListItemDefaults = Nothing,
      completionListItems = items
    }

handleDefinition :: Live.State -> DefinitionParams -> IO (Either String [Location])
handleDefinition state definitionParams = do
  let uri = textDocumentIdentifierUri (definitionParamsTextDocument definitionParams)
      lspPos = definitionParamsPosition definitionParams
  case uriToFilePath uri of
    Nothing -> pure (Right [])
    Just filePath -> do
      mDefLoc <- Helpers.findDefinition state uri filePath lspPos
      case mDefLoc of
        Nothing -> pure (Right [])
        Just defLoc -> pure (Right [defLoc])



handleDeclaration :: Live.State -> DefinitionParams -> IO (Either String [Location])
handleDeclaration _state definitionParams = do
  -- Placeholder: Return a sample declaration location
  -- In a real implementation, this would find where symbols are declared
  let uri = textDocumentIdentifierUri (definitionParamsTextDocument definitionParams)
      pos = definitionParamsPosition definitionParams
      sampleLocation = Location
        { locationUri = uri,
          locationRange = Range
            { rangeStart = Position (positionLine pos) 0,
              rangeEnd = Position (positionLine pos) 10
            }
        }
  return $ Right [sampleLocation]

handleTypeDefinition :: Live.State -> DefinitionParams -> IO (Either String [Location])
handleTypeDefinition _state definitionParams = do
  -- Placeholder: Return a sample type definition location
  -- In a real implementation, this would find where types are defined
  let uri = textDocumentIdentifierUri (definitionParamsTextDocument definitionParams)
      pos = definitionParamsPosition definitionParams
      sampleLocation = Location
        { locationUri = uri,
          locationRange = Range
            { rangeStart = Position (max 0 (positionLine pos - 5)) 0,
              rangeEnd = Position (max 0 (positionLine pos - 5)) 15
            }
        }
  return $ Right [sampleLocation]

parseIncludeDeclaration :: JSON.Value -> Bool
parseIncludeDeclaration v =
  case v of
    JSON.Object o ->
      case KeyMap.lookup "includeDeclaration" o of
        Just (JSON.Bool b) -> b
        _ -> True
    _ -> True

handleReferences :: Live.State -> ReferenceParams -> IO (Either String [Location])
handleReferences state referenceParams = do
  let uri = textDocumentIdentifierUri (referenceParamsTextDocument referenceParams)
      lspPos = referenceParamsPosition referenceParams
      includeDecl = parseIncludeDeclaration (referenceParamsContext referenceParams)
  case uriToFilePath uri of
    Nothing -> pure (Right [])
    Just filePath -> do
      Right <$> Helpers.findAllReferences state uri filePath lspPos includeDecl

handleImplementation :: Live.State -> DefinitionParams -> IO (Either String [Location])
handleImplementation _state _definitionParams = do
  -- Placeholder: Return empty list for now
  -- In Elm, this could show implementations of type class instances
  return $ Right []

handleRename :: Live.State -> RenameParams -> IO (Either String WorkspaceEdit)
handleRename _state renameParams = do
  -- Placeholder: Return a sample workspace edit
  -- In a real implementation, this would safely rename symbols across all files
  let uri = textDocumentIdentifierUri (renameParamsTextDocument renameParams)
      pos = renameParamsPosition renameParams
      newName = renameParamsNewName renameParams
      
      -- Create a text edit for the rename
      textEdit = JSON.object
        [ "range" .= JSON.object
            [ "start" .= JSON.object ["line" .= positionLine pos, "character" .= positionCharacter pos],
              "end" .= JSON.object ["line" .= positionLine pos, "character" .= (positionCharacter pos + 10)]
            ],
          "newText" .= newName
        ]
      
      -- Create the changes map
      sampleEdit = JSON.object
        [ 
          -- Text.unpack uri .= JSON.toJSON [textEdit]
        ]
      
      workspaceEdit = WorkspaceEdit
        { workspaceEditChanges = Just sampleEdit,
          workspaceEditDocumentChanges = Nothing
        }
  return $ Right workspaceEdit

handlePrepareRename :: Live.State -> PrepareRenameParams -> IO (Either String (Maybe PrepareRenameResponse))
handlePrepareRename _state prepareRenameParams = do
  -- Placeholder: Return a sample rename range
  -- In a real implementation, this would validate if the symbol can be renamed
  let pos = prepareRenameParamsPosition prepareRenameParams
      response = PrepareRenameResponse
        { prepareRenameResponseRange = Range
            { rangeStart = pos,
              rangeEnd = Position (positionLine pos) (positionCharacter pos + 8)
            },
          prepareRenameResponsePlaceholder = "newName"
        }
  return $ Right (Just response)

handlePrepareTypeHierarchy :: Live.State -> TypeHierarchyPrepareParams -> IO (Either String [TypeHierarchyItem])
handlePrepareTypeHierarchy _state typeHierarchyParams = do
  -- Placeholder: Return a sample type hierarchy item
  -- In Elm, this could show type alias expansions or union type hierarchies
  let uri = textDocumentIdentifierUri (typeHierarchyPrepareParamsTextDocument typeHierarchyParams)
      pos = typeHierarchyPrepareParamsPosition typeHierarchyParams
      sampleItem = TypeHierarchyItem
        { typeHierarchyItemName = "User",
          typeHierarchyItemKind = 5, -- Class
          typeHierarchyItemTags = Nothing,
          typeHierarchyItemDetail = Just "type User = { name : String, age : Int }",
          typeHierarchyItemUri = uri,
          typeHierarchyItemRange = Range
            { rangeStart = pos,
              rangeEnd = Position (positionLine pos) (positionCharacter pos + 4)
            },
          typeHierarchyItemSelectionRange = Range
            { rangeStart = pos,
              rangeEnd = Position (positionLine pos) (positionCharacter pos + 4)
            },
          typeHierarchyItemData = Nothing
        }
  return $ Right [sampleItem]

handleDiagnostic :: Live.State -> DocumentDiagnosticParams -> IO (Either String JSON.Value)
handleDiagnostic state diagnosticParams = do
  let uri = textDocumentIdentifierUri (documentDiagnosticParamsTextDocument diagnosticParams)
  
  -- Get project caches from state and run compilation
  case uriToFilePath uri of
    Nothing -> return $ Left "Invalid URI"  -- Invalid URI, return empty diagnostics
    Just filePath -> do
      projectResult <- Watchtower.Live.Client.getExistingProject filePath state
      case projectResult of
        Left Watchtower.Live.Client.NoProjectsRegistered -> return (Left "No projects registered")
        Left (Watchtower.Live.Client.ProjectNotFound _) -> return (Left "Project not found")
        Right (projectCache, _) ->
          do { diags <- Helpers.getDiagnosticsForProject state projectCache (Just filePath)
             ; (localizer_, warns) <- Helpers.getWarningsForFile state filePath

             -- Encode as JSON
             ; let diagnosticsResponse = JSON.object
                     [ "kind" .= ("full" :: Text)
                     , "items" .= (diags ++ concatMap Helpers.warningToUnusedDiagnostic warns)
                     ]
             ; return $ Right diagnosticsResponse
             }


-- | Convert LSP URI to file path (basic implementation)
uriToFilePath :: Text -> Maybe FilePath
uriToFilePath uri =
  case Text.stripPrefix "file://" uri of
    Just path -> Just (Text.unpack path)
    Nothing -> Nothing

handleInlayHint :: Live.State -> InlayHintParams -> IO (Either String [InlayHint])
handleInlayHint _state inlayHintParams = do
  -- Placeholder: Return sample inlay hints
  -- In Elm, this could show inferred types, parameter names in function calls, etc.
  let range = inlayHintParamsRange inlayHintParams
      startPos = rangeStart range
      sampleHints =
        [
          --  InlayHint
          --   { inlayHintPosition = Position (positionLine startPos + 2) (positionCharacter startPos + 10),
          --     inlayHintLabel = JSON.String ": String",
          --     inlayHintKind = Just 1, -- Type
          --     inlayHintTextEdits = Nothing,
          --     inlayHintTooltip = Just (JSON.String "Inferred type"),
          --     inlayHintPaddingLeft = Just False,
          --     inlayHintPaddingRight = Just False,
          --     inlayHintData = Nothing
          --   },
          -- InlayHint
          --   { inlayHintPosition = Position (positionLine startPos + 5) (positionCharacter startPos + 15),
          --     inlayHintLabel = JSON.String "name:",
          --     inlayHintKind = Just 2, -- Parameter
          --     inlayHintTextEdits = Nothing,
          --     inlayHintTooltip = Just (JSON.String "Parameter name"),
          --     inlayHintPaddingLeft = Just False,
          --     inlayHintPaddingRight = Just True,
          --     inlayHintData = Nothing
          --   }
        ]
  return $ Right sampleHints


handleSignatureHelp :: Live.State -> SignatureHelpParams -> IO (Either String (Maybe SignatureHelp))
handleSignatureHelp _state _signatureHelpParams = do
  -- Placeholder: Return sample signature help
  -- In a real implementation, this would show function signatures with parameter info
  let sampleSignature = SignatureInformation
        { signatureInformationLabel = "map : (a -> b) -> List a -> List b",
          signatureInformationDocumentation = Just (JSON.String "Apply a function to every element of a list."),
          signatureInformationParameters = Just
            [ JSON.object
                [ "label" .= ("(a -> b)" :: Text),
                  "documentation" .= ("The function to apply" :: Text)
                ],
              JSON.object
                [ "label" .= ("List a" :: Text),
                  "documentation" .= ("The list to map over" :: Text)
                ]
            ],
          signatureInformationActiveParameter = Just 0
        }
      
      signatureHelp = SignatureHelp
        { signatureHelpSignatures = [sampleSignature],
          signatureHelpActiveSignature = Just 0,
          signatureHelpActiveParameter = Just 0
        }
  return $ Right (Just signatureHelp)



-- | LSP TextEdit response item (range + newText)
data TextEditResponse = TextEditResponse
  { textEditResponseRange :: Range,
    textEditResponseNewText :: Text
  }
  deriving stock (Show, Eq, Generic)

$(deriveJSON defaultOptions {fieldLabelModifier = Ext.Common.removePrefixAndDecapitalize "textEditResponse"} ''TextEditResponse)

-- Full document formatting using elm-format
handleDocumentFormatting :: Live.State -> DocumentFormattingParams -> IO (Either String [TextEditResponse])
handleDocumentFormatting _state fmtParams = do
  let uri = textDocumentIdentifierUri (documentFormattingParamsTextDocument fmtParams)
  case uriToFilePath uri of
    Nothing -> pure $ Left ("Failed to convert URI to file path: " ++ Text.unpack uri)
    Just filePath -> do
      -- Read latest content from the file cache (or disk)
      bytes <- Ext.FileCache.readUtf8 filePath
      let inputText = Data.Text.Encoding.decodeUtf8 bytes
      -- Run formatter via local elm-format binary; on failure, return no edits
      formatted_ <- Ext.ElmFormat.format inputText
      case formatted_ of
        Left _err -> pure $ Right []
        Right formatted ->
          if formatted == inputText
            then pure $ Right []
            else do
              let (endLine, endChar) = fullDocumentEnd inputText
                  fullRange = Range { rangeStart = Position 0 0
                                    , rangeEnd = Position endLine endChar
                                    }
              pure $ Right [ TextEditResponse { textEditResponseRange = fullRange
                                              , textEditResponseNewText = formatted
                                              }
                           ]
  where
    -- Compute end position (last line index and last character index) for full replacement
    fullDocumentEnd :: Text -> (Int, Int)
    fullDocumentEnd txt =
      let ls = Text.lines txt in
      case ls of
        [] -> (0, 0)
        _  -> let lastLineIndex = length ls - 1
                  lastLineText = last ls
              in ( lastLineIndex, Text.length lastLineText )


handleCodeLens :: Live.State -> CodeLensParams -> IO (Either String [CodeLens])
handleCodeLens state codeLensParams = do
  let uri = textDocumentIdentifierUri (codeLensParamsTextDocument codeLensParams)
  
  Ext.Log.log Ext.Log.LSP $ "CodeLens: Processing URI: " ++ Text.unpack uri
      
  case uriToFilePath uri of
    Nothing -> do
      Ext.Log.log Ext.Log.LSP $ "CodeLens: Failed to convert URI to file path: " ++ Text.unpack uri
      return $ Right []
    Just filePath -> do
      (localizer, warns) <- Helpers.getWarningsForFile state filePath
      let codeLenses = concatMap (Helpers.warningToCodeLens localizer) warns
          missingAnnotationCount = length $ filter isMissingTypeAnnotation warns
      Ext.Log.log Ext.Log.LSP $ "CodeLens: Found " ++ show (length warns) ++ " total warnings, " ++ show missingAnnotationCount ++ " missing type annotations, " ++ show (length codeLenses) ++ " code lenses"
      return $ Right codeLenses

-- Helper to check if a warning is a MissingTypeAnnotation
isMissingTypeAnnotation :: Warning.Warning -> Bool
isMissingTypeAnnotation warning =
  case warning of
    Warning.MissingTypeAnnotation {} -> True
    _ -> False



handleCodeAction :: Live.State -> CodeActionParams -> IO (Either String [CodeAction])
handleCodeAction _state _codeActionParams = do
  -- Placeholder: Return sample code actions
  -- In Elm, this could provide "Add import", "Generate function", "Extract variable", etc.
  let sampleCodeActions =
        [ 
          -- CodeAction
          --   { codeActionTitle = "Add missing import",
          --     codeActionKind = Just "quickfix",
          --     codeActionDiagnostics = Nothing,
          --     codeActionIsPreferred = Just True,
          --     codeActionDisabled = Nothing,
          --     codeActionEdit = Nothing, -- Would contain the actual edit
          --     codeActionCommand = Just $ JSON.object
          --       [ "title" .= ("Add import" :: Text),
          --         "command" .= ("elm.addImport" :: Text),
          --         "arguments" .= ([] :: [JSON.Value])
          --       ],
          --     codeActionData = Nothing
          --   },
          -- CodeAction
          --   { codeActionTitle = "Extract to function",
          --     codeActionKind = Just "refactor.extract",
          --     codeActionDiagnostics = Nothing,
          --     codeActionIsPreferred = Just False,
          --     codeActionDisabled = Nothing,
          --     codeActionEdit = Nothing, -- Would contain the actual edit
          --     codeActionCommand = Just $ JSON.object
          --       [ "title" .= ("Extract function" :: Text),
          --         "command" .= ("elm.extractFunction" :: Text),
          --         "arguments" .= ([] :: [JSON.Value])
          --       ],
          --     codeActionData = Nothing
          --   }
        ]
  return $ Right sampleCodeActions

handleCodeLensResolve :: Live.State -> CodeLensResolveParams -> IO (Either String CodeLens)
handleCodeLensResolve _state codeLensResolveParams = do
  -- Placeholder: Return the resolved code lens with additional information
  -- In a real implementation, this would add command details, tooltips, etc.
  let resolvedCodeLens = CodeLens
        { codeLensRange = codeLensResolveParamsRange codeLensResolveParams,
          codeLensCommand = case codeLensResolveParamsCommand codeLensResolveParams of
            Just cmd -> Just cmd
            Nothing -> Just $ JSON.object
              [ "title" .= ("Resolved: Show Details" :: Text),
                "command" .= ("elm.showDetails" :: Text),
                "arguments" .= ([] :: [JSON.Value])
              ],
          codeLensData = codeLensResolveParamsData codeLensResolveParams
        }
  return $ Right resolvedCodeLens

handleCodeActionResolve :: Live.State -> CodeActionResolveParams -> IO (Either String CodeAction)
handleCodeActionResolve _state codeActionResolveParams = do
  -- Placeholder: Return the resolved code action with additional information
  -- In a real implementation, this would add workspace edits, additional data, etc.
  let resolvedCodeAction = CodeAction
        { codeActionTitle = codeActionResolveParamsTitle codeActionResolveParams <> " (Resolved)",
          codeActionKind = codeActionResolveParamsKind codeActionResolveParams,
          codeActionDiagnostics = codeActionResolveParamsDiagnostics codeActionResolveParams,
          codeActionIsPreferred = codeActionResolveParamsIsPreferred codeActionResolveParams,
          codeActionDisabled = codeActionResolveParamsDisabled codeActionResolveParams,
          codeActionEdit = case codeActionResolveParamsEdit codeActionResolveParams of
            Just edit -> Just edit
            Nothing -> Just $ WorkspaceEdit
              { workspaceEditChanges = Just $ JSON.object [],
                workspaceEditDocumentChanges = Nothing
              },
          codeActionCommand = codeActionResolveParamsCommand codeActionResolveParams,
          codeActionData = codeActionResolveParamsData codeActionResolveParams
        }
  return $ Right resolvedCodeAction

handleSelectionRange :: Live.State -> SelectionRangeParams -> IO (Either String [SelectionRange])
handleSelectionRange _state selectionRangeParams = do
  -- For now, return a simple selection range for each position
  -- In a real implementation, this would analyze the Elm syntax tree to provide
  -- smart selection ranges (e.g., expand from identifier to expression to statement)
  let positions = selectionRangeParamsPositions selectionRangeParams
      selectionRanges = map createSelectionRange positions
  return $ Right selectionRanges
  where
    createSelectionRange pos = 
      SelectionRange
        { selectionRangeRange = Range
            { rangeStart = pos,
              rangeEnd = Position (positionLine pos) (positionCharacter pos + 10)
            },
          selectionRangeParent = Just $ SelectionRange
            { selectionRangeRange = Range
                { rangeStart = Position (positionLine pos) 0,
                  rangeEnd = Position (positionLine pos + 1) 0
                },
              selectionRangeParent = Nothing
            }
        }

handleDidChangeVisibleRanges :: Live.State -> DidChangeVisibleRangesParams -> IO ()
handleDidChangeVisibleRanges _state visibleRangesParams = do
  -- Handle document visible ranges change notification
  let doc = didChangeVisibleRangesParamsTextDocument visibleRangesParams
      uri = textDocumentIdentifierUri doc
      ranges = didChangeVisibleRangesParamsVisibleRanges visibleRangesParams
  
  Ext.Log.log Ext.Log.LSP $ "textDocument/didChangeVisibleRanges - URI: " ++ Text.unpack uri
  Ext.Log.log Ext.Log.LSP $ "Visible ranges count: " ++ show (length ranges)
  mapM_ (\(i, range) -> do
    Ext.Log.log Ext.Log.LSP $ "Range " ++ show i ++ ": " ++ 
      "start: " ++ show (rangeStart range) ++ ", " ++
      "end: " ++ show (rangeEnd range)
    ) (zip [1..] ranges)

-- * Main LSP Server Handler

-- | Main LSP server handler
serve :: Live.State -> JSONRPC.EventEmitter -> JSONRPC.ConnectionId -> JSONRPC.Request -> IO (Either JSONRPC.Error JSONRPC.Response)
serve state _emit connId req@(JSONRPC.Request _ reqId method params) = do
  
  -- case params of
  --   Just p -> Ext.Log.log Ext.Log.LSP $ "REQ: " ++Text.unpack method ++ " (id: " ++ show reqId ++ "): " ++ (Text.unpack . Data.Text.Encoding.decodeUtf8 . LBS.toStrict . Data.Aeson.Encode.Pretty.encodePretty $ p)
  --   Nothing -> Ext.Log.log Ext.Log.LSP $ "REQ: " ++Text.unpack method ++ " (id: " ++ show reqId ++ ") (no params)" 

  Ext.Log.log Ext.Log.LSP $ "REQ: " ++Text.unpack method ++ " (id: " ++ show reqId ++ ")" 
  
  case Text.unpack method of
    -- Lifecycle
    "initialize" -> 
      handleLSPRequest reqId params "initialize" (\p -> do
        res <- handleInitialize state p
        case res of
          Right _ -> do
            -- Inform the client the server is ready
            -- _emit (showMessage LogMessageTypeInfo "Elm Dev LSP ready")
            return res
          Left _ -> return res
        )
        
    "initialized" -> do
      -- Client finished initialization
      return $ success reqId JSON.Null
      
    "shutdown" -> do
      -- Server shutdown request
      return $ success reqId JSON.Null
      
    -- Text Document Synchronization is handled as notifications, not requests
    -- These handlers were moved to handleNotification in Run.hs
        
    -- Language Features
    "textDocument/hover" -> 
      handleLSPRequest reqId params "hover" (handleHover state)
        
    "textDocument/completion" -> 
      handleLSPRequest reqId params "completion" (handleCompletion state)
        
    "textDocument/definition" -> 
      handleLSPRequest reqId params "definition" (handleDefinition state)

    -- Declaration Provider: Navigate to symbol declarations (where symbols are first introduced)
    -- Provides "Go to Declaration" functionality in editors
    -- https://code.visualstudio.com/api/language-extensions/programmatic-language-features#show-definitions-of-a-symbol
    "textDocument/declaration" -> 
      handleLSPRequest reqId params "declaration" (handleDeclaration state)

    -- Type Definition Provider: Navigate to type definitions
    -- Provides "Go to Type Definition" - useful for navigating from variables to their type definitions
    -- https://code.visualstudio.com/api/language-extensions/programmatic-language-features#show-definitions-of-a-symbol
    "textDocument/typeDefinition" -> 
      handleLSPRequest reqId params "typeDefinition" (handleTypeDefinition state)

    -- References Provider: Find all references to a symbol
    -- Provides "Find All References" functionality showing everywhere a symbol is used
    -- https://code.visualstudio.com/api/language-extensions/programmatic-language-features#find-all-references-to-a-symbol
    "textDocument/references" -> 
      handleLSPRequest reqId params "references" (handleReferences state)

    -- Implementation Provider: Navigate to implementations
    -- Provides "Go to Implementation" - useful for abstract interfaces to concrete implementations
    -- https://code.visualstudio.com/api/language-extensions/programmatic-language-features#show-implementations-of-a-symbol
    "textDocument/implementation" -> 
      handleLSPRequest reqId params "implementation" (handleImplementation state)

    -- Rename Provider: Rename symbols across the workspace
    -- Provides "Rename Symbol" functionality with workspace-wide refactoring
    -- https://code.visualstudio.com/api/language-extensions/programmatic-language-features#rename-symbols
    "textDocument/rename" -> 
      handleLSPRequest reqId params "rename" (handleRename state)

    -- Prepare Rename: Validate that a symbol can be renamed and provide the range
    -- Called before the actual rename to validate and show the range that will be renamed
    "textDocument/prepareRename" -> 
      handleLSPRequest reqId params "prepareRename" (handlePrepareRename state)

    -- Type Hierarchy Provider: Show type inheritance relationships
    -- Provides type hierarchy views showing parent/child type relationships
    -- https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_prepareTypeHierarchy
    "textDocument/prepareTypeHierarchy" -> 
      handleLSPRequest reqId params "prepareTypeHierarchy" (handlePrepareTypeHierarchy state)

    "typeHierarchy/supertypes" -> do
      -- Show parent types in the hierarchy (placeholder)
      return $ success reqId (JSON.toJSON ([] :: [TypeHierarchyItem]))

    "typeHierarchy/subtypes" -> do
      -- Show child types in the hierarchy (placeholder)
      return $ success reqId (JSON.toJSON ([] :: [TypeHierarchyItem]))

    -- Diagnostic Provider: Provide errors, warnings, and informational messages
    -- Powers the "Problems" panel and inline error squiggles
    -- https://code.visualstudio.com/api/language-extensions/programmatic-language-features#provide-diagnostics
    "textDocument/diagnostic" -> 
      handleLSPRequest reqId params "diagnostic" (handleDiagnostic state)

    -- Inlay Hint Provider: Show inline type hints and parameter names
    -- Provides inline hints like type annotations and parameter names without modifying the source
    -- https://code.visualstudio.com/api/language-extensions/programmatic-language-features#inlay-hints
    "textDocument/inlayHint" -> 
      handleLSPRequest reqId params "inlayHint" (handleInlayHint state)

    -- Signature Help Provider: Show function signatures while typing
    -- Provides parameter info when typing function calls - shows parameter types and documentation
    -- https://code.visualstudio.com/api/language-extensions/programmatic-language-features#help-with-function-and-method-signatures
    "textDocument/signatureHelp" -> 
      handleLSPRequest reqId params "signatureHelp" (handleSignatureHelp state)

    -- Document Formatting Provider: Format the entire file
    -- Returns a list of TextEdits; typically a single full-document replacement
    "textDocument/formatting" ->
      handleLSPRequest reqId params "documentFormatting" (handleDocumentFormatting state)

    -- Code Lens Provider: Show inline actionable information
    -- Provides clickable links above code elements (e.g., "run test", "X references")
    -- https://code.visualstudio.com/api/language-extensions/programmatic-language-features#codelens-show-actionable-context-information-within-source-code
    "textDocument/codeLens" -> 
      handleLSPRequest reqId params "codeLens" (handleCodeLens state)

    "codeLens/resolve" -> 
      handleLSPRequest reqId params "codeLensResolve" (handleCodeLensResolve state)

    -- Code Action Provider: Quick fixes and refactoring actions
    -- Provides light bulb menu with fixes like "Add missing import", "Extract function", etc.
    -- https://code.visualstudio.com/api/language-extensions/programmatic-language-features#code-actions-quick-fixes-and-refactorings
    "textDocument/codeAction" -> 
      handleLSPRequest reqId params "codeAction" (handleCodeAction state)

    "codeAction/resolve" -> 
      handleLSPRequest reqId params "codeActionResolve" (handleCodeActionResolve state)

    -- Selection Range Provider: Smart selection expansion
    -- Provides "Expand Selection" functionality that intelligently selects increasingly larger syntactic constructs
    -- https://code.visualstudio.com/api/language-extensions/programmatic-language-features#smart-selection
    "textDocument/selectionRange" -> 
      handleLSPRequest reqId params "selectionRange" (handleSelectionRange state)

    -- Legacy test methods for compatibility
    "ping" -> do
      return $ success reqId (JSON.String "pong")
    "echo" -> do
      let result = case params of
            Just p -> p
            Nothing -> JSON.Null
      return $ success reqId result
      
    _ -> do
      return $ Left (JSONRPC.errorMethodNotFound reqId method)

-- | Handle LSP notifications (no response expected)
handleNotification :: Live.State -> JSONRPC.EventEmitter -> JSONRPC.ConnectionId -> JSONRPC.Notification -> IO ()
handleNotification state send connId (JSONRPC.Notification _ method params) = do

  -- case params of
  --   Just p -> Ext.Log.log Ext.Log.LSP $ "NOTIF: " ++Text.unpack method ++ " " ++ (Text.unpack . Data.Text.Encoding.decodeUtf8 . LBS.toStrict . Data.Aeson.Encode.Pretty.encodePretty $ p)
  --   Nothing -> Ext.Log.log Ext.Log.LSP $ "NOTIF: " ++Text.unpack method ++ "(no params)" 

  Ext.Log.log Ext.Log.LSP $ "NOTIF: " ++ Text.unpack method

  case T.unpack method of
    "textDocument/didOpen" -> do
      case params of
        Just p -> do
          case JSON.fromJSON p of
            JSON.Success openParams -> do
              _ <- handleDidOpen state openParams
              send (logMessage LogMessageTypeInfo "Opened document")
              return ()
            JSON.Error err -> do
              Ext.Log.log Ext.Log.LSP $ "Failed to parse didOpen params: " ++ err
        Nothing -> do
          Ext.Log.log Ext.Log.LSP "didOpen notification missing parameters"
          
    "textDocument/didChange" -> do
      case params of
        Just p -> do
          case JSON.fromJSON p of
            JSON.Success changeParams -> do
              _ <- handleDidChange state changeParams
              -- Emit code lens refresh; diagnostics are provided via pull (DocumentDiagnostic)
              send (JSONRPC.OutboundRequest "workspace/codeLens/refresh" Nothing)
              send (logMessage LogMessageTypeLog "Document changed")
              return ()
            JSON.Error err -> do
              Ext.Log.log Ext.Log.LSP $ "Failed to parse didChange params: " ++ err
        Nothing -> do
          Ext.Log.log Ext.Log.LSP "didChange notification missing parameters"
          
    "textDocument/didClose" -> do
      case params of
        Just p -> do
          case JSON.fromJSON p of
            JSON.Success closeParams -> do
              _ <- handleDidClose state closeParams
              send (logMessage LogMessageTypeInfo "Closed document")
              return ()
            JSON.Error err -> do
              Ext.Log.log Ext.Log.LSP $ "Failed to parse didClose params: " ++ err
        Nothing -> do
          Ext.Log.log Ext.Log.LSP "didClose notification missing parameters"
          
    "textDocument/didSave" -> do
      case params of
        Just p -> do
          case JSON.fromJSON p of
            JSON.Success saveParams -> do
              _ <- handleDidSave state saveParams
              -- Emit code lens refresh; diagnostics are provided via pull (DocumentDiagnostic)
              send (JSONRPC.OutboundRequest "workspace/codeLens/refresh" Nothing)
              send (logMessage LogMessageTypeInfo "Saved document")
              return ()
            JSON.Error err -> do
              Ext.Log.log Ext.Log.LSP $ "Failed to parse didSave params: " ++ err
        Nothing -> do
          Ext.Log.log Ext.Log.LSP "didSave notification missing parameters"
          
    "initialized" -> do
      Ext.Log.log Ext.Log.LSP "LSP: Client finished initialization"

    "textDocument/didChangeVisibleRanges" -> do
      -- Note, this is not in the spec,
      -- And VSCode doesn't seem to send it either.
      -- But the AI keeps saying it's here.
      -- Don't need it for the moment, so we'll just ignore it.
      case params of
        Just p -> do
          case JSON.fromJSON p of
            JSON.Success visibleRangesParams -> do
              handleDidChangeVisibleRanges state visibleRangesParams
            JSON.Error err -> do
              Ext.Log.log Ext.Log.LSP $ "Failed to parse didChangeVisibleRanges params: " ++ err
        Nothing -> do
          Ext.Log.log Ext.Log.LSP "didChangeVisibleRanges notification missing parameters"
      
    _ -> do
      Ext.Log.log Ext.Log.LSP $ "LSP: Unhandled notification: " ++ T.unpack method



