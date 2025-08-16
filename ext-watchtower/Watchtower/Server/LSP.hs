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
import Data.Aeson ((.=))
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Encode.Pretty
import qualified Data.Text as T
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.TH
import qualified Data.ByteString.Lazy as LBS
import Data.Char (toLower)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding
import qualified Ext.Common
import qualified Ext.Dev
import qualified Ext.FileCache
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
import System.IO (hPutStrLn, stderr, hFlush)
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

-- * Core LSP Types

-- | Position in a text document (zero-based)
data Position = Position
  { positionLine :: Int,
    positionCharacter :: Int
  }
  deriving stock (Show, Eq, Generic)

-- | Range in a text document
data Range = Range
  { rangeStart :: Position,
    rangeEnd :: Position
  }
  deriving stock (Show, Eq, Generic)

-- | Location in a text document
data Location = Location
  { locationUri :: Text,
    locationRange :: Range
  }
  deriving stock (Show, Eq, Generic)

-- | Text document identifier
data TextDocumentIdentifier = TextDocumentIdentifier
  { textDocumentIdentifierUri :: Text
  }
  deriving stock (Show, Eq, Generic)

-- | Versioned text document identifier
data VersionedTextDocumentIdentifier = VersionedTextDocumentIdentifier
  { versionedTextDocumentIdentifierUri :: Text,
    versionedTextDocumentIdentifierVersion :: Maybe Int
  }
  deriving stock (Show, Eq, Generic)

-- | Text document item
data TextDocumentItem = TextDocumentItem
  { textDocumentItemUri :: Text,
    textDocumentItemLanguageId :: Text,
    textDocumentItemVersion :: Int,
    textDocumentItemText :: Text
  }
  deriving stock (Show, Eq, Generic)

-- | Content change in a text document
data TextDocumentContentChangeEvent = TextDocumentContentChangeEvent
  { textDocumentContentChangeEventRange :: Maybe Range,
    textDocumentContentChangeEventRangeLength :: Maybe Int,
    textDocumentContentChangeEventText :: Text
  }
  deriving stock (Show, Eq, Generic)

-- * Initialization Types

-- | Workspace folder
data WorkspaceFolder = WorkspaceFolder
  { workspaceFolderUri :: Text,
    workspaceFolderName :: Text
  }
  deriving stock (Show, Eq, Generic)

-- | Client capabilities
data ClientCapabilities = ClientCapabilities
  { clientCapabilitiesWorkspace :: Maybe JSON.Value,
    clientCapabilitiesTextDocument :: Maybe JSON.Value,
    clientCapabilitiesWindow :: Maybe JSON.Value,
    clientCapabilitiesGeneral :: Maybe JSON.Value,
    clientCapabilitiesExperimental :: Maybe JSON.Value
  }
  deriving stock (Show, Eq, Generic)

-- | Initialize parameters
data InitializeParams = InitializeParams
  { initializeParamsProcessId :: Maybe Int,
    initializeParamsClientInfo :: Maybe JSON.Value,
    initializeParamsLocale :: Maybe Text,
    initializeParamsRootPath :: Maybe Text,
    initializeParamsRootUri :: Maybe Text,
    initializeParamsCapabilities :: ClientCapabilities,
    initializeParamsInitializationOptions :: Maybe JSON.Value,
    initializeParamsTrace :: Maybe Text,
    initializeParamsWorkspaceFolders :: Maybe [WorkspaceFolder]
  }
  deriving stock (Show, Eq, Generic)

-- | Text document synchronization kind
-- 
-- Determines what document change notifications the server will receive
-- and how much content is included in those notifications.
data TextDocumentSyncKind
  = TextDocumentSyncNone 
    -- ^ No synchronization - server receives no document change notifications.
    -- The server will not be notified when documents are modified.
    -- Only textDocument/didOpen and textDocument/didClose are sent.
  | TextDocumentSyncFull 
    -- ^ Full document synchronization - server receives complete document content.
    -- When a document changes, textDocument/didChange notifications contain:
    -- - The entire document text in contentChanges[0].text
    -- - No range information (contentChanges[0].range is undefined)
    -- - Simple but potentially inefficient for large documents
  | TextDocumentSyncIncremental 
    -- ^ Incremental document synchronization - server receives only changed parts.
    -- When a document changes, textDocument/didChange notifications contain:
    -- - Only the modified text ranges in contentChanges[].text  
    -- - Precise range information in contentChanges[].range
    -- - Multiple changes can be batched in a single notification
    -- - Efficient for large documents, requires more complex handling
  deriving stock (Show, Eq, Generic)

instance JSON.ToJSON TextDocumentSyncKind where
  toJSON TextDocumentSyncNone = JSON.Number 0
  toJSON TextDocumentSyncFull = JSON.Number 1
  toJSON TextDocumentSyncIncremental = JSON.Number 2

instance JSON.FromJSON TextDocumentSyncKind where
  parseJSON (JSON.Number 0) = pure TextDocumentSyncNone
  parseJSON (JSON.Number 1) = pure TextDocumentSyncFull
  parseJSON (JSON.Number 2) = pure TextDocumentSyncIncremental
  parseJSON _ = fail "Invalid TextDocumentSyncKind"

-- | Save options for text document synchronization
--
-- Controls the content of textDocument/didSave notifications:
--
-- When includeText = True:
--   - didSave notification includes the full document text
--   - Useful if server needs the complete content after save
--
-- When includeText = False:
--   - didSave notification only includes the document URI and version
--   - More efficient when server only needs to know that a save occurred
data SaveOptions = SaveOptions
  { saveOptionsIncludeText :: Bool -- ^ Whether to include document text in save notifications
  }
  deriving stock (Show, Eq, Generic)

instance JSON.ToJSON SaveOptions where
  toJSON (SaveOptions includeText) = JSON.object
    [ "includeText" .= includeText ]

instance JSON.FromJSON SaveOptions where
  parseJSON = JSON.withObject "SaveOptions" $ \o -> SaveOptions
    <$> o JSON..: "includeText"

-- | Text document synchronization options
--
-- Configures which document lifecycle notifications the server wants to receive.
-- Each field controls a different type of notification:
--
-- When textDocumentSyncOpenClose = True:
--   - textDocument/didOpen: sent when a document is opened in the editor
--   - textDocument/didClose: sent when a document is closed in the editor
--
-- When textDocumentSyncChange is set:
--   - textDocument/didChange: sent when document content is modified
--   - The sync kind determines the format of change notifications (see TextDocumentSyncKind)
--
-- When textDocumentSyncSave is provided:
--   - textDocument/didSave: sent when a document is saved
--   - SaveOptions.includeText determines if the full document text is included
data TextDocumentSyncOptions = TextDocumentSyncOptions
  { textDocumentSyncOpenClose :: Bool, -- ^ Whether to send open/close notifications
    textDocumentSyncChange :: TextDocumentSyncKind, -- ^ How to send change notifications  
    textDocumentSyncSave :: Maybe SaveOptions -- ^ Whether to send save notifications (optional)
  }
  deriving stock (Show, Eq, Generic)

instance JSON.ToJSON TextDocumentSyncOptions where
  toJSON (TextDocumentSyncOptions openClose change save) = JSON.object
    [ "openClose" .= openClose
    , "change" .= change
    , "save" .= save
    ]

instance JSON.FromJSON TextDocumentSyncOptions where
  parseJSON = JSON.withObject "TextDocumentSyncOptions" $ \o -> TextDocumentSyncOptions
    <$> o JSON..: "openClose"
    <*> o JSON..: "change"
    <*> o JSON..:? "save"

-- | Server capabilities
data ServerCapabilities = ServerCapabilities
  { serverCapabilitiesPositionEncoding :: Maybe Text,
    serverCapabilitiesTextDocumentSync :: Maybe TextDocumentSyncOptions,
    serverCapabilitiesNotebookDocumentSync :: Maybe JSON.Value,
    serverCapabilitiesCompletionProvider :: Maybe JSON.Value,
    serverCapabilitiesHoverProvider :: Maybe Bool,
    serverCapabilitiesSignatureHelpProvider :: Maybe JSON.Value,
    serverCapabilitiesDeclarationProvider :: Maybe Bool,
    serverCapabilitiesDefinitionProvider :: Maybe Bool,
    serverCapabilitiesTypeDefinitionProvider :: Maybe Bool,
    serverCapabilitiesImplementationProvider :: Maybe Bool,
    serverCapabilitiesReferencesProvider :: Maybe Bool,
    serverCapabilitiesDocumentHighlightProvider :: Maybe Bool,
    serverCapabilitiesDocumentSymbolProvider :: Maybe Bool,
    serverCapabilitiesCodeActionProvider :: Maybe JSON.Value,
    serverCapabilitiesCodeLensProvider :: Maybe JSON.Value,
    serverCapabilitiesDocumentLinkProvider :: Maybe JSON.Value,
    serverCapabilitiesColorProvider :: Maybe JSON.Value,
    serverCapabilitiesDocumentFormattingProvider :: Maybe Bool,
    serverCapabilitiesDocumentRangeFormattingProvider :: Maybe Bool,
    serverCapabilitiesDocumentOnTypeFormattingProvider :: Maybe JSON.Value,
    serverCapabilitiesRenameProvider :: Maybe JSON.Value,
    serverCapabilitiesFoldingRangeProvider :: Maybe Bool,
    serverCapabilitiesExecuteCommandProvider :: Maybe JSON.Value,
    serverCapabilitiesSelectionRangeProvider :: Maybe Bool,
    serverCapabilitiesLinkedEditingRangeProvider :: Maybe Bool,
    serverCapabilitiesCallHierarchyProvider :: Maybe Bool,
    serverCapabilitiesSemanticTokensProvider :: Maybe JSON.Value,
    serverCapabilitiesMonikerProvider :: Maybe Bool,
    serverCapabilitiesTypeHierarchyProvider :: Maybe Bool,
    serverCapabilitiesInlineValueProvider :: Maybe Bool,
    serverCapabilitiesInlayHintProvider :: Maybe JSON.Value,
    serverCapabilitiesDiagnosticProvider :: Maybe JSON.Value,
    serverCapabilitiesWorkspaceSymbolProvider :: Maybe Bool,
    serverCapabilitiesWorkspace :: Maybe JSON.Value,
    serverCapabilitiesExperimental :: Maybe JSON.Value
  }
  deriving stock (Show, Eq, Generic)

-- | Server info
data ServerInfo = ServerInfo
  { serverInfoName :: Text,
    serverInfoVersion :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

-- | Initialize result
data InitializeResult = InitializeResult
  { initializeResultCapabilities :: ServerCapabilities,
    initializeResultServerInfo :: Maybe ServerInfo
  }
  deriving stock (Show, Eq, Generic)

-- * Text Document Synchronization Types

-- | Document open notification parameters
data DidOpenTextDocumentParams = DidOpenTextDocumentParams
  { didOpenTextDocumentParamsTextDocument :: TextDocumentItem
  }
  deriving stock (Show, Eq, Generic)

-- | Document change notification parameters
data DidChangeTextDocumentParams = DidChangeTextDocumentParams
  { didChangeTextDocumentParamsTextDocument :: VersionedTextDocumentIdentifier,
    didChangeTextDocumentParamsContentChanges :: [TextDocumentContentChangeEvent]
  }
  deriving stock (Show, Eq, Generic)

-- | Document close notification parameters
data DidCloseTextDocumentParams = DidCloseTextDocumentParams
  { didCloseTextDocumentParamsTextDocument :: TextDocumentIdentifier
  }
  deriving stock (Show, Eq, Generic)

-- | Document save notification parameters
data DidSaveTextDocumentParams = DidSaveTextDocumentParams
  { didSaveTextDocumentParamsTextDocument :: TextDocumentIdentifier,
    didSaveTextDocumentParamsText :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

-- * Language Feature Types

-- | Hover request parameters
data HoverParams = HoverParams
  { hoverParamsTextDocument :: TextDocumentIdentifier,
    hoverParamsPosition :: Position,
    hoverParamsWorkDoneToken :: Maybe JSON.Value
  }
  deriving stock (Show, Eq, Generic)

-- | Markup content
data MarkupContent = MarkupContent
  { markupContentKind :: Text, -- "plaintext" or "markdown"
    markupContentValue :: Text
  }
  deriving stock (Show, Eq, Generic)

-- | Hover response
data Hover = Hover
  { hoverContents :: MarkupContent,
    hoverRange :: Maybe Range
  }
  deriving stock (Show, Eq, Generic)

-- | Completion request parameters
data CompletionParams = CompletionParams
  { completionParamsTextDocument :: TextDocumentIdentifier,
    completionParamsPosition :: Position,
    completionParamsWorkDoneToken :: Maybe JSON.Value,
    completionParamsPartialResultToken :: Maybe JSON.Value,
    completionParamsContext :: Maybe JSON.Value
  }
  deriving stock (Show, Eq, Generic)

-- | Completion item
data CompletionItem = CompletionItem
  { completionItemLabel :: Text,
    completionItemLabelDetails :: Maybe JSON.Value,
    completionItemKind :: Maybe Int,
    completionItemTags :: Maybe [Int],
    completionItemDetail :: Maybe Text,
    completionItemDocumentation :: Maybe JSON.Value,
    completionItemDeprecated :: Maybe Bool,
    completionItemPreselect :: Maybe Bool,
    completionItemSortText :: Maybe Text,
    completionItemFilterText :: Maybe Text,
    completionItemInsertText :: Maybe Text,
    completionItemInsertTextFormat :: Maybe Int,
    completionItemInsertTextMode :: Maybe Int,
    completionItemTextEdit :: Maybe JSON.Value,
    completionItemAdditionalTextEdits :: Maybe [JSON.Value],
    completionItemCommitCharacters :: Maybe [Text],
    completionItemCommand :: Maybe JSON.Value,
    completionItemData :: Maybe JSON.Value
  }
  deriving stock (Show, Eq, Generic)

-- | Completion list
data CompletionList = CompletionList
  { completionListIsIncomplete :: Bool,
    completionListItemDefaults :: Maybe JSON.Value,
    completionListItems :: [CompletionItem]
  }
  deriving stock (Show, Eq, Generic)

-- | Definition request parameters
data DefinitionParams = DefinitionParams
  { definitionParamsTextDocument :: TextDocumentIdentifier,
    definitionParamsPosition :: Position,
    definitionParamsWorkDoneToken :: Maybe JSON.Value,
    definitionParamsPartialResultToken :: Maybe JSON.Value
  }
  deriving stock (Show, Eq, Generic)

-- | References request parameters
data ReferenceParams = ReferenceParams
  { referenceParamsTextDocument :: TextDocumentIdentifier,
    referenceParamsPosition :: Position,
    referenceParamsWorkDoneToken :: Maybe JSON.Value,
    referenceParamsPartialResultToken :: Maybe JSON.Value,
    referenceParamsContext :: JSON.Value -- Contains includeDeclaration boolean
  }
  deriving stock (Show, Eq, Generic)

-- | Rename request parameters
data RenameParams = RenameParams
  { renameParamsTextDocument :: TextDocumentIdentifier,
    renameParamsPosition :: Position,
    renameParamsWorkDoneToken :: Maybe JSON.Value,
    renameParamsNewName :: Text
  }
  deriving stock (Show, Eq, Generic)

-- | Prepare rename request parameters
data PrepareRenameParams = PrepareRenameParams
  { prepareRenameParamsTextDocument :: TextDocumentIdentifier,
    prepareRenameParamsPosition :: Position,
    prepareRenameParamsWorkDoneToken :: Maybe JSON.Value
  }
  deriving stock (Show, Eq, Generic)

-- | Code action request parameters
data CodeActionParams = CodeActionParams
  { codeActionParamsTextDocument :: TextDocumentIdentifier,
    codeActionParamsRange :: Range,
    codeActionParamsWorkDoneToken :: Maybe JSON.Value,
    codeActionParamsPartialResultToken :: Maybe JSON.Value,
    codeActionParamsContext :: JSON.Value -- Contains diagnostics and only array
  }
  deriving stock (Show, Eq, Generic)

-- | Code lens request parameters
data CodeLensParams = CodeLensParams
  { codeLensParamsTextDocument :: TextDocumentIdentifier,
    codeLensParamsWorkDoneToken :: Maybe JSON.Value,
    codeLensParamsPartialResultToken :: Maybe JSON.Value
  }
  deriving stock (Show, Eq, Generic)

-- | Signature help request parameters
data SignatureHelpParams = SignatureHelpParams
  { signatureHelpParamsTextDocument :: TextDocumentIdentifier,
    signatureHelpParamsPosition :: Position,
    signatureHelpParamsWorkDoneToken :: Maybe JSON.Value,
    signatureHelpParamsContext :: Maybe JSON.Value
  }
  deriving stock (Show, Eq, Generic)

-- | Inlay hint request parameters
data InlayHintParams = InlayHintParams
  { inlayHintParamsTextDocument :: TextDocumentIdentifier,
    inlayHintParamsRange :: Range,
    inlayHintParamsWorkDoneToken :: Maybe JSON.Value
  }
  deriving stock (Show, Eq, Generic)

-- | Type hierarchy prepare request parameters
data TypeHierarchyPrepareParams = TypeHierarchyPrepareParams
  { typeHierarchyPrepareParamsTextDocument :: TextDocumentIdentifier,
    typeHierarchyPrepareParamsPosition :: Position,
    typeHierarchyPrepareParamsWorkDoneToken :: Maybe JSON.Value
  }
  deriving stock (Show, Eq, Generic)

-- | Document diagnostic request parameters
data DocumentDiagnosticParams = DocumentDiagnosticParams
  { documentDiagnosticParamsTextDocument :: TextDocumentIdentifier,
    documentDiagnosticParamsWorkDoneToken :: Maybe JSON.Value,
    documentDiagnosticParamsPartialResultToken :: Maybe JSON.Value,
    documentDiagnosticParamsPreviousResultId :: Maybe Text,
    documentDiagnosticParamsIdentifier :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

-- | Code lens resolve parameters (the original code lens to resolve)
data CodeLensResolveParams = CodeLensResolveParams
  { codeLensResolveParamsRange :: Range,
    codeLensResolveParamsCommand :: Maybe JSON.Value,
    codeLensResolveParamsData :: Maybe JSON.Value
  }
  deriving stock (Show, Eq, Generic)

-- | Code action resolve parameters (the original code action to resolve)
data CodeActionResolveParams = CodeActionResolveParams
  { codeActionResolveParamsTitle :: Text,
    codeActionResolveParamsKind :: Maybe Text,
    codeActionResolveParamsDiagnostics :: Maybe [Diagnostic],
    codeActionResolveParamsIsPreferred :: Maybe Bool,
    codeActionResolveParamsDisabled :: Maybe JSON.Value,
    codeActionResolveParamsEdit :: Maybe WorkspaceEdit,
    codeActionResolveParamsCommand :: Maybe JSON.Value,
    codeActionResolveParamsData :: Maybe JSON.Value
  }
  deriving stock (Show, Eq, Generic)

-- | Diagnostic severity
data DiagnosticSeverity = DiagnosticSeverity Int
  deriving stock (Show, Eq, Generic)

-- | Diagnostic
data Diagnostic = Diagnostic
  { diagnosticRange :: Range,
    diagnosticSeverity :: Maybe DiagnosticSeverity,
    diagnosticCode :: Maybe JSON.Value,
    diagnosticCodeDescription :: Maybe JSON.Value,
    diagnosticSource :: Maybe Text,
    diagnosticMessage :: Text,
    diagnosticTags :: Maybe [Int],
    diagnosticRelatedInformation :: Maybe [JSON.Value],
    diagnosticData :: Maybe JSON.Value
  }
  deriving stock (Show, Eq, Generic)

-- | Publish diagnostics parameters
data PublishDiagnosticsParams = PublishDiagnosticsParams
  { publishDiagnosticsParamsUri :: Text,
    publishDiagnosticsParamsVersion :: Maybe Int,
    publishDiagnosticsParamsDiagnostics :: [Diagnostic]
  }
  deriving stock (Show, Eq, Generic)

-- * Response Types

-- | Workspace edit for rename operations
data WorkspaceEdit = WorkspaceEdit
  { workspaceEditChanges :: Maybe JSON.Value,
    workspaceEditDocumentChanges :: Maybe [JSON.Value]
  }
  deriving stock (Show, Eq, Generic)

-- | Prepare rename response with range
data PrepareRenameResponse = PrepareRenameResponse
  { prepareRenameResponseRange :: Range,
    prepareRenameResponsePlaceholder :: Text
  }
  deriving stock (Show, Eq, Generic)

-- | Code action response
data CodeAction = CodeAction
  { codeActionTitle :: Text,
    codeActionKind :: Maybe Text,
    codeActionDiagnostics :: Maybe [Diagnostic],
    codeActionIsPreferred :: Maybe Bool,
    codeActionDisabled :: Maybe JSON.Value,
    codeActionEdit :: Maybe WorkspaceEdit,
    codeActionCommand :: Maybe JSON.Value,
    codeActionData :: Maybe JSON.Value
  }
  deriving stock (Show, Eq, Generic)

-- | Code lens response
data CodeLens = CodeLens
  { codeLensRange :: Range,
    codeLensCommand :: Maybe JSON.Value,
    codeLensData :: Maybe JSON.Value
  }
  deriving stock (Show, Eq, Generic)

-- | Signature information
data SignatureInformation = SignatureInformation
  { signatureInformationLabel :: Text,
    signatureInformationDocumentation :: Maybe JSON.Value,
    signatureInformationParameters :: Maybe [JSON.Value],
    signatureInformationActiveParameter :: Maybe Int
  }
  deriving stock (Show, Eq, Generic)

-- | Signature help response
data SignatureHelp = SignatureHelp
  { signatureHelpSignatures :: [SignatureInformation],
    signatureHelpActiveSignature :: Maybe Int,
    signatureHelpActiveParameter :: Maybe Int
  }
  deriving stock (Show, Eq, Generic)

-- | Inlay hint response
data InlayHint = InlayHint
  { inlayHintPosition :: Position,
    inlayHintLabel :: JSON.Value, -- Can be string or InlayHintLabelPart[]
    inlayHintKind :: Maybe Int,
    inlayHintTextEdits :: Maybe [JSON.Value],
    inlayHintTooltip :: Maybe JSON.Value,
    inlayHintPaddingLeft :: Maybe Bool,
    inlayHintPaddingRight :: Maybe Bool,
    inlayHintData :: Maybe JSON.Value
  }
  deriving stock (Show, Eq, Generic)

-- | Type hierarchy item
data TypeHierarchyItem = TypeHierarchyItem
  { typeHierarchyItemName :: Text,
    typeHierarchyItemKind :: Int,
    typeHierarchyItemTags :: Maybe [Int],
    typeHierarchyItemDetail :: Maybe Text,
    typeHierarchyItemUri :: Text,
    typeHierarchyItemRange :: Range,
    typeHierarchyItemSelectionRange :: Range,
    typeHierarchyItemData :: Maybe JSON.Value
  }
  deriving stock (Show, Eq, Generic)

-- | Selection range request parameters
data SelectionRangeParams = SelectionRangeParams
  { selectionRangeParamsTextDocument :: TextDocumentIdentifier,
    selectionRangeParamsPositions :: [Position],
    selectionRangeParamsWorkDoneToken :: Maybe JSON.Value,
    selectionRangeParamsPartialResultToken :: Maybe JSON.Value
  }
  deriving stock (Show, Eq, Generic)

-- | Selection range response
data SelectionRange = SelectionRange
  { selectionRangeRange :: Range,
    selectionRangeParent :: Maybe SelectionRange
  }
  deriving stock (Show, Eq, Generic)

-- | Document visible ranges change notification parameters (VS Code extension)
data DidChangeVisibleRangesParams = DidChangeVisibleRangesParams
  { didChangeVisibleRangesParamsTextDocument :: TextDocumentIdentifier,
    didChangeVisibleRangesParamsVisibleRanges :: [Range]
  }
  deriving stock (Show, Eq, Generic)

-- * JSON Instances

-- Custom JSON instance for DiagnosticSeverity (must come before deriveJSON for Diagnostic)
instance JSON.ToJSON DiagnosticSeverity where
  toJSON (DiagnosticSeverity n) = JSON.toJSON n

instance JSON.FromJSON DiagnosticSeverity where
  parseJSON v = DiagnosticSeverity <$> JSON.parseJSON v

$(deriveJSON defaultOptions {fieldLabelModifier = Ext.Common.removePrefixAndDecapitalize "position"} ''Position)
$(deriveJSON defaultOptions {fieldLabelModifier = Ext.Common.removePrefixAndDecapitalize "range"} ''Range)
$(deriveJSON defaultOptions {fieldLabelModifier = Ext.Common.removePrefixAndDecapitalize "location"} ''Location)
$(deriveJSON defaultOptions {fieldLabelModifier = Ext.Common.removePrefixAndDecapitalize "textDocumentIdentifier"} ''TextDocumentIdentifier)
$(deriveJSON defaultOptions {fieldLabelModifier = Ext.Common.removePrefixAndDecapitalize "versionedTextDocumentIdentifier", omitNothingFields = True} ''VersionedTextDocumentIdentifier)
$(deriveJSON defaultOptions {fieldLabelModifier = Ext.Common.removePrefixAndDecapitalize "textDocumentItem"} ''TextDocumentItem)
$(deriveJSON defaultOptions {fieldLabelModifier = Ext.Common.removePrefixAndDecapitalize "textDocumentContentChangeEvent", omitNothingFields = True} ''TextDocumentContentChangeEvent)
$(deriveJSON defaultOptions {fieldLabelModifier = Ext.Common.removePrefixAndDecapitalize "workspaceFolder"} ''WorkspaceFolder)
$(deriveJSON defaultOptions {fieldLabelModifier = Ext.Common.removePrefixAndDecapitalize "clientCapabilities", omitNothingFields = True} ''ClientCapabilities)
$(deriveJSON defaultOptions {fieldLabelModifier = Ext.Common.removePrefixAndDecapitalize "initializeParams", omitNothingFields = True} ''InitializeParams)
$(deriveJSON defaultOptions {fieldLabelModifier = Ext.Common.removePrefixAndDecapitalize "serverCapabilities", omitNothingFields = True} ''ServerCapabilities)
$(deriveJSON defaultOptions {fieldLabelModifier = Ext.Common.removePrefixAndDecapitalize "serverInfo", omitNothingFields = True} ''ServerInfo)
$(deriveJSON defaultOptions {fieldLabelModifier = Ext.Common.removePrefixAndDecapitalize "initializeResult"} ''InitializeResult)
$(deriveJSON defaultOptions {fieldLabelModifier = Ext.Common.removePrefixAndDecapitalize "didOpenTextDocumentParams"} ''DidOpenTextDocumentParams)
$(deriveJSON defaultOptions {fieldLabelModifier = Ext.Common.removePrefixAndDecapitalize "didChangeTextDocumentParams"} ''DidChangeTextDocumentParams)
$(deriveJSON defaultOptions {fieldLabelModifier = Ext.Common.removePrefixAndDecapitalize "didCloseTextDocumentParams"} ''DidCloseTextDocumentParams)
$(deriveJSON defaultOptions {fieldLabelModifier = Ext.Common.removePrefixAndDecapitalize "didSaveTextDocumentParams", omitNothingFields = True} ''DidSaveTextDocumentParams)
$(deriveJSON defaultOptions {fieldLabelModifier = Ext.Common.removePrefixAndDecapitalize "hoverParams", omitNothingFields = True} ''HoverParams)
$(deriveJSON defaultOptions {fieldLabelModifier = Ext.Common.removePrefixAndDecapitalize "markupContent"} ''MarkupContent)
$(deriveJSON defaultOptions {fieldLabelModifier = Ext.Common.removePrefixAndDecapitalize "hover", omitNothingFields = True} ''Hover)
$(deriveJSON defaultOptions {fieldLabelModifier = Ext.Common.removePrefixAndDecapitalize "completionParams", omitNothingFields = True} ''CompletionParams)
$(deriveJSON defaultOptions {fieldLabelModifier = Ext.Common.removePrefixAndDecapitalize "completionItem", omitNothingFields = True} ''CompletionItem)
$(deriveJSON defaultOptions {fieldLabelModifier = Ext.Common.removePrefixAndDecapitalize "completionList", omitNothingFields = True} ''CompletionList)
$(deriveJSON defaultOptions {fieldLabelModifier = Ext.Common.removePrefixAndDecapitalize "definitionParams", omitNothingFields = True} ''DefinitionParams)
$(deriveJSON defaultOptions {fieldLabelModifier = Ext.Common.removePrefixAndDecapitalize "referenceParams", omitNothingFields = True} ''ReferenceParams)
$(deriveJSON defaultOptions {fieldLabelModifier = Ext.Common.removePrefixAndDecapitalize "renameParams", omitNothingFields = True} ''RenameParams)
$(deriveJSON defaultOptions {fieldLabelModifier = Ext.Common.removePrefixAndDecapitalize "prepareRenameParams", omitNothingFields = True} ''PrepareRenameParams)
$(deriveJSON defaultOptions {fieldLabelModifier = Ext.Common.removePrefixAndDecapitalize "codeActionParams", omitNothingFields = True} ''CodeActionParams)
$(deriveJSON defaultOptions {fieldLabelModifier = Ext.Common.removePrefixAndDecapitalize "codeLensParams", omitNothingFields = True} ''CodeLensParams)
$(deriveJSON defaultOptions {fieldLabelModifier = Ext.Common.removePrefixAndDecapitalize "signatureHelpParams", omitNothingFields = True} ''SignatureHelpParams)
$(deriveJSON defaultOptions {fieldLabelModifier = Ext.Common.removePrefixAndDecapitalize "inlayHintParams", omitNothingFields = True} ''InlayHintParams)
$(deriveJSON defaultOptions {fieldLabelModifier = Ext.Common.removePrefixAndDecapitalize "typeHierarchyPrepareParams", omitNothingFields = True} ''TypeHierarchyPrepareParams)
$(deriveJSON defaultOptions {fieldLabelModifier = Ext.Common.removePrefixAndDecapitalize "documentDiagnosticParams", omitNothingFields = True} ''DocumentDiagnosticParams)
$(deriveJSON defaultOptions {fieldLabelModifier = Ext.Common.removePrefixAndDecapitalize "codeLensResolveParams", omitNothingFields = True} ''CodeLensResolveParams)
$(deriveJSON defaultOptions {fieldLabelModifier = Ext.Common.removePrefixAndDecapitalize "diagnostic", omitNothingFields = True} ''Diagnostic)
$(deriveJSON defaultOptions {fieldLabelModifier = Ext.Common.removePrefixAndDecapitalize "workspaceEdit", omitNothingFields = True} ''WorkspaceEdit)
$(deriveJSON defaultOptions {fieldLabelModifier = Ext.Common.removePrefixAndDecapitalize "codeActionResolveParams", omitNothingFields = True} ''CodeActionResolveParams)
$(deriveJSON defaultOptions {fieldLabelModifier = Ext.Common.removePrefixAndDecapitalize "publishDiagnosticsParams", omitNothingFields = True} ''PublishDiagnosticsParams)
$(deriveJSON defaultOptions {fieldLabelModifier = Ext.Common.removePrefixAndDecapitalize "prepareRenameResponse"} ''PrepareRenameResponse)
$(deriveJSON defaultOptions {fieldLabelModifier = Ext.Common.removePrefixAndDecapitalize "codeAction", omitNothingFields = True} ''CodeAction)
$(deriveJSON defaultOptions {fieldLabelModifier = Ext.Common.removePrefixAndDecapitalize "codeLens", omitNothingFields = True} ''CodeLens)
$(deriveJSON defaultOptions {fieldLabelModifier = Ext.Common.removePrefixAndDecapitalize "signatureInformation", omitNothingFields = True} ''SignatureInformation)
$(deriveJSON defaultOptions {fieldLabelModifier = Ext.Common.removePrefixAndDecapitalize "signatureHelp", omitNothingFields = True} ''SignatureHelp)
$(deriveJSON defaultOptions {fieldLabelModifier = Ext.Common.removePrefixAndDecapitalize "inlayHint", omitNothingFields = True} ''InlayHint)
$(deriveJSON defaultOptions {fieldLabelModifier = Ext.Common.removePrefixAndDecapitalize "typeHierarchyItem", omitNothingFields = True} ''TypeHierarchyItem)
$(deriveJSON defaultOptions {fieldLabelModifier = Ext.Common.removePrefixAndDecapitalize "selectionRangeParams", omitNothingFields = True} ''SelectionRangeParams)
$(deriveJSON defaultOptions {fieldLabelModifier = Ext.Common.removePrefixAndDecapitalize "selectionRange", omitNothingFields = True} ''SelectionRange)
$(deriveJSON defaultOptions {fieldLabelModifier = Ext.Common.removePrefixAndDecapitalize "didChangeVisibleRangesParams"} ''DidChangeVisibleRangesParams)

-- * Helper Functions

success :: JSONRPC.RequestId -> JSON.Value -> Either JSONRPC.Error JSONRPC.Response
success reqId result =
  Right (JSONRPC.success reqId result)

err :: JSONRPC.RequestId -> String -> Either JSONRPC.Error JSONRPC.Response
err reqId str =
  Left (JSONRPC.err reqId (Text.pack str))

-- | Helper function to parse LSP request parameters
parseParams :: (JSON.FromJSON a) => JSONRPC.RequestId -> Maybe JSON.Value -> String -> Either JSONRPC.Error a
parseParams reqId maybeParams paramType =
  case maybeParams of
    Just p -> 
      case JSON.fromJSON p of
        JSON.Success parsed -> Right parsed
        JSON.Error e -> Left (JSONRPC.err reqId (Text.pack $ "Invalid " ++ paramType ++ " params: " ++ e))
    Nothing -> Left (JSONRPC.err reqId (Text.pack $ "Missing " ++ paramType ++ " parameters"))

-- | Helper function to handle LSP requests with parameter parsing
handleLSPRequest :: (JSON.FromJSON a, JSON.ToJSON b) => 
                   JSONRPC.RequestId -> 
                   Maybe JSON.Value -> 
                   String -> 
                   (a -> IO (Either String b)) -> 
                   IO (Either JSONRPC.Error JSONRPC.Response)
handleLSPRequest reqId params paramType handler = do
  -- hPutStrLn stderr $ "=== HANDLING " ++ paramType ++ " REQUEST ==="
  -- hFlush stderr
  
  case parseParams reqId params paramType of
    Right parsed -> do
      -- hPutStrLn stderr $ "Parameters parsed successfully for " ++ paramType
      -- hFlush stderr
      
      result <- handler parsed
      case result of
        Right response -> do
          -- hPutStrLn stderr $ "Handler returned success for " ++ paramType
          -- hFlush stderr
          return $ success reqId (JSON.toJSON response)
        Left errorMsg -> do
          -- hPutStrLn stderr $ "Handler returned error for " ++ paramType ++ ": " ++ errorMsg
          -- hFlush stderr
          return $ err reqId errorMsg
    Left jsonRpcError -> do
      -- hPutStrLn stderr $ "Parameter parsing failed for " ++ paramType ++ ": " ++ show jsonRpcError
      -- hFlush stderr
      return $ Left jsonRpcError

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
  -- Extract root path from initialization parameters
  let maybeRoot = case initializeParamsRootPath initParams of
        Just path -> Just (Text.unpack path)
        Nothing -> case initializeParamsRootUri initParams of
          Just uri -> Just (Text.unpack uri)
          Nothing -> Nothing
  
  -- Call discover if we have a root path
  case maybeRoot of
    Just root -> do
      Discover.discover state root
    Nothing -> do
      -- Log that no root was provided
      hPutStrLn stderr "LSP Initialize: No root path provided"
      hFlush stderr
  
  let initResult = InitializeResult
        { initializeResultCapabilities = defaultServerCapabilities,
          initializeResultServerInfo = Just $ ServerInfo
            { serverInfoName = "elm-dev-lsp",
              serverInfoVersion = Just "1.0.0"
            }
        }
  return $ Right initResult

handleDidOpen :: Live.State -> DidOpenTextDocumentParams -> IO (Either String JSON.Value)
handleDidOpen _state openParams = do
  -- Handle document open notification
  let doc = didOpenTextDocumentParamsTextDocument openParams
      uri = textDocumentItemUri doc
      languageId = textDocumentItemLanguageId doc
      version = textDocumentItemVersion doc
      text = textDocumentItemText doc
  
  hPutStrLn stderr $ "LSP: textDocument/didOpen - URI: " ++ Text.unpack uri
  hPutStrLn stderr $ "LSP: Language: " ++ Text.unpack languageId ++ ", Version: " ++ show version
  hPutStrLn stderr $ "LSP: Document length: " ++ show (Text.length text) ++ " characters"
  hFlush stderr
  
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
            Watchtower.Live.Compile.recompile state [filePath]
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
handleDidClose _state closeParams = do
  -- Handle document close notification
  let doc = didCloseTextDocumentParamsTextDocument closeParams
      uri = textDocumentIdentifierUri doc
  
  hPutStrLn stderr $ "LSP: textDocument/didClose - URI: " ++ Text.unpack uri
  hFlush stderr
  
  return $ Right JSON.Null

handleDidSave :: Live.State -> DidSaveTextDocumentParams -> IO (Either String JSON.Value)
handleDidSave _state saveParams = do
  -- Handle document save notification
  let doc = didSaveTextDocumentParamsTextDocument saveParams
      uri = textDocumentIdentifierUri doc
      text = didSaveTextDocumentParamsText saveParams
  
  hPutStrLn stderr $ "LSP: textDocument/didSave - URI: " ++ Text.unpack uri
  case text of
    Just content -> hPutStrLn stderr $ "LSP: Save with content, length: " ++ show (Text.length content)
    Nothing -> hPutStrLn stderr "LSP: Save without content"
  hFlush stderr
  
  return $ Right JSON.Null

handleHover :: Live.State -> HoverParams -> IO (Either String (Maybe Hover))
handleHover _state hoverParams = do
  -- For now, return a simple hover message
  -- In a real implementation, this would analyze the Elm code at the given position
  let pos = hoverParamsPosition hoverParams
      uri = textDocumentIdentifierUri (hoverParamsTextDocument hoverParams)
      
  -- Mock hover content
  let content = MarkupContent
        { markupContentKind = "markdown",
          markupContentValue = "**Elm Language Server**\n\nHover information at line " 
            <> Text.pack (show (positionLine pos)) 
            <> ", character " 
            <> Text.pack (show (positionCharacter pos))
        }
      
  return $ Right $ Just $ Hover
    { hoverContents = content,
      hoverRange = Nothing
    }

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
handleDefinition _state _definitionParams = do
  -- For now, return empty list
  -- In a real implementation, this would find the definition of the symbol at the given position
  return $ Right []

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

handleReferences :: Live.State -> ReferenceParams -> IO (Either String [Location])
handleReferences _state referenceParams = do
  -- Placeholder: Return sample reference locations
  -- In a real implementation, this would find all usages of a symbol
  let uri = textDocumentIdentifierUri (referenceParamsTextDocument referenceParams)
      pos = referenceParamsPosition referenceParams
      sampleLocations =
        [ Location
            { locationUri = uri,
              locationRange = Range
                { rangeStart = Position (positionLine pos + 5) 4,
                  rangeEnd = Position (positionLine pos + 5) 14
                }
            },
          Location
            { locationUri = uri,
              locationRange = Range
                { rangeStart = Position (positionLine pos + 10) 8,
                  rangeEnd = Position (positionLine pos + 10) 18
                }
            }
        ]
  return $ Right sampleLocations

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
  result <- case uriToFilePath uri of
    Nothing -> return $ Right []  -- Invalid URI, return empty diagnostics
    Just filePath -> do
      projectResult <- Watchtower.Live.Client.getExistingProject filePath state
      case projectResult of
        Left Watchtower.Live.Client.NoProjectsRegistered -> 
          return $ Right []  -- No projects loaded, return empty diagnostics
        Left (Watchtower.Live.Client.ProjectNotFound _) -> 
          return $ Right []  -- File not in any project, return empty diagnostics
        Right (projectCache, _) -> do
          -- Read the existing compile result from the project cache instead of re-running compile
          currentResult <- Control.Concurrent.STM.readTVarIO (Watchtower.Live.Client.compileResult projectCache)
          Ext.Log.log Ext.Log.LSP $ "READ COMPILE RESULT for " ++ Watchtower.Live.Client.getProjectRoot projectCache
          case currentResult of
            Watchtower.Live.Client.Success _ -> do
              Ext.Log.log Ext.Log.LSP "COMPILE RESULT SUCCESS, NO DIAGNOSTICS"
              return $ Right []
            Watchtower.Live.Client.Error (Watchtower.Live.Client.ReactorError exitReactor) -> do
              Ext.Log.log Ext.Log.LSP "COMPILE RESULT ERROR (Reactor), EXTRACTING DIAGNOSTICS"
              return $ Right $ extractDiagnosticsFromReactor uri exitReactor
            Watchtower.Live.Client.Error (Watchtower.Live.Client.GenerationError _) -> do
              Ext.Log.log Ext.Log.LSP "COMPILE RESULT ERROR (Generation), NO FILE-SPECIFIC DIAGNOSTICS"
              return $ Right []
            Watchtower.Live.Client.NotCompiled -> do
              Ext.Log.log Ext.Log.LSP "COMPILE RESULT NOT COMPILED YET, NO DIAGNOSTICS"
              return $ Right []

  case result of
    Right diagnostics -> do
      -- Also include per-file unused warnings (grayed/muted via Unnecessary tag)
      warnDiags <- case uriToFilePath uri of
        Just path -> do
          warns <- getWarningsForFile state path
          pure (warningsToDiagnostics warns)
        Nothing -> pure []
      let diagnosticsResponse = JSON.object
            [ "kind" .= ("full" :: Text),
              "items" .= (diagnostics ++ warnDiags)
            ]
      Ext.Log.log Ext.Log.LSP $ "Diagnostics: " ++ show diagnosticsResponse
      return $ Right diagnosticsResponse
    Left err -> do 
      Ext.Log.log Ext.Log.LSP $ "Diagnostics FAILURE: " ++ show err
      return $ Left err

-- | Extract diagnostics from Reactor errors, filtering by the requested file URI
extractDiagnosticsFromReactor :: Text -> Reporting.Exit.Reactor -> [Diagnostic]
extractDiagnosticsFromReactor uri reactor =
  case reactor of
    Reporting.Exit.ReactorBadBuild (Reporting.Exit.BuildBadModules _root firstModule otherModules) ->
      let allModules = firstModule : otherModules
          targetFilePath = uriToFilePath uri
      in concatMap (moduleErrorsToDiagnostics targetFilePath) allModules
    _ -> []  -- Other reactor errors are not module-specific

-- | Convert a module's errors to diagnostics if it matches the target file
moduleErrorsToDiagnostics :: Maybe FilePath -> Reporting.Error.Module -> [Diagnostic]
moduleErrorsToDiagnostics targetFilePath (Reporting.Error.Module _name absolutePath _time source errors) =
  case targetFilePath of
    Nothing -> []  -- Could not parse URI
    Just target -> 
      if System.FilePath.normalise absolutePath == System.FilePath.normalise target
        then errorToDiagnostics (Reporting.Render.Code.toSource source) errors
        else []

-- | Convert Elm errors to LSP diagnostics
errorToDiagnostics :: Reporting.Render.Code.Source -> Reporting.Error.Error -> [Diagnostic]
errorToDiagnostics source err =
  let reports = Reporting.Error.toReports source err
  in map reportToDiagnostic (Data.NonEmptyList.toList reports)

-- | Convert a Report to an LSP Diagnostic
reportToDiagnostic :: Reporting.Report.Report -> Diagnostic
reportToDiagnostic (Reporting.Report.Report title region _suggestions message) =
  Diagnostic
    { diagnosticRange = regionToRange region,
      diagnosticSeverity = Just (DiagnosticSeverity 1), -- Error
      diagnosticCode = Nothing,
      diagnosticCodeDescription = Nothing,
      diagnosticSource = Just "elm",
      diagnosticMessage = Text.pack (Reporting.Doc.toString message),
      diagnosticTags = Nothing,
      diagnosticRelatedInformation = Nothing,
      diagnosticData = Nothing
    }


-- | Read stored warnings for a given file from Live.State
getWarningsForFile :: Live.State -> FilePath -> IO [Warning.Warning]
getWarningsForFile state filePath = do
  case state of
    Watchtower.Live.Client.State _ _ mFileInfo -> do
      fileMap <- Control.Concurrent.STM.readTVarIO mFileInfo
      case Map.lookup filePath fileMap of
        Just (Watchtower.Live.Client.FileInfo { Watchtower.Live.Client.warnings = warns }) -> pure warns
        Nothing -> pure []

-- | Convert warnings to LSP diagnostics for unused items (grayed/muted)
warningsToDiagnostics :: [Warning.Warning] -> [Diagnostic]
warningsToDiagnostics =
  concatMap warningToUnusedDiagnostic

warningToUnusedDiagnostic :: Warning.Warning -> [Diagnostic]
warningToUnusedDiagnostic warn =
  case warn of
    Warning.UnusedImport region moduleName ->
      [ Diagnostic
          { diagnosticRange = regionToRange region,
            diagnosticSeverity = Just (DiagnosticSeverity 3), -- Information
            diagnosticCode = Nothing,
            diagnosticCodeDescription = Nothing,
            diagnosticSource = Just "elm",
            diagnosticMessage = Text.pack ("Unused import: " ++ Name.toChars moduleName),
            diagnosticTags = Just [1], -- Unnecessary (This is what makes it grayed out )
            diagnosticRelatedInformation = Nothing,
            diagnosticData = Nothing
          }
      ]
    Warning.UnusedVariable region context name ->
      let thing = case context of
            Warning.Def -> "definition"
            Warning.Pattern -> "variable"
      in
      [ Diagnostic
          { diagnosticRange = regionToRange region,
            diagnosticSeverity = Just (DiagnosticSeverity 3), -- Information
            diagnosticCode = Nothing,
            diagnosticCodeDescription = Nothing,
            diagnosticSource = Just "elm",
            diagnosticMessage = Text.pack ("Unused " ++ thing ++ ": " ++ Name.toChars name),
            diagnosticTags = Just [1], -- Unnecessary (This is what makes it grayed out )
            diagnosticRelatedInformation = Nothing,
            diagnosticData = Nothing
          }
      ]
    _ -> []


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
        [ InlayHint
            { inlayHintPosition = Position (positionLine startPos + 2) (positionCharacter startPos + 10),
              inlayHintLabel = JSON.String ": String",
              inlayHintKind = Just 1, -- Type
              inlayHintTextEdits = Nothing,
              inlayHintTooltip = Just (JSON.String "Inferred type"),
              inlayHintPaddingLeft = Just False,
              inlayHintPaddingRight = Just False,
              inlayHintData = Nothing
            },
          InlayHint
            { inlayHintPosition = Position (positionLine startPos + 5) (positionCharacter startPos + 15),
              inlayHintLabel = JSON.String "name:",
              inlayHintKind = Just 2, -- Parameter
              inlayHintTextEdits = Nothing,
              inlayHintTooltip = Just (JSON.String "Parameter name"),
              inlayHintPaddingLeft = Just False,
              inlayHintPaddingRight = Just True,
              inlayHintData = Nothing
            }
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

handleCodeLens :: Live.State -> CodeLensParams -> IO (Either String [CodeLens])
handleCodeLens state codeLensParams = do
  let uri = textDocumentIdentifierUri (codeLensParamsTextDocument codeLensParams)
  
  Ext.Log.log Ext.Log.LSP $ "CodeLens: Processing URI: " ++ Text.unpack uri
      
  -- Convert file:// URI to file path
  case uriToFilePath uri of
    Nothing -> do
      Ext.Log.log Ext.Log.LSP $ "CodeLens: Failed to convert URI to file path: " ++ Text.unpack uri
      return $ Right []
    Just filePath -> do
      warns <- getWarningsForFile state filePath
      let localizer = Reporting.Render.Type.Localizer.empty
          codeLenses = concatMap (warningToCodeLens localizer) warns
          missingAnnotationCount = length $ filter isMissingTypeAnnotation warns
      Ext.Log.log Ext.Log.LSP $ "CodeLens: Found " ++ show (length warns) ++ " total warnings, " ++ show missingAnnotationCount ++ " missing type annotations, " ++ show (length codeLenses) ++ " code lenses"
      return $ Right codeLenses

-- Helper to check if a warning is a MissingTypeAnnotation
isMissingTypeAnnotation :: Warning.Warning -> Bool
isMissingTypeAnnotation warning =
  case warning of
    Warning.MissingTypeAnnotation {} -> True
    _ -> False


-- Convert a warning to a code lens if it's a MissingTypeAnnotation
warningToCodeLens :: Reporting.Render.Type.Localizer.Localizer -> Warning.Warning -> [CodeLens]
warningToCodeLens localizer warning =
  case warning of
    Warning.MissingTypeAnnotation region name type_ ->
      let typeSignature = Reporting.Doc.toString $
            Reporting.Render.Type.canToDoc localizer Reporting.Render.Type.None type_
          range = regionToRange region

          withTypeSignature = Name.toChars name ++ " : " ++ typeSignature 
      in [ CodeLens
             { codeLensRange = range,
               codeLensCommand = Just $ JSON.object
                 [ "title" .= withTypeSignature,
                   "command" .= ("elm.addTypeSignature" :: Text),
                   "arguments" .= [JSON.String (Text.pack withTypeSignature)]
                 ],
               codeLensData = Nothing
             }
         ]
    _ -> []

-- Convert an Elm region to an LSP range
regionToRange :: Ann.Region -> Range
regionToRange (Ann.Region start end) =
  Range
    { rangeStart = positionToLSPPosition start,
      rangeEnd = positionToLSPPosition end
    }

-- Convert an Elm position to an LSP position (converting from 1-based to 0-based)
positionToLSPPosition :: Ann.Position -> Position
positionToLSPPosition (Ann.Position row col) =
  Position
    { positionLine = fromIntegral row - 1,
      positionCharacter = fromIntegral col - 1
    }

handleCodeAction :: Live.State -> CodeActionParams -> IO (Either String [CodeAction])
handleCodeAction _state _codeActionParams = do
  -- Placeholder: Return sample code actions
  -- In Elm, this could provide "Add import", "Generate function", "Extract variable", etc.
  let sampleCodeActions =
        [ CodeAction
            { codeActionTitle = "Add missing import",
              codeActionKind = Just "quickfix",
              codeActionDiagnostics = Nothing,
              codeActionIsPreferred = Just True,
              codeActionDisabled = Nothing,
              codeActionEdit = Nothing, -- Would contain the actual edit
              codeActionCommand = Just $ JSON.object
                [ "title" .= ("Add import" :: Text),
                  "command" .= ("elm.addImport" :: Text),
                  "arguments" .= ([] :: [JSON.Value])
                ],
              codeActionData = Nothing
            },
          CodeAction
            { codeActionTitle = "Extract to function",
              codeActionKind = Just "refactor.extract",
              codeActionDiagnostics = Nothing,
              codeActionIsPreferred = Just False,
              codeActionDisabled = Nothing,
              codeActionEdit = Nothing, -- Would contain the actual edit
              codeActionCommand = Just $ JSON.object
                [ "title" .= ("Extract function" :: Text),
                  "command" .= ("elm.extractFunction" :: Text),
                  "arguments" .= ([] :: [JSON.Value])
                ],
              codeActionData = Nothing
            }
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
serve :: Live.State -> JSONRPC.Request -> IO (Either JSONRPC.Error JSONRPC.Response)
serve state req@(JSONRPC.Request _ reqId method params) = do
  
  case params of
    Just p -> Ext.Log.log Ext.Log.LSP $ "REQ: " ++Text.unpack method ++ " (id: " ++ show reqId ++ "): " ++ (Text.unpack . Data.Text.Encoding.decodeUtf8 . LBS.toStrict . Data.Aeson.Encode.Pretty.encodePretty $ p)
    Nothing -> Ext.Log.log Ext.Log.LSP $ "REQ: " ++Text.unpack method ++ " (id: " ++ show reqId ++ ") (no params)" 
  
  case Text.unpack method of
    -- Lifecycle
    "initialize" -> 
      handleLSPRequest reqId params "initialize" (handleInitialize state)
        
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
handleNotification :: Live.State -> JSONRPC.Notification -> IO ()
handleNotification state (JSONRPC.Notification _ method params) = do

  case params of
    Just p -> Ext.Log.log Ext.Log.LSP $ "NOTIF: " ++Text.unpack method ++ " " ++ (Text.unpack . Data.Text.Encoding.decodeUtf8 . LBS.toStrict . Data.Aeson.Encode.Pretty.encodePretty $ p)
    Nothing -> Ext.Log.log Ext.Log.LSP $ "NOTIF: " ++Text.unpack method ++ "(no params)" 

  case T.unpack method of
    "textDocument/didOpen" -> do
      case params of
        Just p -> do
          case JSON.fromJSON p of
            JSON.Success openParams -> do
              _ <- handleDidOpen state openParams
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
