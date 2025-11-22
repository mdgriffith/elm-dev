{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Watchtower.Server.LSP.Protocol where

import Data.Text (Text)
import qualified Data.Aeson as JSON
import Data.Aeson.TH (deriveJSON, defaultOptions, Options(..))
import Data.Aeson.TH
import GHC.Generics
import qualified Watchtower.Server.JSONRPC as JSONRPC
import qualified Ext.Common
import qualified Data.Text as Text




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
    [ "includeText" JSON..= includeText ]

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
    [ "openClose" JSON..= openClose
    , "change" JSON..= change
    , "save" JSON..= save
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

-- | Formatting options (subset of LSP options we care about)
data FormattingOptions = FormattingOptions
  { formattingOptionsTabSize :: Int,
    formattingOptionsInsertSpaces :: Bool,
    formattingOptionsTrimTrailingWhitespace :: Maybe Bool,
    formattingOptionsInsertFinalNewline :: Maybe Bool,
    formattingOptionsTrimFinalNewlines :: Maybe Bool
  }
  deriving stock (Show, Eq, Generic)

-- | Document formatting request parameters
data DocumentFormattingParams = DocumentFormattingParams
  { documentFormattingParamsTextDocument :: TextDocumentIdentifier,
    documentFormattingParamsOptions :: FormattingOptions,
    documentFormattingParamsWorkDoneToken :: Maybe JSON.Value
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


-- | Code lens request parameters
data CodeLensParams = CodeLensParams
  { codeLensParamsTextDocument :: TextDocumentIdentifier,
    codeLensParamsWorkDoneToken :: Maybe JSON.Value,
    codeLensParamsPartialResultToken :: Maybe JSON.Value
  }
  deriving stock (Show, Eq, Generic)

-- | URI newtype to represent document URIs
newtype Uri = Uri { unUri :: Text }
  deriving stock (Show, Eq, Generic)

-- | Construct a file:// URI from an absolute or relative file path
fromFilePath :: FilePath -> Uri
fromFilePath filePath =
  let stripLeadingSlashes = dropWhile (== '/')
  in Uri (Text.pack ("file:///" ++ stripLeadingSlashes filePath))

-- | Convert a URI back to a file system path
toFilePath :: Uri -> FilePath
toFilePath (Uri t) =
  let raw = t
  in case Text.stripPrefix "file:///" raw of
       Just rest -> '/' : Text.unpack rest
       Nothing ->
         case Text.stripPrefix "file://" raw of
           Just rest ->
             let s = Text.unpack rest
             in if not (null s) && head s == '/' then s else '/' : s
           Nothing ->
             Text.unpack raw

-- | Workspace document diagnostics item
data WorkspaceDocumentDiagnostics = WorkspaceDocumentDiagnostics
  { workspaceDocumentDiagnosticsUri :: Uri,
    workspaceDocumentDiagnosticsVersion :: Maybe Int,
    workspaceDocumentDiagnosticsKind :: Text,
    workspaceDocumentDiagnosticsItems :: [Diagnostic]
  }
  deriving stock (Show, Eq, Generic)

-- | Workspace document diagnostics item for unchanged reports (no items field)
data WorkspaceDocumentDiagnosticsUnchanged = WorkspaceDocumentDiagnosticsUnchanged
  { workspaceDocumentDiagnosticsUnchangedUri :: Uri,
    workspaceDocumentDiagnosticsUnchangedVersion :: Maybe Int,
    workspaceDocumentDiagnosticsUnchangedKind :: Text
  }
  deriving stock (Show, Eq, Generic)

-- | Workspace diagnostics report
data WorkspaceDiagnostics = WorkspaceDiagnostics
  { workspaceDiagnosticsItems :: [WorkspaceDocumentDiagnostics]
  }
  deriving stock (Show, Eq, Generic)


-- * JSON Instances

-- Custom JSON instance for DiagnosticSeverity (must come before deriveJSON for Diagnostic)
instance JSON.ToJSON DiagnosticSeverity where
  toJSON (DiagnosticSeverity n) = JSON.toJSON n

instance JSON.FromJSON DiagnosticSeverity where
  parseJSON v = DiagnosticSeverity <$> JSON.parseJSON v

instance JSON.ToJSON Uri where
  toJSON (Uri t) = JSON.String t

instance JSON.FromJSON Uri where
  parseJSON (JSON.String t) = pure (Uri t)
  parseJSON _ = fail "Invalid URI (expected string)"

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
$(deriveJSON defaultOptions {fieldLabelModifier = Ext.Common.removePrefixAndDecapitalize "formattingOptions", omitNothingFields = True} ''FormattingOptions)
$(deriveJSON defaultOptions {fieldLabelModifier = Ext.Common.removePrefixAndDecapitalize "documentFormattingParams", omitNothingFields = True} ''DocumentFormattingParams)
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
$(deriveJSON defaultOptions {fieldLabelModifier = Ext.Common.removePrefixAndDecapitalize "workspaceDocumentDiagnostics", omitNothingFields = True} ''WorkspaceDocumentDiagnostics)
$(deriveJSON defaultOptions {fieldLabelModifier = Ext.Common.removePrefixAndDecapitalize "workspaceDocumentDiagnosticsUnchanged", omitNothingFields = True} ''WorkspaceDocumentDiagnosticsUnchanged)
$(deriveJSON defaultOptions {fieldLabelModifier = Ext.Common.removePrefixAndDecapitalize "workspaceDiagnostics"} ''WorkspaceDiagnostics)



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
  case parseParams reqId params paramType of
    Right parsed -> do
      result <- handler parsed
      case result of
        Right response -> do
          return $ success reqId (JSON.toJSON response)
        Left errorMsg -> do
          return $ err reqId errorMsg
    Left jsonRpcError -> do
      return $ Left jsonRpcError


-- | LSP window/showMessage and window/logMessage severity
data LogMessageType
  = LogMessageTypeError
  | LogMessageTypeWarning
  | LogMessageTypeInfo
  | LogMessageTypeLog
  deriving stock (Show, Eq)

instance JSON.ToJSON LogMessageType where
  toJSON t = case t of
    LogMessageTypeError -> JSON.Number 1
    LogMessageTypeWarning -> JSON.Number 2
    LogMessageTypeInfo -> JSON.Number 3
    LogMessageTypeLog -> JSON.Number 4

-- | Build a window/logMessage notification as Outbound
logMessage :: LogMessageType -> Text -> JSONRPC.Outbound
logMessage typ msg =
  JSONRPC.OutboundNotification (JSONRPC.Notification "2.0" "window/logMessage" (Just (JSON.object ["type" JSON..= typ, "message" JSON..= msg])))

-- | Build a window/showMessage notification as Outbound
showMessage :: LogMessageType -> Text -> JSONRPC.Outbound
showMessage typ msg =
  JSONRPC.OutboundNotification (JSONRPC.Notification "2.0" "window/showMessage" (Just (JSON.object ["type" JSON..= typ, "message" JSON..= msg])))

