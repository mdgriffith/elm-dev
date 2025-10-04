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
      Ext.Log.log Ext.Log.LSP "LSP Initialize: No root path provided"
  
  let initResult = InitializeResult
        { initializeResultCapabilities = defaultServerCapabilities,
          initializeResultServerInfo = Just $ ServerInfo
            { serverInfoName = "elm-dev-lsp",
              serverInfoVersion = Just "1.0.0"
            }
        }
  return $ Right initResult

handleDidOpen :: Live.State -> DidOpenTextDocumentParams -> IO (Either String JSON.Value)
handleDidOpen state openParams = do
  -- Handle document open notification
  let doc = didOpenTextDocumentParamsTextDocument openParams
      uri = textDocumentItemUri doc
      languageId = textDocumentItemLanguageId doc
      version = textDocumentItemVersion doc
      text = textDocumentItemText doc
  
  -- On open: compile only if any relevant project is currently uncompiled
  case uriToFilePath uri of
    Nothing -> return $ Right JSON.Null
    Just filePath -> do
      projectResult <- Watchtower.Live.Client.getExistingProject filePath state
      case projectResult of
        Left _ -> return $ Right JSON.Null
        Right (first, rest) -> do
          firstStatus <- Control.Concurrent.STM.readTVarIO (Watchtower.Live.Client.compileResult first)
          restStatuses <- mapM (\p -> Control.Concurrent.STM.readTVarIO (Watchtower.Live.Client.compileResult p)) rest
          let anyUncompiled = any isUncompiled (firstStatus : restStatuses)
          if anyUncompiled
            then do
              Watchtower.State.Compile.compileRelevantProjects state [filePath]
              return $ Right JSON.Null
            else return $ Right JSON.Null
  where
    isUncompiled res = case res of
      Watchtower.Live.Client.NotCompiled -> True
      _ -> False

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
handleDidClose _state closeParams = do
  -- Handle document close notification
  let doc = didCloseTextDocumentParamsTextDocument closeParams
      uri = textDocumentIdentifierUri doc
    
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
      mInfo <- Watchtower.Live.Client.getFileInfo filePath state
      case mInfo of
        Nothing -> pure (Right Nothing)
        Just (Watchtower.Live.Client.FileInfo
              { Watchtower.Live.Client.sourceAst = _maybeSrc
              , Watchtower.Live.Client.canonicalAst = maybeCan
              , Watchtower.Live.Client.localizer = maybeLoc
              }) ->
          case maybeCan of
            Just canMod -> do
              let pos = lspPositionToElmPosition lspPos
              case Watchtower.AST.Lookup.findAtPosition canMod pos of
                Nothing -> pure (Right Nothing)
                Just (found, region) -> do
                  case found of
                    Watchtower.AST.Lookup.FoundType name tipe -> do
                      let content = renderHoverSignature maybeLoc (Just (Text.pack (Name.toChars name))) tipe
                      pure (Right (Just Hover { hoverContents = content, hoverRange = Just (regionToRange region) }))
                    Watchtower.AST.Lookup.FoundPattern patt tipe -> do
                      let maybeNm = patternName patt
                          content = renderHoverSignature maybeLoc (fmap (Text.pack . Name.toChars) maybeNm) tipe
                      pure (Right (Just Hover { hoverContents = content, hoverRange = Just (regionToRange region) }))
                    Watchtower.AST.Lookup.FoundVarLocal name -> do
                      case getTypeForFoundVar mInfo region of
                        Nothing -> pure (Right Nothing)
                        Just tipe -> do
                          let content = renderHoverSignature maybeLoc (Just (Text.pack (Name.toChars name))) tipe
                          pure (Right (Just Hover { hoverContents = content, hoverRange = Just (regionToRange region) }))
                    Watchtower.AST.Lookup.FoundVarTopLevel name -> do
                      case getTypeForFoundVar mInfo region of
                        Nothing -> pure (Right Nothing)
                        Just tipe -> do
                          let content = renderHoverSignature maybeLoc (Just (Text.pack (Name.toChars name))) tipe
                          pure (Right (Just Hover { hoverContents = content, hoverRange = Just (regionToRange region) }))
                    Watchtower.AST.Lookup.FoundVarForeign home name (Can.Forall _ tipe) -> do
                      docsAbove <- getForeignValueDocs state filePath home name
                      let signatureContent = renderHoverSignatureQualified maybeLoc home name tipe
                          content = case docsAbove of
                            Just docText ->
                              MarkupContent
                                { markupContentKind = "markdown"
                                , markupContentValue = Text.pack (docText ++ "\n\n" ++ Text.unpack (markupContentValue signatureContent))
                                }
                            Nothing -> signatureContent
                      pure (Right (Just Hover { hoverContents = content, hoverRange = Just (regionToRange region) }))
                    Watchtower.AST.Lookup.FoundVarCtor _home name (Can.Forall _ tipe) -> do
                      let content = renderHoverSignature maybeLoc (Just (Text.pack (Name.toChars name))) tipe
                      pure (Right (Just Hover { hoverContents = content, hoverRange = Just (regionToRange region) }))
                    Watchtower.AST.Lookup.FoundVarDebug _home name (Can.Forall _ tipe) -> do
                      let content = renderHoverSignature maybeLoc (Just (Text.pack (Name.toChars name))) tipe
                      pure (Right (Just Hover { hoverContents = content, hoverRange = Just (regionToRange region) }))
                    Watchtower.AST.Lookup.FoundVarOperator sym _home _real (Can.Forall _ tipe) -> do
                      let content = renderHoverSignature maybeLoc (Just (Text.pack (Name.toChars sym))) tipe
                      pure (Right (Just Hover { hoverContents = content, hoverRange = Just (regionToRange region) }))
                    Watchtower.AST.Lookup.FoundBinop sym _home _real (Can.Forall _ tipe) -> do
                      let content = renderHoverSignature maybeLoc (Just (Text.pack (Name.toChars sym))) tipe
                      pure (Right (Just Hover { hoverContents = content, hoverRange = Just (regionToRange region) }))
            _ -> pure (Right Nothing)

-- Look up the inferred type for a FoundVar (local or top-level) by its region
getTypeForFoundVar :: Maybe Watchtower.Live.Client.FileInfo -> Ann.Region -> Maybe Can.Type
getTypeForFoundVar mInfo region =
  case mInfo of
    Just (Watchtower.Live.Client.FileInfo { Watchtower.Live.Client.typeAt = Just typeAtMap }) ->
      case Map.lookup region typeAtMap of
        Just (Can.Forall _ tipe) -> Just tipe
        _ -> Nothing
    _ -> Nothing

-- Render a markdown hover with Elm syntax highlighting. Include name when provided.
renderHoverSignature :: Maybe Reporting.Render.Type.Localizer.Localizer -> Maybe Text -> Can.Type -> MarkupContent
renderHoverSignature maybeLoc maybeName tipe =
  let loc = Maybe.fromMaybe Reporting.Render.Type.Localizer.empty maybeLoc
      aliasBlock = Ext.Render.Type.renderAlias loc tipe
      label = Reporting.Doc.toString (Reporting.Render.Type.canToDoc loc Reporting.Render.Type.None tipe)
      nameStr = case maybeName of
        Just n -> Text.unpack n
        Nothing -> ""
      header = if null aliasBlock then "" else aliasBlock ++ "\n\n"
      signature = formatNameAndLabel nameStr label
      fenced = "```elm\n" ++ header ++ signature ++ "\n```"
  in MarkupContent { markupContentKind = "markdown", markupContentValue = Text.pack fenced }

-- Render a markdown hover with a fully qualified value name when possible.
-- For foreign values, use the localizer to determine the best qualification.
renderHoverSignatureQualified :: Maybe Reporting.Render.Type.Localizer.Localizer -> Elm.ModuleName.Canonical -> Name.Name -> Can.Type -> MarkupContent
renderHoverSignatureQualified maybeLoc home name tipe =
  let loc = Maybe.fromMaybe Reporting.Render.Type.Localizer.empty maybeLoc
      aliasBlock = Ext.Render.Type.renderAlias loc tipe
      label = Reporting.Doc.toString (Reporting.Render.Type.canToDoc loc Reporting.Render.Type.None tipe)
      nameQualified = case maybeLoc of
        Just l -> Reporting.Render.Type.Localizer.toChars l home name
        Nothing -> case home of
          Elm.ModuleName.Canonical _ moduleName -> Name.toChars moduleName ++ "." ++ Name.toChars name
      header = if null aliasBlock then "" else aliasBlock ++ "\n\n"
      signature = formatNameAndLabel nameQualified label
      fenced = "```elm\n" ++ header ++ signature ++ "\n```"
  in MarkupContent { markupContentKind = "markdown", markupContentValue = Text.pack fenced }

-- Helper: format name and label for hover signatures.
-- When the label spans multiple lines, place the colon after the name and
-- indent each label line by 4 spaces. Otherwise, keep it on a single line.
formatNameAndLabel :: String -> String -> String
formatNameAndLabel name label =
  if '\n' `elem` label
    then name ++ " :\n" ++ unlines (map ("    " ++) (lines label))
    else name ++ " : " ++ label

-- Try to fetch docs for a foreign value from the module's FileInfo stored in state
getForeignValueDocs :: Live.State -> FilePath -> Elm.ModuleName.Canonical -> Name.Name -> IO (Maybe String)
getForeignValueDocs state currentFilePath home valueName = do
  projectResult <- Watchtower.Live.Client.getExistingProject currentFilePath state
  case projectResult of
    Right (projectCache, _) -> do
      Ext.Log.log Ext.Log.Live $ "hovering over " ++ currentFilePath ++ " for " ++ (case home of Elm.ModuleName.Canonical pkg moduleName -> Elm.Package.toChars pkg ++ ":" ++ Name.toChars moduleName) ++ "." ++ Name.toChars valueName
      Watchtower.Live.Client.logFileInfoKeys state
      case home of
        -- Local module from this project: use FileInfo as before
        Elm.ModuleName.Canonical pkg _moduleName | pkg == Elm.Package.dummyName -> do
          mInfo <- Watchtower.Live.Client.getFileInfoFromModuleName projectCache home state
          case mInfo of
            Just (Watchtower.Live.Client.FileInfo { Watchtower.Live.Client.docs = Just docModule }) -> do
              case docModule of
                Elm.Docs.Module _ _ _ _ values _ ->
                  case Map.lookup valueName values of
                    Just (Elm.Docs.Value comment _) -> do
                      let raw = Json.String.toChars comment
                          rendered = Text.unpack (Text.replace "\\n" "\n" (Text.pack raw))
                      pure (Just rendered)
                    _ -> pure (Just "no value found")
            _ -> pure (Just "no info for docs")

        -- Module from a dependency package: read from packages cache
        Elm.ModuleName.Canonical pkg moduleName -> do
          let mPackages = case state of
                Watchtower.Live.Client.State _ _ _ mpkgs _ -> mpkgs
          packagesMap <- Control.Concurrent.STM.readTVarIO mPackages
          case Map.lookup pkg packagesMap of
            Just (Watchtower.Live.Client.PackageInfo { Watchtower.Live.Client.packageModules = mods }) -> do
              case Map.lookup moduleName mods of
                Just (Watchtower.Live.Client.PackageModule { Watchtower.Live.Client.packageModuleDocs = docModule }) -> do
                  case docModule of
                    Elm.Docs.Module _ _ _ _ values _ ->
                      case Map.lookup valueName values of
                        Just (Elm.Docs.Value comment _) -> do
                          let raw = Json.String.toChars comment
                              rendered = Text.unpack (Text.replace "\\n" "\n" (Text.pack raw))
                              header = "_from " ++ Elm.Package.toChars pkg ++ "_\n\n"
                          pure (Just (header ++ rendered))
                        _ -> pure (Just "no package value found")
                _ -> pure (Just "no package module found")
            _ -> pure (Just "no package info for docs")
    _ -> pure (Just "no docs")

-- Convert LSP Position to Elm Ann.Position (1-based)
lspPositionToElmPosition :: Position -> Ann.Position
lspPositionToElmPosition (Position l c) = Ann.Position (fromIntegral (l + 1)) (fromIntegral (c + 1))


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
      mInfo <- Watchtower.Live.Client.getFileInfo filePath state
      case mInfo of
        Nothing -> pure (Right [])
        Just (Watchtower.Live.Client.FileInfo { Watchtower.Live.Client.canonicalAst = maybeCan }) ->
          case maybeCan of
            Nothing -> pure (Right [])
            Just canMod -> do
              let pos = lspPositionToElmPosition lspPos
              case Watchtower.AST.Lookup.findAtPosition canMod pos of
                Nothing -> pure (Right [])
                Just (found, _region) -> do
                  defLocs <- case found of
                    Watchtower.AST.Lookup.FoundVarLocal name -> do
                      let mDefReg = Watchtower.AST.Definition.findLocalDefinitionRegion canMod pos name
                      pure $ maybe [] (\r -> [Location { locationUri = uri, locationRange = regionToRange r }]) mDefReg
                    Watchtower.AST.Lookup.FoundVarTopLevel name -> do
                      let mReg = Watchtower.AST.Definition.findTopLevelDefRegion canMod name
                      pure $ maybe [] (\r -> [Location { locationUri = uri, locationRange = regionToRange r }]) mReg
                    Watchtower.AST.Lookup.FoundVarForeign home name _ -> do
                      resolveForeignDefinition state home name
                    Watchtower.AST.Lookup.FoundVarOperator _sym home real _ -> do
                      resolveForeignDefinition state home real
                    Watchtower.AST.Lookup.FoundBinop _sym home real _ -> do
                      resolveForeignDefinition state home real
                    Watchtower.AST.Lookup.FoundVarDebug home name _ -> do
                      resolveForeignDefinition state home name
                    Watchtower.AST.Lookup.FoundVarCtor _home _name _ ->
                      pure []
                    Watchtower.AST.Lookup.FoundType _ _ -> pure []
                    Watchtower.AST.Lookup.FoundPattern _ _ -> pure []
                  pure (Right defLocs)

  where
    resolveForeignDefinition :: Live.State -> Elm.ModuleName.Canonical -> Name.Name -> IO [Location]
    resolveForeignDefinition st home name = do
      mPath <- findModuleFilePathInState st home
      case mPath of
        Nothing -> pure []
        Just modPath -> do
          mInfo <- Watchtower.Live.Client.getFileInfo modPath st
          case mInfo of
            Just (Watchtower.Live.Client.FileInfo { Watchtower.Live.Client.canonicalAst = Just canMod }) -> do
              let mReg = Watchtower.AST.Definition.findTopLevelDefRegion canMod name
              pure $ maybe [] (\r -> [Location { locationUri = filePathToUri modPath, locationRange = regionToRange r }]) mReg
            _ -> pure []

-- Attempt to derive a human-friendly name for a pattern, when possible
patternName :: Can.Pattern_ -> Maybe Name.Name
patternName patt =
  case patt of
    Can.PVar n -> Just n
    Can.PAlias _ n -> Just n
    _ -> Nothing

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

handleReferences state referenceParams = do
  let uri = textDocumentIdentifierUri (referenceParamsTextDocument referenceParams)
      lspPos = referenceParamsPosition referenceParams
      includeDecl = parseIncludeDeclaration (referenceParamsContext referenceParams)
  case uriToFilePath uri of
    Nothing -> pure (Right [])
    Just filePath -> do
      mInfo <- Watchtower.Live.Client.getFileInfo filePath state
      case mInfo of
        Nothing -> pure (Right [])
        Just (Watchtower.Live.Client.FileInfo { Watchtower.Live.Client.canonicalAst = maybeCan }) ->
          case maybeCan of
            Nothing -> pure (Right [])
            Just canMod -> do
              let pos = lspPositionToElmPosition lspPos
              case Watchtower.AST.Lookup.findAtPosition canMod pos of
                Nothing -> pure (Right [])
                Just (found, _r) -> do
                  locations <- case found of
                    Watchtower.AST.Lookup.FoundVarLocal name -> do
                      let refs = Watchtower.AST.Definition.collectLocalReferences canMod pos name
                          defReg = Watchtower.AST.Definition.findLocalDefinitionRegion canMod pos name
                          allRegs = if includeDecl then maybe refs (:refs) defReg else refs
                      pure (map (\r -> Location { locationUri = uri, locationRange = regionToRange r }) allRegs)

                    Watchtower.AST.Lookup.FoundVarTopLevel name -> do
                      let home = case canMod of Can.Module homeName _ _ _ _ _ _ _ -> homeName
                      collectGlobalRefs state home name includeDecl

                    Watchtower.AST.Lookup.FoundVarForeign home name _ ->
                      collectGlobalRefs state home name includeDecl

                    Watchtower.AST.Lookup.FoundVarOperator _sym home real _ ->
                      collectGlobalRefs state home real includeDecl

                    Watchtower.AST.Lookup.FoundBinop _sym home real _ ->
                      collectGlobalRefs state home real includeDecl

                    Watchtower.AST.Lookup.FoundVarDebug home name _ ->
                      collectGlobalRefs state home name includeDecl

                    Watchtower.AST.Lookup.FoundVarCtor home name _ ->
                      collectGlobalRefs state home name includeDecl

                    _ -> pure []
                  pure (Right locations)

  where
    collectGlobalRefs :: Live.State -> Elm.ModuleName.Canonical -> Name.Name -> Bool -> IO [Location]
    collectGlobalRefs st home name includeDecl' = do
      allInfos <- Watchtower.Live.Client.getAllFileInfos st
      let refs =
            Map.foldrWithKey
              (\path fi acc ->
                case fi of
                  Watchtower.Live.Client.FileInfo { Watchtower.Live.Client.canonicalAst = Just m } ->
                    let rs = map (\r -> Location { locationUri = filePathToUri path, locationRange = regionToRange r })
                                 (Watchtower.AST.References.collectVarRefsInModule m home name)
                    in rs ++ acc
                  _ -> acc
              )
              []
              allInfos
      declLocs <- if includeDecl'
                    then do
                      mPath <- findModuleFilePathInState st home
                      case mPath of
                        Nothing -> pure []
                        Just modPath -> do
                          mInfo <- Watchtower.Live.Client.getFileInfo modPath st
                          case mInfo of
                            Just (Watchtower.Live.Client.FileInfo { Watchtower.Live.Client.canonicalAst = Just canMod }) -> do
                              let mReg = Watchtower.AST.Definition.findTopLevelDefRegion canMod name
                              pure $ maybe [] (\r -> [Location { locationUri = filePathToUri modPath, locationRange = regionToRange r }]) mReg
                            _ -> pure []
                    else pure []
      pure (declLocs ++ refs)

    parseIncludeDeclaration :: JSON.Value -> Bool
    parseIncludeDeclaration v =
      case v of
        JSON.Object o ->
          case KeyMap.lookup "includeDeclaration" o of
            Just (JSON.Bool b) -> b
            _ -> True
        _ -> True

-- Find a file path in state for a given canonical module
findModuleFilePathInState :: Live.State -> Elm.ModuleName.Canonical -> IO (Maybe FilePath)
findModuleFilePathInState state home = do
  infos <- Watchtower.Live.Client.getAllFileInfos state
  let matchesHome fi =
        case fi of
          Watchtower.Live.Client.FileInfo { Watchtower.Live.Client.canonicalAst = Just (Can.Module homeName _ _ _ _ _ _ _) } -> homeName == home
          _ -> False
  pure (fst <$> Data.List.find (matchesHome . snd) (Map.toList infos))

filePathToUri :: FilePath -> Text
filePathToUri fp = Text.pack ("file://" ++ fp)

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
          do { diags <- getDiagnosticsForProject state projectCache (Just filePath)
             ; (localizer_, warns) <- getWarningsForFile state filePath

             -- Encode as JSON
             ; let diagnosticsResponse = JSON.object
                     [ "kind" .= ("full" :: Text)
                     , "items" .= (diags ++ concatMap warningToUnusedDiagnostic warns)
                     ]
             ; return $ Right diagnosticsResponse
             }
        


getDiagnosticsForProject :: Live.State -> Watchtower.Live.Client.ProjectCache -> Maybe FilePath -> IO [Diagnostic]
getDiagnosticsForProject state projectCache maybeFilePath = do
  
  currentResult <- Control.Concurrent.STM.readTVarIO (Watchtower.Live.Client.compileResult projectCache)
  Ext.Log.log Ext.Log.LSP $ "READ COMPILE RESULT for " ++ Watchtower.Live.Client.getProjectRoot projectCache

  case currentResult of
    Watchtower.Live.Client.Success _ -> return []
    Watchtower.Live.Client.Error (Watchtower.Live.Client.ReactorError exitReactor) -> do
      Ext.Log.log Ext.Log.LSP "COMPILE RESULT ERROR (Reactor), EXTRACTING DIAGNOSTICS"
      return $ extractDiagnosticsFromReactor maybeFilePath exitReactor
    Watchtower.Live.Client.Error (Watchtower.Live.Client.GenerationError _) -> do
      Ext.Log.log Ext.Log.LSP "COMPILE RESULT ERROR (Generation)"
      return []
    Watchtower.Live.Client.NotCompiled -> do
      Ext.Log.log Ext.Log.LSP "COMPILE RESULT NOT COMPILED YET"
      return []


-- | Extract diagnostics from Reactor errors, optionally filtering by the requested file URI
extractDiagnosticsFromReactor :: Maybe FilePath -> Reporting.Exit.Reactor -> [Diagnostic]
extractDiagnosticsFromReactor maybeFilePathFilter reactor =
  case reactor of
    Reporting.Exit.ReactorBadBuild (Reporting.Exit.BuildBadModules _root firstModule otherModules) ->    
      concatMap (moduleErrorsToDiagnostics maybeFilePathFilter) (firstModule : otherModules)
    _ -> []  -- Other reactor errors are not module-specific

-- | Convert a module's errors to diagnostics if it matches the target file
moduleErrorsToDiagnostics :: Maybe FilePath -> Reporting.Error.Module -> [Diagnostic]
moduleErrorsToDiagnostics targetFilePath (Reporting.Error.Module _name absolutePath _time source errors) =
  case targetFilePath of
    Nothing -> errorToDiagnostics (Reporting.Render.Code.toSource source) errors
    Just target -> 
      if System.FilePath.normalise absolutePath == System.FilePath.normalise target
        then errorToDiagnostics (Reporting.Render.Code.toSource source) errors
        else []

-- | Convert Elm errors to LSP diagnostics
errorToDiagnostics :: Reporting.Render.Code.Source -> Reporting.Error.Error -> [Diagnostic]
errorToDiagnostics source err =
  let reports = Ext.Reporting.Error.toReports source err
  in map reportToDiagnostic (Data.NonEmptyList.toList reports)

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
getWarningsForFile :: Live.State -> FilePath -> IO (Reporting.Render.Type.Localizer.Localizer, [Warning.Warning])
getWarningsForFile state filePath = do
  fileInfo <- Watchtower.Live.Client.getFileInfo filePath state
  case fileInfo of
    Just (Watchtower.Live.Client.FileInfo { Watchtower.Live.Client.warnings = warns, Watchtower.Live.Client.localizer = maybeLocalizer }) -> 
      pure (Maybe.fromMaybe Reporting.Render.Type.Localizer.empty maybeLocalizer, warns)
    Nothing -> pure (Reporting.Render.Type.Localizer.empty, [])
  


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
      
  -- Convert file:// URI to file path
  case uriToFilePath uri of
    Nothing -> do
      Ext.Log.log Ext.Log.LSP $ "CodeLens: Failed to convert URI to file path: " ++ Text.unpack uri
      return $ Right []
    Just filePath -> do
      (localizer, warns) <- getWarningsForFile state filePath
      let codeLenses = concatMap (warningToCodeLens localizer) warns
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
serve :: Live.State -> JSONRPC.EventEmitter -> JSONRPC.Request -> IO (Either JSONRPC.Error JSONRPC.Response)
serve state _emit req@(JSONRPC.Request _ reqId method params) = do
  
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
handleNotification :: Live.State -> JSONRPC.EventEmitter -> JSONRPC.Notification -> IO ()
handleNotification state send (JSONRPC.Notification _ method params) = do

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



