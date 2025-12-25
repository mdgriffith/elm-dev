{-# LANGUAGE OverloadedStrings #-}

module Watchtower.Server.LSP.Helpers (getDiagnosticsForProject, getProjectDiagnosticsByFile, getUnchangedProjectDiagnosticsByFile, getDiagnosticsFromTestReactorByFile, getWarningsForFile, warningToUnusedDiagnostic, warningToCodeLens, findAllReferences, findDefinition, showHover) where

import Data.Aeson
import qualified Watchtower.Live.Client
import qualified Watchtower.Live
import qualified Ext.Log
import qualified Ext.Reporting.Error
import qualified Ext.Render.Type
import qualified Elm.Package
import qualified Elm.Docs
import qualified Control.Concurrent.STM
import qualified Reporting.Render.Code
import qualified Reporting.Render.Type
import qualified Reporting.Render.Type.Localizer
import qualified Reporting.Error
import qualified Reporting.Exit
import qualified Reporting.Report
import qualified Reporting.Doc
import qualified Reporting.Annotation as Ann
import qualified Reporting.Warning as Warning
import qualified System.FilePath
import qualified Data.Name as Name
import qualified Data.Maybe as Maybe
import qualified Data.List
import qualified Data.NonEmptyList
import qualified Data.Aeson as JSON
import qualified Data.Map as Map
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Json.String
import qualified AST.Source as Src
import qualified AST.Canonical as Can
import qualified Watchtower.AST.Lookup
import qualified Watchtower.AST.Definition
import qualified Watchtower.AST.References
import qualified Elm.ModuleName
import Data.Text (Text)
import qualified Data.Text as Text
import Watchtower.Server.LSP.Protocol


getDiagnosticsForProject :: Watchtower.Live.Client.State -> Watchtower.Live.Client.ProjectCache -> Maybe FilePath -> IO [Diagnostic]
getDiagnosticsForProject state projectCache maybeFilePath = do
  currentResult <- Control.Concurrent.STM.readTVarIO (Watchtower.Live.Client.compileResult projectCache)
  testInfo      <- Control.Concurrent.STM.readTVarIO (Watchtower.Live.Client.test projectCache)
  Ext.Log.log Ext.Log.LSP $ "READ COMPILE RESULT for " ++ Watchtower.Live.Client.getProjectRoot projectCache

  -- Project diagnostics
  projectDiags <-
    case currentResult of
      Watchtower.Live.Client.Success _ -> pure []
      Watchtower.Live.Client.Error (Watchtower.Live.Client.ReactorError exitReactor) -> do
        pure (extractDiagnosticsFromReactor maybeFilePath exitReactor)
      Watchtower.Live.Client.Error (Watchtower.Live.Client.GenerationError _) -> do
        pure []
      Watchtower.Live.Client.NotCompiled -> do
        pure []

  -- Test diagnostics (if present)
  let testDiags =
        case testInfo of
          Just (Watchtower.Live.Client.TestInfo _ _ (Just (Watchtower.Live.Client.TestError reactorErr))) ->
            extractDiagnosticsFromReactor maybeFilePath reactorErr
          _ -> []

  pure (projectDiags ++ testDiags)

-- | Aggregate diagnostics for a whole project grouped by file path.
-- Returns a map from absolute file path to all diagnostics for that file.
getProjectDiagnosticsByFile :: Watchtower.Live.Client.State -> Watchtower.Live.Client.ProjectCache -> IO (Map.Map FilePath [Diagnostic])
getProjectDiagnosticsByFile _state projectCache = do
  currentResult <- Control.Concurrent.STM.readTVarIO (Watchtower.Live.Client.compileResult projectCache)
  case currentResult of
    Watchtower.Live.Client.Success _ -> pure Map.empty
    Watchtower.Live.Client.Error (Watchtower.Live.Client.ReactorError exitReactor) ->
      pure (extractDiagnosticsFromReactorByFile exitReactor)
    Watchtower.Live.Client.Error (Watchtower.Live.Client.GenerationError _) ->
      pure Map.empty
    Watchtower.Live.Client.NotCompiled ->
      pure Map.empty


-- | Build 'unchanged' workspace diagnostic entries for files with current diagnostics
getUnchangedProjectDiagnosticsByFile :: Watchtower.Live.Client.State -> Watchtower.Live.Client.ProjectCache -> IO [WorkspaceDocumentDiagnosticsUnchanged]
getUnchangedProjectDiagnosticsByFile state projectCache = do
  diags <- getProjectDiagnosticsByFile state projectCache
  pure
    ( map
        ( \(path, _errs) ->
            WorkspaceDocumentDiagnosticsUnchanged
              { workspaceDocumentDiagnosticsUnchangedUri = fromFilePath path,
                workspaceDocumentDiagnosticsUnchangedVersion = Nothing,
                workspaceDocumentDiagnosticsUnchangedKind = ("unchanged" :: Text)
              }
        )
        (Map.toList diags)
    )




-- | Extract diagnostics from Reactor errors, optionally filtering by the requested file URI
extractDiagnosticsFromReactor :: Maybe FilePath -> Reporting.Exit.Reactor -> [Diagnostic]
extractDiagnosticsFromReactor maybeFilePathFilter reactor =
  case reactor of
    Reporting.Exit.ReactorBadBuild (Reporting.Exit.BuildBadModules _root firstModule otherModules) ->    
      concatMap (moduleErrorsToDiagnostics maybeFilePathFilter) (firstModule : otherModules)
    _ -> []  -- Other reactor errors are not module-specific

-- | Extract diagnostics from Reactor errors grouped by file path
extractDiagnosticsFromReactorByFile :: Reporting.Exit.Reactor -> Map.Map FilePath [Diagnostic]
extractDiagnosticsFromReactorByFile reactor =
  case reactor of
    Reporting.Exit.ReactorBadBuild (Reporting.Exit.BuildBadModules _root firstModule otherModules) ->
      let pairs = map moduleErrorsToDiagnosticsByFile (firstModule : otherModules)
      in Map.fromListWith (++) pairs
    _ -> Map.empty

-- | Public wrapper to obtain diagnostics grouped by file from a test compile reactor error.
getDiagnosticsFromTestReactorByFile :: Reporting.Exit.Reactor -> Map.Map FilePath [Diagnostic]
getDiagnosticsFromTestReactorByFile =
  extractDiagnosticsFromReactorByFile


-- | Convert a module's errors to diagnostics if it matches the target file
moduleErrorsToDiagnostics :: Maybe FilePath -> Reporting.Error.Module -> [Diagnostic]
moduleErrorsToDiagnostics targetFilePath (Reporting.Error.Module _name absolutePath _time source errors) =
  case targetFilePath of
    Nothing -> errorToDiagnostics (Reporting.Render.Code.toSource source) errors
    Just target -> 
      if System.FilePath.normalise absolutePath == System.FilePath.normalise target
        then errorToDiagnostics (Reporting.Render.Code.toSource source) errors
        else []

-- | Convert a module's errors to a (filePath, diagnostics) pair
moduleErrorsToDiagnosticsByFile :: Reporting.Error.Module -> (FilePath, [Diagnostic])
moduleErrorsToDiagnosticsByFile (Reporting.Error.Module _name absolutePath _time source errors) =
  ( absolutePath
  , errorToDiagnostics (Reporting.Render.Code.toSource source) errors
  )


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


-- WARNINIGS



-- | Read stored warnings for a given file from Live.State
getWarningsForFile :: Watchtower.Live.Client.State -> FilePath -> IO (Reporting.Render.Type.Localizer.Localizer, [Warning.Warning])
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

-- Convert LSP Position to Elm Ann.Position (1-based)
lspPositionToElmPosition :: Position -> Ann.Position
lspPositionToElmPosition (Position l c) = Ann.Position (fromIntegral (l + 1)) (fromIntegral (c + 1))



-- FIND ALL REFERENCES


findAllReferences :: Watchtower.Live.Client.State -> Text -> FilePath -> Position -> Bool -> IO [Location]
findAllReferences state uri filePath lspPos includeDecl = do
    mInfo <- Watchtower.Live.Client.getFileInfo filePath state
    case mInfo of
        Nothing -> pure []
        Just (Watchtower.Live.Client.FileInfo { Watchtower.Live.Client.canonicalAst = maybeCan }) ->
            case maybeCan of
            Nothing -> pure []
            Just canMod -> do
                let pos = lspPositionToElmPosition lspPos
                case Watchtower.AST.Lookup.findAtPosition canMod pos of
                    Nothing -> pure []
                    Just (found, _r) -> do
                        case found of
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
                      

    
  where
    collectGlobalRefs :: Watchtower.Live.Client.State -> Elm.ModuleName.Canonical -> Name.Name -> Bool -> IO [Location]
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
 

filePathToUri :: FilePath -> Text
filePathToUri fp =
  let stripLeadingSlashes = dropWhile (== '/')
  in Text.pack ("file:///" ++ stripLeadingSlashes fp)




-- Find a file path in state for a given canonical module
findModuleFilePathInState :: Watchtower.Live.Client.State -> Elm.ModuleName.Canonical -> IO (Maybe FilePath)
findModuleFilePathInState state home = do
  infos <- Watchtower.Live.Client.getAllFileInfos state
  let matchesHome fi =
        case fi of
          Watchtower.Live.Client.FileInfo { Watchtower.Live.Client.canonicalAst = Just (Can.Module homeName _ _ _ _ _ _ _) } -> homeName == home
          _ -> False
  pure (fst <$> Data.List.find (matchesHome . snd) (Map.toList infos))




findDefinition :: Watchtower.Live.Client.State -> Text -> FilePath -> Position -> IO (Maybe Location)
findDefinition state uri filePath lspPos = do
    mInfo <- Watchtower.Live.Client.getFileInfo filePath state
    case mInfo of
        Nothing -> pure Nothing
        Just (Watchtower.Live.Client.FileInfo { Watchtower.Live.Client.canonicalAst = maybeCan }) ->
            case maybeCan of
                Nothing -> pure Nothing
                Just canMod -> do
                    let pos = lspPositionToElmPosition lspPos
                    case Watchtower.AST.Lookup.findAtPosition canMod pos of
                        Nothing -> pure Nothing
                        Just (found, _region) -> do
                            case found of
                                Watchtower.AST.Lookup.FoundVarLocal name -> do
                                    case Watchtower.AST.Definition.findLocalDefinitionRegion canMod pos name of
                                        Nothing -> pure Nothing
                                        Just reg -> pure (Just (Location { locationUri = uri, locationRange = regionToRange reg }))
                                Watchtower.AST.Lookup.FoundVarTopLevel name -> do
                                    case Watchtower.AST.Definition.findTopLevelDefRegion canMod name of
                                        Nothing -> pure Nothing
                                        Just reg -> pure (Just (Location { locationUri = uri, locationRange = regionToRange reg }))
                                Watchtower.AST.Lookup.FoundVarForeign home name _ ->
                                    resolveForeignDefinition state home name
                                Watchtower.AST.Lookup.FoundVarOperator _sym home real _ ->
                                    resolveForeignDefinition state home real
                                Watchtower.AST.Lookup.FoundBinop _sym home real _ ->
                                    resolveForeignDefinition state home real
                                Watchtower.AST.Lookup.FoundVarDebug home name _ ->
                                    resolveForeignDefinition state home name
                                Watchtower.AST.Lookup.FoundVarCtor _home _name _ ->
                                    pure Nothing
                                Watchtower.AST.Lookup.FoundType _ _ -> pure Nothing
                                Watchtower.AST.Lookup.FoundPattern _ _ -> pure Nothing
                             

  where
    resolveForeignDefinition :: Watchtower.Live.Client.State -> Elm.ModuleName.Canonical -> Name.Name -> IO (Maybe Location)
    resolveForeignDefinition st home name = do
      mPath <- findModuleFilePathInState st home
      case mPath of
        Nothing -> pure Nothing
        Just modPath -> do
          mInfo <- Watchtower.Live.Client.getFileInfo modPath st
          case mInfo of
            Just (Watchtower.Live.Client.FileInfo { Watchtower.Live.Client.canonicalAst = Just canMod }) -> do
              case Watchtower.AST.Definition.findTopLevelDefRegion canMod name of
                Nothing -> pure Nothing
                Just reg -> pure (Just (Location { locationUri = filePathToUri modPath, locationRange = regionToRange reg }))
              
            _ -> pure Nothing






-- 

showHover state filePath lspPos = do
    mInfo <- Watchtower.Live.Client.getFileInfo filePath state
    case mInfo of
        Nothing -> pure (Right Nothing)
        Just (Watchtower.Live.Client.FileInfo
              { Watchtower.Live.Client.sourceAst = _maybeSrc
              , Watchtower.Live.Client.canonicalAst = maybeCan
              , Watchtower.Live.Client.localizer = maybeLoc
              }) ->
          case maybeCan of
            Nothing -> pure (Right Nothing)
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
getForeignValueDocs :: Watchtower.Live.Client.State -> FilePath -> Elm.ModuleName.Canonical -> Name.Name -> IO (Maybe String)
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
                Watchtower.Live.Client.State _ _ _ mpkgs _ _ _ _ -> mpkgs
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


-- Attempt to derive a human-friendly name for a pattern, when possible
patternName :: Can.Pattern_ -> Maybe Name.Name
patternName patt =
  case patt of
    Can.PVar n -> Just n
    Can.PAlias _ n -> Just n
    _ -> Nothing