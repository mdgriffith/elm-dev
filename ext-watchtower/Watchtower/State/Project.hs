{-# LANGUAGE ScopedTypeVariables #-}
module Watchtower.State.Project
  ( upsert
  , upsertVirtual
  , UpsertError(..)
  , isRelevantWatchedPath
  , shouldSyncFilesystemPath
  ) where

import qualified Control.Concurrent.STM as STM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as List
import qualified Data.NonEmptyList as NE
import qualified Elm.Outline
import qualified Ext.CompileHelpers.Generic as CompileHelpers
import qualified Reporting.Exit
import qualified Ext.Dev.Project
import qualified Ext.FileCache
import qualified Ext.Filewatch
import qualified Ext.Log
import qualified Ext.VirtualFile
import qualified Ext.Test.Compile as TestCompile
import qualified File
import qualified Gen.Config
import qualified Gen.Generate
import qualified Json.Encode
import qualified System.Directory as Dir
import System.FilePath ((</>))
import qualified System.FilePath as FilePath
import qualified Watchtower.Live.Client as Client
import qualified Watchtower.Server.LSP.EditorsOpen as EditorsOpen
import qualified Watchtower.State.Compile
import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Data.Set as Set


entrypointsIncluded :: NE.List FilePath -> NE.List FilePath -> Bool
entrypointsIncluded provided existing =
  let providedList = NE.toList provided
      existingList = NE.toList existing
  in List.all (\p -> List.elem p existingList) providedList

updateProjectIfNecessary :: Client.ProjectCache -> CompileHelpers.Flags -> NE.List FilePath -> Maybe Client.ProjectCache
updateProjectIfNecessary existingProjectCache newFlags providedEntrypoints =
  case existingProjectCache of
    Client.ProjectCache proj docsInfo0 oldFlags mCompileResult mTest ->
      let Ext.Dev.Project.Project root0 projectRoot0 existingEntrypoints srcDirs0 shortId0 = proj
      in if oldFlags == newFlags && entrypointsIncluded providedEntrypoints existingEntrypoints
           then Nothing
           else
             let existingList = NE.toList existingEntrypoints
                 providedList = NE.toList providedEntrypoints
                 extras = List.filter (\p -> not (List.elem p existingList)) providedList
                 combinedEntrypoints = NE.append extras existingEntrypoints
                 updatedProj = Ext.Dev.Project.Project root0 projectRoot0 combinedEntrypoints srcDirs0 shortId0
             in Just (Client.ProjectCache updatedProj docsInfo0 newFlags mCompileResult mTest)

{-
This is for creating a virtual project.

Points of interest:
- cwd - The root of the *real* project, which has an elm.json for that app/package
- vwd - The root of the virtual project, which is a real directory, likely in `elm-stuff/interactive`

This means
- elm.json is fully virtual and is at the vwd.
  - We need to read the elm.json at the cwd, duplicate it, and rewrite the source dirs.

-}
upsertVirtual :: Client.State -> CompileHelpers.Flags -> FilePath -> FilePath -> IO (Maybe Client.ProjectCache)
upsertVirtual state@(Client.State mClients mProjects _ _ _ _ _ _) flags root entrypoint = do
  elmJsonResult <- insertVirtualElmJson root
  case elmJsonResult of
    Left err -> do
      putStrLn (show err)
      pure Nothing
    Right virtualRoot -> do
      let docsInfo = Gen.Config.defaultDocs
      -- Read source dirs from the newly written virtual elm.json
      outlineResult <- Elm.Outline.read virtualRoot
      srcDirs <- case outlineResult of
        Right (Elm.Outline.App appOutline) -> do
          let srcDirsList = NE.toList (Elm.Outline._app_source_dirs appOutline)
          pure (map (Elm.Outline.toAbsolute virtualRoot) srcDirsList)
        _ -> pure []
      -- Preserve a stable shortId for virtual projects by reusing an existing id if present, otherwise 0 (will be set on upsert)
      let newProject = Ext.Dev.Project.Project virtualRoot virtualRoot (NE.List entrypoint []) srcDirs 0
      mCompileResult <- STM.newTVarIO Client.NotCompiled
      mTest <- STM.newTVarIO Nothing
      let newProjectCache = Client.ProjectCache newProject docsInfo flags mCompileResult mTest
      pure (Just newProjectCache)

insertVirtualElmJson :: FilePath -> IO (Either String FilePath)
insertVirtualElmJson root = do
  realProjectElmJson <- Elm.Outline.read root
  case realProjectElmJson of
    Left err -> do
      putStrLn $ "Error reading elm.json: " <> show err
      pure (Left (show err))
    Right (Elm.Outline.App appOutline) -> do
      let virtualRoot = Ext.VirtualFile.dir root
      let virtualSrcDir = virtualRoot </> "src"

      -- Ensure the virtual root directory exists
      Dir.createDirectoryIfMissing True virtualRoot
      Dir.createDirectoryIfMissing True virtualSrcDir

      putStrLn $ "Creating virtual root if necessary " <> show virtualRoot
      putStrLn $ "VIRTUAL SRC DIR " <> show virtualSrcDir

      -- Convert existing source directories to absolute paths
      let srcDirsList = NE.toList (Elm.Outline._app_source_dirs appOutline)
      let absoluteSrcDirsList = map (Elm.Outline.AbsoluteSrcDir . Elm.Outline.toAbsolute root) srcDirsList

      -- Add the new virtual source directory
      let newSrcDirs = NE.List (Elm.Outline.AbsoluteSrcDir virtualSrcDir) absoluteSrcDirsList

      -- Create the virtual outline
      let virtualOutline =
            Elm.Outline.App $
              appOutline
                { Elm.Outline._app_source_dirs = newSrcDirs
                }

      putStrLn $ "WRITING VIRTUAL ELM.JSON " <> show virtualRoot

      -- Encode and write the virtual elm.json
      Elm.Outline.write virtualRoot virtualOutline

      pure (Right virtualRoot)
    Right (Elm.Outline.Pkg _) -> do
      pure (Left "TODO: Implement packages for interactive stuff")

-- Normalize entrypoint paths to absolute paths relative to the given root.
-- If an entrypoint is already absolute, it is normalized; otherwise it is joined to the absolute root and normalized.
normalizeEntrypoints :: FilePath -> NE.List FilePath -> IO (NE.List FilePath)
normalizeEntrypoints root entrypoints = do
  absRoot <- Dir.makeAbsolute root
  let toAbs p =
        if FilePath.isAbsolute p
          then FilePath.normalise p
          else FilePath.normalise (absRoot </> p)
  case entrypoints of
    NE.List ep eps -> pure (NE.List (toAbs ep) (map toAbs eps))
 
data UpsertError
  = NoElmJson FilePath
  | ElmJsonError Reporting.Exit.Outline
  | EntrypointNotFound [FilePath]
  | EntrypointOutsideSourceDirs FilePath [FilePath]
  deriving (Show)
 
upsert :: Client.State -> CompileHelpers.Flags -> FilePath -> NE.List FilePath -> IO (Either UpsertError Client.ProjectCache)
upsert state@(Client.State mClients mProjects _ _ _ _ _ _) flags root entrypoints = do
  normalizedEntrypoints <- normalizeEntrypoints root entrypoints

  eOutline <- Exception.try (Elm.Outline.read root)
  case eOutline of
    Left (_ :: Exception.IOException) ->
      pure (Left (NoElmJson root))
    Right (Left outlineExit) ->
      pure (Left (ElmJsonError outlineExit))
    Right (Right outline) -> do
      srcDirs <- case outline of
        Elm.Outline.App appOutline -> do
          let srcDirsList = NE.toList (Elm.Outline._app_source_dirs appOutline)
          pure (map (Elm.Outline.toAbsolute root) srcDirsList)
        Elm.Outline.Pkg _ -> pure [root </> "src"]
      let normalizedList = NE.toList normalizedEntrypoints
      
      missing <- missingFiles normalizedList
      missingAfterGeneration <-
        if List.null missing
          then pure []
          else do
            generationResult <- Gen.Generate.run root
            case generationResult of
              Right () -> missingFiles normalizedList
              Left generationErr -> do
                Ext.Log.log Ext.Log.Live ("Code generation failed while validating entrypoints: " <> generationErr)
                pure missing

      case missingAfterGeneration of
        (_:_) ->
          pure (Left (EntrypointNotFound missingAfterGeneration))
        [] -> do
          let inAnySrc ep =
                any
                  (\dir ->
                    let d = FilePath.addTrailingPathSeparator (FilePath.normalise dir)
                        f = FilePath.normalise ep
                    in List.isPrefixOf d f
                  )
                  srcDirs
          case List.find (not . inAnySrc) normalizedList of
            Just badEp ->
              pure (Left (EntrypointOutsideSourceDirs badEp srcDirs))
            Nothing -> do
              docsInfo <- readDocsInfo root

              -- Assign a stable shortId based on existing projects; reuse if matching project exists, else next available
              existingProjects <- STM.readTVarIO mProjects
              let existingIds = map (Ext.Dev.Project._shortId . (\(Client.ProjectCache p _ _ _ _) -> p)) existingProjects
              let nextId = case existingIds of
                             [] -> 1
                             _  -> (maximum existingIds) + 1
              let newProject = Ext.Dev.Project.Project root root normalizedEntrypoints srcDirs nextId 
              mCompileResult <- STM.newTVarIO Client.NotCompiled
              mTest <- STM.newTVarIO Nothing
              let newProjectCache = Client.ProjectCache newProject docsInfo flags mCompileResult mTest

              (isNew, project) <- STM.atomically $ do
                existingProjects' <- STM.readTVar mProjects
                case List.find (Client.matchingProject newProjectCache) existingProjects' of
                  Just existingProject -> do
                    case updateProjectIfNecessary existingProject flags normalizedEntrypoints of
                      Nothing -> do
                        pure (False, existingProject)
                      Just updatedProjectCache -> do
                        let updatedProjects =
                              updatedProjectCache : List.filter (not . Client.matchingProject newProjectCache) existingProjects'
                        STM.writeTVar mProjects updatedProjects
                        pure (False, updatedProjectCache)
                  Nothing -> do
                    STM.writeTVar mProjects (newProjectCache : existingProjects')
                    pure (True, newProjectCache)

              if isNew
                then do
                  Ext.Filewatch.watch
                    root
                    (\filesChanged -> do
                        let normalized = map FilePath.normalise filesChanged
                        let relevant = filter isRelevantWatchedPath normalized
                        if List.null relevant
                          then pure ()
                          else do
                            Ext.Log.log Ext.Log.Live $ "👀 files changed: " <> List.intercalate ", " (map FilePath.takeFileName relevant)
                            let (Client.State _ _ _ _ _ _ mEditorsOpen _) = state
                            editorsOpen <- STM.readTVarIO mEditorsOpen
                            let syncable = filter (`shouldSyncFilesystemPath` editorsOpen) relevant

                            existing <- Monad.filterM Dir.doesFileExist syncable
                            let existingSet = Set.fromList existing
                            let removed = filter (\p -> not (Set.member p existingSet)) syncable

                            Monad.mapM_
                              (\path -> do
                                  bytes <- File.readUtf8 path
                                  Ext.FileCache.writeUtf8 path bytes
                              )
                              existing

                            Monad.mapM_ Ext.FileCache.delete removed

                            let elmJsonRoots =
                                  Set.toList
                                    ( Set.fromList
                                        ( map FilePath.takeDirectory
                                            ( filter (\path -> FilePath.takeFileName path == "elm.json") relevant
                                            )
                                        )
                                    )
                            Monad.mapM_ TestCompile.regenerateTestElmJson elmJsonRoots

                            Watchtower.State.Compile.markFilesystemChanged state syncable
                            Watchtower.State.Compile.compileRelevantProjects state relevant
                    )
                  pure ()
                else pure ()

              pure (Right project)


missingFiles :: [FilePath] -> IO [FilePath]
missingFiles paths = do
  statuses <- mapM Dir.doesFileExist paths
  let combined = zip paths statuses
  pure [p | (p, exists) <- combined, not exists]

readDocsInfo :: FilePath -> IO Gen.Config.DocsConfig
readDocsInfo root = do
  configResult <- Gen.Generate.readConfig root
  case configResult of
    Gen.Generate.ConfigFound _ config -> do
      case Gen.Config.configDocs config of
        Just docsConfig -> pure docsConfig
        Nothing -> pure Gen.Config.defaultDocs
    _ -> pure Gen.Config.defaultDocs

isRelevantWatchedPath :: FilePath -> Bool
isRelevantWatchedPath path =
  let base = FilePath.takeFileName path
      ext = FilePath.takeExtension path
   in ext == ".elm"
        || base == "elm.json"
        || base == "elm.dev.json"

shouldSyncFilesystemPath :: FilePath -> EditorsOpen.EditorsOpen -> Bool
shouldSyncFilesystemPath path editorsOpen =
  not (EditorsOpen.isFileOpen path editorsOpen)
