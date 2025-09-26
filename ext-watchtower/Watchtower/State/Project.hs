module Watchtower.State.Project (upsert, upsertVirtual) where

import qualified Control.Concurrent.STM as STM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as List
import qualified Data.NonEmptyList as NE
import qualified Elm.Outline
import qualified Ext.CompileHelpers.Generic as CompileHelpers
import qualified Ext.Dev.Project
import qualified Ext.FileCache
import qualified Ext.Filewatch
import qualified Ext.Log
import qualified Ext.VirtualFile
import qualified Gen.Config
import qualified Gen.Generate
import qualified Json.Encode
import qualified System.Directory as Dir
import System.FilePath ((</>))
import qualified System.FilePath as FilePath
import qualified Watchtower.Live.Client as Client
import qualified Watchtower.State.Compile


entrypointsIncluded :: NE.List FilePath -> NE.List FilePath -> Bool
entrypointsIncluded provided existing =
  let providedList = NE.toList provided
      existingList = NE.toList existing
  in List.all (\p -> List.elem p existingList) providedList

updateProjectIfNecessary :: Client.ProjectCache -> CompileHelpers.Flags -> NE.List FilePath -> Maybe Client.ProjectCache
updateProjectIfNecessary existingProjectCache newFlags providedEntrypoints =
  case existingProjectCache of
    Client.ProjectCache proj docsInfo0 oldFlags mCompileResult mTestResults ->
      let Ext.Dev.Project.Project root0 projectRoot0 existingEntrypoints srcDirs0 = proj
      in if oldFlags == newFlags && entrypointsIncluded providedEntrypoints existingEntrypoints
           then Nothing
           else
             let existingList = NE.toList existingEntrypoints
                 providedList = NE.toList providedEntrypoints
                 extras = List.filter (\p -> not (List.elem p existingList)) providedList
                 combinedEntrypoints = NE.append extras existingEntrypoints
                 updatedProj = Ext.Dev.Project.Project root0 projectRoot0 combinedEntrypoints srcDirs0
             in Just (Client.ProjectCache updatedProj docsInfo0 newFlags mCompileResult mTestResults)

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
upsertVirtual state@(Client.State mClients mProjects _ _ _) flags root entrypoint = do
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
      let newProject = Ext.Dev.Project.Project virtualRoot virtualRoot (NE.List entrypoint []) srcDirs
      mCompileResult <- STM.newTVarIO Client.NotCompiled
      mTestResults <- STM.newTVarIO Nothing
      let newProjectCache = Client.ProjectCache newProject docsInfo flags mCompileResult mTestResults
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

upsert :: Client.State -> CompileHelpers.Flags -> FilePath -> NE.List FilePath -> IO Client.ProjectCache
upsert state@(Client.State mClients mProjects _ _ _) flags root entrypoints = do
  docsInfo <- readDocsInfo root
  normalizedEntrypoints <- normalizeEntrypoints root entrypoints
  -- Read srcDirs from elm.json
  outlineResult <- Elm.Outline.read root
  srcDirs <- case outlineResult of
    Right (Elm.Outline.App appOutline) -> do
      let srcDirsList = NE.toList (Elm.Outline._app_source_dirs appOutline)
      pure (map (Elm.Outline.toAbsolute root) srcDirsList)
    Right (Elm.Outline.Pkg _) -> pure [root </> "src"]
    Left _ -> pure []
  let newProject = Ext.Dev.Project.Project root root normalizedEntrypoints srcDirs 
  mCompileResult <- STM.newTVarIO Client.NotCompiled
  mTestResults <- STM.newTVarIO Nothing
  let newProjectCache = Client.ProjectCache newProject docsInfo flags mCompileResult mTestResults

  (isNew, project) <- STM.atomically $ do
    existingProjects <- STM.readTVar mProjects
    case List.find (Client.matchingProject newProjectCache) existingProjects of
      Just existingProject -> do
        case updateProjectIfNecessary existingProject flags normalizedEntrypoints of
          Nothing -> do
            pure (False, existingProject)
          Just updatedProjectCache -> do
            let updatedProjects =
                  updatedProjectCache : List.filter (not . Client.matchingProject newProjectCache) existingProjects
            STM.writeTVar mProjects updatedProjects
            pure (False, updatedProjectCache)
      Nothing -> do
        STM.writeTVar mProjects (newProjectCache : existingProjects)
        pure (True, newProjectCache)

  if isNew
    then do
      -- Ext.Filewatch.watch
      --   root
      --   ( \filesChanged -> do
      --       Ext.Log.log Ext.Log.Live $ "👀 files changed: " <> List.intercalate ", " (map FilePath.takeFileName filesChanged)
      --       mapM_ Ext.FileCache.delete filesChanged
      --       Watchtower.State.Compile.compile state flags newProjectCache filesChanged
      --       pure ()
      --   )
      pure ()
    else pure ()

  pure project

readDocsInfo :: FilePath -> IO Gen.Config.DocsConfig
readDocsInfo root =
  Dir.withCurrentDirectory root $ do
    configResult <- Gen.Generate.readConfig
    case configResult of
      Gen.Generate.ConfigFound _ config -> do
        case Gen.Config.configDocs config of
          Just docsConfig -> pure docsConfig
          Nothing -> pure Gen.Config.defaultDocs
      _ -> pure Gen.Config.defaultDocs