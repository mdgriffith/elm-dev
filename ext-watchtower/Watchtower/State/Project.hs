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
upsertVirtual state@(Client.State mClients mProjects _) flags root entrypoint = do
  elmJsonResult <- insertVirtualElmJson root
  case elmJsonResult of
    Left err -> do
      putStrLn (show err)
      pure Nothing
    Right virtualRoot -> do
      let docsInfo = Gen.Config.defaultDocs
      let newProject = Ext.Dev.Project.Project virtualRoot virtualRoot (NE.List entrypoint [])
      mCompileResult <- STM.newTVarIO Client.NotCompiled
      mTestResults <- STM.newTVarIO Nothing
      let newProjectCache = Client.ProjectCache newProject docsInfo mCompileResult mTestResults
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

upsert :: Client.State -> CompileHelpers.Flags -> FilePath -> NE.List FilePath -> IO Client.ProjectCache
upsert state@(Client.State mClients mProjects _) flags root entrypoints = do
  docsInfo <- readDocsInfo root
  let newProject = Ext.Dev.Project.Project root root entrypoints 
  mCompileResult <- STM.newTVarIO Client.NotCompiled
  mTestResults <- STM.newTVarIO Nothing
  let newProjectCache = Client.ProjectCache newProject docsInfo mCompileResult mTestResults

  (isNew, project) <- STM.atomically $ do
    existingProjects <- STM.readTVar mProjects
    case List.find (Client.matchingProject newProjectCache) existingProjects of
      Just existingProject -> do
        pure (False, existingProject)
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