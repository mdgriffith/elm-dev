module Watchtower.State.Project (upsert) where

import qualified Control.Concurrent.STM as STM
import qualified Data.List as List
import qualified Data.NonEmptyList as NE
import qualified Ext.CompileHelpers.Generic as CompileHelpers
import qualified Ext.Dev.Project
import qualified Ext.FileCache
import qualified Ext.Filewatch
import qualified Ext.Log
import qualified Ext.Sentry
import qualified Gen.Config
import qualified Gen.Generate
import qualified System.Directory as Dir
import qualified System.FilePath as FilePath
import qualified Watchtower.Live.Client as Client
import qualified Watchtower.State.Compile

upsert :: Client.State -> CompileHelpers.Flags -> FilePath -> NE.List FilePath -> IO Client.ProjectCache
upsert state@(Client.State mClients mProjects) flags root entrypoints = do
  sentryCache <- Ext.Sentry.init
  docsInfo <- readDocsInfo root
  let newProject = Ext.Dev.Project.Project root root entrypoints
  let newProjectCache = Client.ProjectCache newProject docsInfo sentryCache

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
      Ext.Filewatch.watch
        root
        ( \filesChanged -> do
            Ext.Log.log Ext.Log.Live $ "👀 files changed: " <> List.intercalate ", " (map FilePath.takeFileName filesChanged)
            mapM_ Ext.FileCache.delete filesChanged
            Watchtower.State.Compile.compile flags newProjectCache
            pure ()
        )
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