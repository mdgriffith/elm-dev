module Watchtower.State.Project (upsert) where

import qualified Control.Concurrent.STM as STM
import qualified Data.List as List
import qualified Ext.Dev.Project
import qualified Ext.Sentry
import qualified Data.NonEmptyList as NE
import qualified Watchtower.Live.Client as Client


upsert :: Client.State -> FilePath -> NE.List FilePath -> IO Client.ProjectCache
upsert state@(Client.State mClients mProjects) root entrypoints = do

    sentryCache <- Ext.Sentry.init
    let newProject = Ext.Dev.Project.Project root root entrypoints
    let newProjectCache = Client.ProjectCache newProject sentryCache

    STM.atomically $ do
        existingProjects <- STM.readTVar mProjects
        case List.find (Client.matchingProject newProjectCache) existingProjects of
            Just existingProject -> do
                pure existingProject
            Nothing -> do
                STM.writeTVar mProjects (newProjectCache : existingProjects)
                pure newProjectCache