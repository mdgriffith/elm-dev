{-# LANGUAGE OverloadedStrings #-}

module Watchtower.State.Discover (discover) where

import qualified Control.Concurrent.STM as STM
import Control.Monad as Monad
import Data.Function ((&))
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Ext.Dev.Project
import qualified Ext.Log
import qualified Ext.Sentry
import qualified Watchtower.Live.Client as Client
import qualified Watchtower.Live.Compile
import qualified Watchtower.Websocket

discover :: Client.State -> FilePath -> Map.Map FilePath Client.FileWatchType -> IO ()
discover state@(Client.State mClients mProjects) root watching = do
  Ext.Log.log Ext.Log.Live ("👀 discover requested: " <> root)
  discovered <- discoverProjects root

  STM.atomically $ do
    STM.modifyTVar
      mProjects
      ( \projects ->
          -- Add any projects that are not already in the list.
          List.foldl
            ( \existing new ->
                if List.any (Client.matchingProject new) existing
                  then existing
                  else new : existing
            )
            projects
            discovered
      )

  Watchtower.Live.Compile.recompile state (Map.keys watching)

discoverProjects :: FilePath -> IO [Client.ProjectCache]
discoverProjects root = do
  projects <- Ext.Dev.Project.discover root

  let projectTails = fmap (getProjectShorthand root) projects
  Ext.Log.log Ext.Log.Live (("👁️  found projects\n" ++ root) <> Ext.Log.formatList projectTails)
  Monad.foldM initializeProject [] projects

initializeProject :: [Client.ProjectCache] -> Ext.Dev.Project.Project -> IO [Client.ProjectCache]
initializeProject accum project =
  do
    cache <- Ext.Sentry.init
    pure (Client.ProjectCache project cache : accum)

getProjectShorthand :: FilePath -> Ext.Dev.Project.Project -> FilePath
getProjectShorthand root proj =
  case List.stripPrefix root (Ext.Dev.Project.getRoot proj) of
    Nothing -> "."
    Just "" -> "."
    Just str ->
      str
