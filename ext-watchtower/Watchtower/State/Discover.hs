{-# LANGUAGE OverloadedStrings #-}

module Watchtower.State.Discover (discover) where

import qualified Control.Concurrent.STM as STM
import Control.Monad as Monad
import Data.Function ((&))
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Ext.CompileHelpers.Generic as CompileHelpers
import qualified Ext.Dev.Project
import qualified Ext.Log
import qualified Ext.Sentry
import qualified Watchtower.Live.Client as Client
import qualified Watchtower.Live.Compile
import qualified Watchtower.State.Project
import qualified Watchtower.Websocket

discover :: Client.State -> FilePath -> Map.Map FilePath Client.FileWatchType -> IO ()
discover state@(Client.State mClients mProjects) root watching = do
  Ext.Log.log Ext.Log.Live ("👀 discover requested: " <> root)
  projects <- Ext.Dev.Project.discover root

  let projectTails = fmap (getProjectShorthand root) projects
  Ext.Log.log Ext.Log.Live (("👁️  found projects\n" ++ root) <> Ext.Log.formatList projectTails)

  Monad.foldM (initializeProject state) [] projects
  pure ()

initializeProject :: Client.State -> [Client.ProjectCache] -> Ext.Dev.Project.Project -> IO [Client.ProjectCache]
initializeProject state accum project =
  do
    let flags = CompileHelpers.Flags CompileHelpers.Dev CompileHelpers.NoOutput
    projectCache <- Watchtower.State.Project.upsert state flags (Ext.Dev.Project._root project) (Ext.Dev.Project._entrypoints project)
    pure (projectCache : accum)

getProjectShorthand :: FilePath -> Ext.Dev.Project.Project -> FilePath
getProjectShorthand root proj =
  case List.stripPrefix root (Ext.Dev.Project.getRoot proj) of
    Nothing -> "."
    Just "" -> "."
    Just str ->
      str
