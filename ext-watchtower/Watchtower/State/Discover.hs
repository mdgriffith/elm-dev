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

discover :: Client.State -> FilePath -> IO ()
discover state root = do
  Ext.Log.log Ext.Log.Live ("👀 discover requested: " <> root)
  projects <- Ext.Dev.Project.discover root

  let projectTails = fmap (getProjectShorthand root) projects

  if List.null projectTails
    then Ext.Log.log Ext.Log.Live "found no projects"
    else Ext.Log.log Ext.Log.Live (("found projects (" ++ root ++ ")") <> Ext.Log.formatList projectTails)

  Monad.foldM_ (initializeProject state) [] projects
  

initializeProject :: Client.State -> [Client.ProjectCache] -> Ext.Dev.Project.Project -> IO [Client.ProjectCache]
initializeProject state accum project = do
  let flags = CompileHelpers.Flags CompileHelpers.Dev CompileHelpers.NoOutput
  result <- Watchtower.State.Project.upsert state flags (Ext.Dev.Project._root project) (Ext.Dev.Project._entrypoints project)
  case result of
    Left _ ->
      pure accum
    Right projectCache ->
      pure (projectCache : accum)

getProjectShorthand :: FilePath -> Ext.Dev.Project.Project -> FilePath
getProjectShorthand root proj =
  case List.stripPrefix root (Ext.Dev.Project.getRoot proj) of
    Nothing -> "."
    Just "" -> "."
    Just str ->
      str
