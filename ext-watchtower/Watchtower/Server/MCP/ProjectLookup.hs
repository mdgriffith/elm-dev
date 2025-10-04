{-# LANGUAGE OverloadedStrings #-}

module Watchtower.Server.MCP.ProjectLookup
  ( resolveProject
  , listKnownProjectsText
  ) where

import qualified Control.Concurrent.STM as STM
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Ext.Dev.Project
import qualified Watchtower.Live.Client as Client


-- | Produce a human-readable list of all known projects with their ids.
listKnownProjectsText :: [Client.ProjectCache] -> Text
listKnownProjectsText projects =
  let fmt pc@(Client.ProjectCache proj _ _ _ _) =
        let pid = Ext.Dev.Project._shortId proj
            root = Ext.Dev.Project.getRoot proj
        in Text.pack ("- " ++ show pid ++ ": " ++ root)
  in case projects of
       [] -> "(none)"
       _  -> Text.intercalate "\n" (fmap fmt projects)


-- | Resolve a project to operate on given an optional shortId.
--
-- Behavior:
-- - If 'Just id', find that project or return an error including known projects.
-- - If 'Nothing' and exactly one project is known, select it.
-- - Otherwise, return an ambiguous error including known projects.
resolveProject :: Maybe Int -> Client.State -> IO (Either Text Client.ProjectCache)
resolveProject mProjectId (Client.State _ mProjects _ _ _) = do
  projects <- STM.readTVarIO mProjects
  case mProjectId of
    Just pid -> do
      let found = List.find (\(Client.ProjectCache proj _ _ _ _) -> Ext.Dev.Project._shortId proj == pid) projects
      case found of
        Just pc -> pure (Right pc)
        Nothing -> do
          let known = listKnownProjectsText projects
          pure (Left (Text.pack ("Project id " ++ show pid ++ " not found. Known projects:\n") <> known))
    Nothing ->
      case projects of
        [one] -> pure (Right one)
        _ -> do
          let known = listKnownProjectsText projects
          let countTxt = Text.pack (show (length projects))
          pure (Left ("Ambiguous project: " <> countTxt <> " projects known. Provide projectId.\nKnown projects:\n" <> known))



