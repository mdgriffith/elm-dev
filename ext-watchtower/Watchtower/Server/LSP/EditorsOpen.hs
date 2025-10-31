{-# LANGUAGE OverloadedStrings #-}

module Watchtower.Server.LSP.EditorsOpen
  ( EditorsOpen(..)
  , empty
  , fileMarkedOpen
  , fileMarkedClosed
  , isProjectOpen
  ) where

import qualified Data.Foldable
import qualified Data.Map.Strict as Map
import qualified Ext.Dev.Project

-- Track which files are currently open in editors (via LSP notifications),
-- counting multiple opens of the same file.
newtype EditorsOpen = EditorsOpen (Map.Map FilePath Int)

empty :: EditorsOpen
empty = EditorsOpen Map.empty

fileMarkedOpen :: FilePath -> EditorsOpen -> EditorsOpen
fileMarkedOpen path (EditorsOpen m) =
  let newCount = case Map.lookup path m of
        Nothing -> 1
        Just n -> n + 1
  in EditorsOpen (Map.insert path newCount m)

fileMarkedClosed :: FilePath -> EditorsOpen -> EditorsOpen
fileMarkedClosed path (EditorsOpen m) =
  case Map.lookup path m of
    Nothing -> EditorsOpen m
    Just n ->
      if n <= 1
        then EditorsOpen (Map.delete path m)
        else EditorsOpen (Map.insert path (n - 1) m)

-- Is the given project considered open by any open file?
isProjectOpen :: Ext.Dev.Project.Project -> EditorsOpen -> Bool
isProjectOpen proj (EditorsOpen openFiles) =
  Data.Foldable.any (\p -> Ext.Dev.Project.contains p proj) (Map.keys openFiles)


