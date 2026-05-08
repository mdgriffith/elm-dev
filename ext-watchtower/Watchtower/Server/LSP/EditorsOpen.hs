{-# LANGUAGE OverloadedStrings #-}

module Watchtower.Server.LSP.EditorsOpen
  ( EditorsOpen(..)
  , empty
  , fileMarkedOpen
  , fileMarkedClosed
  , connectionClosed
  , isFileOpen
  , isProjectOpen
  , toCounts
  ) where

import qualified Data.Foldable
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Ext.Dev.Project

-- Track open files both in aggregate and per connection so disconnect cleanup
-- can remove leaked open state when a client disappears without didClose.
data EditorsOpen = EditorsOpen
  { openCounts :: Map.Map FilePath Int
  , openByConnection :: Map.Map Text.Text (Map.Map FilePath Int)
  }

empty :: EditorsOpen
empty = EditorsOpen Map.empty Map.empty

fileMarkedOpen :: Text.Text -> FilePath -> EditorsOpen -> EditorsOpen
fileMarkedOpen connId path (EditorsOpen counts byConnection) =
  let newCount = case Map.lookup path counts of
        Nothing -> 1
        Just n -> n + 1
      connFiles = Map.findWithDefault Map.empty connId byConnection
      connCount = case Map.lookup path connFiles of
        Nothing -> 1
        Just n -> n + 1
   in EditorsOpen
        (Map.insert path newCount counts)
        (Map.insert connId (Map.insert path connCount connFiles) byConnection)

fileMarkedClosed :: Text.Text -> FilePath -> EditorsOpen -> EditorsOpen
fileMarkedClosed connId path editors =
  case Map.lookup connId (openByConnection editors) of
    Nothing -> editors
    Just connFiles ->
      case Map.lookup path connFiles of
        Nothing -> editors
        Just _ ->
          let counts' = decrementN path 1 (openCounts editors)
              connFiles' = decrementN path 1 connFiles
              byConnection' =
                if Map.null connFiles'
                  then Map.delete connId (openByConnection editors)
                  else Map.insert connId connFiles' (openByConnection editors)
           in EditorsOpen counts' byConnection'

connectionClosed :: Text.Text -> EditorsOpen -> EditorsOpen
connectionClosed connId editors =
  case Map.lookup connId (openByConnection editors) of
    Nothing -> editors
    Just connFiles ->
      let counts' = Map.foldlWithKey' (\acc path n -> decrementN path n acc) (openCounts editors) connFiles
       in EditorsOpen counts' (Map.delete connId (openByConnection editors))

isFileOpen :: FilePath -> EditorsOpen -> Bool
isFileOpen path (EditorsOpen openFiles _) =
  case Map.lookup path openFiles of
    Just n -> n > 0
    Nothing -> False

-- Is the given project considered open by any open file?
isProjectOpen :: Ext.Dev.Project.Project -> EditorsOpen -> Bool
isProjectOpen proj (EditorsOpen openFiles _) =
  Data.Foldable.any (\p -> Ext.Dev.Project.contains p proj) (Map.keys openFiles)

toCounts :: EditorsOpen -> Map.Map FilePath Int
toCounts = openCounts

decrementN :: FilePath -> Int -> Map.Map FilePath Int -> Map.Map FilePath Int
decrementN path n m =
  case Map.lookup path m of
    Nothing -> m
    Just current ->
      let next = current - n
       in if next <= 0
            then Map.delete path m
            else Map.insert path next m
