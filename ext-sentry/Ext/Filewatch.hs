{-# LANGUAGE OverloadedStrings #-}

module Ext.Filewatch where

import Control.Concurrent (forkIO, threadDelay)
import qualified Control.FoldDebounce as Debounce
import Control.Monad (forever)
import qualified Data.List as List
import qualified Ext.Common
import qualified System.FSNotify
import qualified System.FilePath
import qualified Ext.Log

watch :: FilePath -> ([FilePath] -> IO ()) -> IO ()
watch root action =
  Ext.Common.trackedForkIO $
     System.FSNotify.withManager $ \mgr -> do
      trigger <-
        Debounce.new
          Debounce.Args
            { Debounce.cb = (\events -> action events),
              Debounce.fold = (\l v -> List.nub $ v : l),
              Debounce.init = []
            }
          Debounce.def
            { Debounce.delay = 50 * 1000, -- milliseconds
              Debounce.alwaysResetTimer = True
            }

      -- start a watching job (in the background)
      System.FSNotify.watchTree
        mgr -- manager
        root -- directory to watch
        shouldTrigger -- predicate
        (\event -> do
            Ext.Log.log Ext.Log.FileWatch (toString event)
            Debounce.send trigger (getEventFilePath event)
        )

      -- sleep forever (until interrupted)
      forever $ threadDelay 1000000



toString :: System.FSNotify.Event -> String
toString event =
  case event of
    System.FSNotify.Added filepath _ _                   -> "Added " <> System.FilePath.takeFileName filepath
    System.FSNotify.Modified filepath _ _                -> "Modified " <> System.FilePath.takeFileName filepath
    System.FSNotify.ModifiedAttributes filepath _ _      -> "ModifiedAttributes " <> System.FilePath.takeFileName filepath
    System.FSNotify.Removed filepath _ _                 -> "Removed " <> System.FilePath.takeFileName filepath
    System.FSNotify.WatchedDirectoryRemoved filepath _ _ -> "WatchedDirectoryRemoved" <> System.FilePath.takeFileName filepath
    System.FSNotify.CloseWrite filepath _ _              -> "CloseWrite" <> System.FilePath.takeFileName filepath
    System.FSNotify.Unknown filepath _ _ _               -> "Unknown " <> System.FilePath.takeFileName filepath

getEventFilePath :: System.FSNotify.Event -> FilePath
getEventFilePath event =
  case event of
    System.FSNotify.Added filepath _ _                   -> filepath
    System.FSNotify.Modified filepath _ _                -> filepath
    System.FSNotify.ModifiedAttributes filepath _ _      -> filepath
    System.FSNotify.Removed filepath _ _                 -> filepath
    System.FSNotify.WatchedDirectoryRemoved filepath _ _ -> filepath
    System.FSNotify.CloseWrite filepath _ _              -> filepath
    System.FSNotify.Unknown filepath _ _ _               -> filepath


shouldTrigger :: System.FSNotify.Event -> Bool
shouldTrigger event =
    let
        path = getEventFilePath event
    in
    not (List.isInfixOf ".git" path)
        && not (List.isInfixOf "elm-stuff" path)
        && not (List.isInfixOf "node_modules" path)