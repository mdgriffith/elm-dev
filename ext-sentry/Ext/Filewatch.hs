{-# LANGUAGE OverloadedStrings #-}

module Ext.Filewatch where

import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import qualified Control.Concurrent.MVar as MVar
import qualified Control.FoldDebounce as Debounce
import Control.Monad (forever)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Ext.Common
import qualified System.FSNotify
import qualified System.FilePath
import qualified Ext.Log
import qualified Ext.Dev.Project as Project
import qualified System.Directory as Directory
import qualified System.IO.Unsafe as Unsafe
import qualified Watchtower.State.StartupMetrics as Metrics

data ActiveWatcher = ActiveWatcher
  { watcherThread :: ThreadId
  , registeredRoots :: MVar.MVar [FilePath]
  }

{-# NOINLINE activeWatchers #-}
activeWatchers :: MVar.MVar (Map.Map FilePath ActiveWatcher)
activeWatchers = Unsafe.unsafePerformIO (MVar.newMVar Map.empty)

watch :: FilePath -> ([FilePath] -> IO ()) -> IO ()
watch requestedRoot action = do
  root <- Directory.canonicalizePath requestedRoot
  MVar.modifyMVar_ activeWatchers $ \watchers ->
    case Map.toList (Map.filterWithKey (\watchedRoot _ -> watchedRoot `covers` root) watchers) of
      (_, existing) : _ -> do
        MVar.modifyMVar_ (registeredRoots existing) (pure . List.nub . (root :))
        pure watchers
      [] -> do
        let coveredChildren = Map.filterWithKey (\watchedRoot _ -> root `covers` watchedRoot) watchers
        childRoots <- concat <$> mapM (MVar.readMVar . registeredRoots) (Map.elems coveredChildren)
        mapM_ (killThread . watcherThread) (Map.elems coveredChildren)
        roots <- MVar.newMVar (List.nub (root : childRoots))
        threadId <- forkIO (watchLoop root roots action)
        Ext.Common.trackGhciThread threadId
        let updated = Map.insert root (ActiveWatcher threadId roots) (Map.difference watchers coveredChildren)
        Metrics.increment "watchers.started"
        Metrics.setGauge "watchers.active_roots" (Map.size updated)
        pure updated

watchLoop :: FilePath -> MVar.MVar [FilePath] -> ([FilePath] -> IO ()) -> IO ()
watchLoop root roots action =
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
        isRelevantEvent -- predicate
        (\event -> do
            let path = getEventFilePath event
            explicitRoots <- MVar.readMVar roots
            if shouldTriggerPathRelativeTo root path || any (\explicitRoot -> explicitRoot /= root && explicitRoot `covers` path && shouldTriggerPathRelativeTo explicitRoot path) explicitRoots
              then do
                Ext.Log.log Ext.Log.FileWatch (toString event)
                Debounce.send trigger path
              else pure ()
        )

      -- sleep forever (until interrupted)
      forever $ threadDelay 1000000

covers :: FilePath -> FilePath -> Bool
covers parent child =
  let normalizedParent = System.FilePath.normalise parent
      normalizedChild = System.FilePath.normalise child
      parentWithSeparator = System.FilePath.addTrailingPathSeparator normalizedParent
  in normalizedChild == normalizedParent || parentWithSeparator `List.isPrefixOf` normalizedChild



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
    shouldTriggerPath (getEventFilePath event)

isRelevantEvent :: System.FSNotify.Event -> Bool
isRelevantEvent event =
  let path = getEventFilePath event
      base = System.FilePath.takeFileName path
  in System.FilePath.takeExtension path == ".elm" || base == "elm.json" || base == "elm.dev.json"

shouldTriggerForRoot :: FilePath -> System.FSNotify.Event -> Bool
shouldTriggerForRoot root event =
  shouldTriggerPathRelativeTo root (getEventFilePath event)

shouldTriggerPath :: FilePath -> Bool
shouldTriggerPath path =
    let base = System.FilePath.takeFileName path
        ext = System.FilePath.takeExtension path
        segments = System.FilePath.splitDirectories (System.FilePath.normalise path)
    in (ext == ".elm" || base == "elm.json" || base == "elm.dev.json")
        && not (any Project.isExcludedDirectoryName segments)

shouldTriggerPathRelativeTo :: FilePath -> FilePath -> Bool
shouldTriggerPathRelativeTo root path =
  let relative = System.FilePath.makeRelative root path
      base = System.FilePath.takeFileName relative
      ext = System.FilePath.takeExtension relative
      directorySegments = System.FilePath.splitDirectories (System.FilePath.takeDirectory relative)
  in (ext == ".elm" || base == "elm.json" || base == "elm.dev.json")
      && not (any Project.isExcludedDirectoryName directorySegments)
