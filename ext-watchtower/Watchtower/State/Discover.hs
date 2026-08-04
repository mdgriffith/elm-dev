{-# LANGUAGE OverloadedStrings #-}

module Watchtower.State.Discover (discover, invalidate, discoverTests) where

import qualified Control.Concurrent.STM as STM
import Control.Monad as Monad
import Data.Function ((&))
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Ext.CompileHelpers.Generic as CompileHelpers
import qualified Ext.Dev.Project
import qualified Ext.Log
import qualified Ext.Sentry
import qualified System.Directory as Dir
import qualified Watchtower.Live.Client as Client
import qualified Watchtower.Live.Compile
import qualified Watchtower.State.Project
import qualified Watchtower.Websocket
import qualified Ext.Test.Compile
import qualified Ext.Test.Introspect
import qualified Ext.Test.Discover as TestDiscover
import qualified Control.Concurrent.STM as STM
import qualified Data.NonEmptyList as NE
import qualified Elm.ModuleName as ModuleName
import qualified Ext.Trace as PerfTrace
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Exception as Exception
import qualified System.IO.Unsafe as Unsafe
import qualified System.FilePath as FilePath
import qualified Watchtower.State.StartupMetrics as Metrics
import qualified Data.Set as Set

{-# NOINLINE discoveryCache #-}
discoveryCache :: MVar.MVar (Map.Map FilePath (MVar.MVar ()))
discoveryCache = Unsafe.unsafePerformIO (MVar.newMVar Map.empty)

{-# NOINLINE workspaceProjects #-}
workspaceProjects :: MVar.MVar (Map.Map FilePath (Set.Set FilePath))
workspaceProjects = Unsafe.unsafePerformIO (MVar.newMVar Map.empty)

discover :: Client.State -> FilePath -> IO ()
discover state root = do
  canonicalRoot <- Dir.canonicalizePath root
  (scanFinished, shouldScan) <- MVar.modifyMVar discoveryCache $ \cached ->
    case Map.lookup canonicalRoot cached of
      Just existing -> pure (cached, (existing, False))
      Nothing -> do
        pending <- MVar.newEmptyMVar
        pure (Map.insert canonicalRoot pending cached, (pending, True))
  if shouldScan
    then Exception.onException
      (do
        Metrics.increment "discovery.scans"
        discoverUncached state canonicalRoot
        MVar.putMVar scanFinished ()
      )
      (do
        MVar.modifyMVar_ discoveryCache (pure . Map.delete canonicalRoot)
        MVar.tryPutMVar scanFinished () >>= \_ -> pure ()
      )
    else do
      Metrics.increment "discovery.cache_hits"
      MVar.readMVar scanFinished

invalidate :: Client.State -> FilePath -> IO ()
invalidate state changedPath = do
  absolutePath <- Dir.makeAbsolute changedPath
  roots <- MVar.modifyMVar discoveryCache $ \cached -> do
    let affected = filter (`containsPath` absolutePath) (Map.keys cached)
    pure (foldr Map.delete cached affected, affected)
  mapM_ (discover state) roots

containsPath :: FilePath -> FilePath -> Bool
containsPath root path =
  let normalizedRoot = FilePath.normalise root
      normalizedPath = FilePath.normalise path
      rootWithSeparator = FilePath.addTrailingPathSeparator normalizedRoot
  in normalizedPath == normalizedRoot || rootWithSeparator `List.isPrefixOf` normalizedPath

discoverUncached :: Client.State -> FilePath -> IO ()
discoverUncached state canonicalRoot = do
  Ext.Log.log Ext.Log.Live ("👀 discover requested: " <> canonicalRoot)
  projects <- Ext.Dev.Project.discover canonicalRoot
  Metrics.add "discovery.projects_found" (length projects)
  let discoveredRoots = Set.fromList (map Ext.Dev.Project.getRoot projects)
  removedRoots <- MVar.modifyMVar workspaceProjects $ \known -> do
    let previous = Map.findWithDefault Set.empty canonicalRoot known
        knownWithoutCurrent = Map.delete canonicalRoot known
        ownedElsewhere = Set.unions (Map.elems knownWithoutCurrent)
        removed = Set.toList (Set.difference (Set.difference previous discoveredRoots) ownedElsewhere)
    pure (Map.insert canonicalRoot discoveredRoots known, removed)
  missingRoots <- Monad.filterM (fmap not . Dir.doesFileExist . (FilePath.</> "elm.json")) removedRoots
  STM.atomically $ STM.modifyTVar' (Client.projects state)
    (filter (\(Client.ProjectCache project _ _ _ _) -> Ext.Dev.Project.getRoot project `notElem` missingRoots))

  let projectTails = zipWith (\ix proj -> show ix ++ ":" ++ getProjectShorthand canonicalRoot proj) [0..] projects

  if List.null projectTails
    then Ext.Log.log Ext.Log.Live "found no projects"
    else Ext.Log.log Ext.Log.Live (("found projects (" ++ canonicalRoot ++ ")") <> Ext.Log.formatList projectTails)

  Monad.foldM_ (initializeProject state canonicalRoot) [] projects
  

initializeProject :: Client.State -> FilePath -> [Client.ProjectCache] -> Ext.Dev.Project.Project -> IO [Client.ProjectCache]
initializeProject state workspaceRoot accum project = do
  let flags = CompileHelpers.Flags CompileHelpers.Dev CompileHelpers.NoOutput CompileHelpers.DebuggerNone
  result <- Watchtower.State.Project.upsertWithWatchRoot state workspaceRoot (invalidate state) flags (Ext.Dev.Project._root project) (Ext.Dev.Project._entrypoints project)
  case result of
    Left _ ->
      pure accum
    Right projectCache ->
      do
        -- Populate tests for this project (best-effort)
        discoverTests state projectCache
        registerTraceDiscoveredProject state project projectCache
        pure (projectCache : accum)

registerTraceDiscoveredProject :: Client.State -> Ext.Dev.Project.Project -> Client.ProjectCache -> IO ()
registerTraceDiscoveredProject state discoveredProject (Client.ProjectCache cachedProject _ _ _ _) = do
  PerfTrace.project
    (Client.trace state)
    (Ext.Dev.Project._root discoveredProject)
    (Ext.Dev.Project._projectRoot discoveredProject)
    (NE.toList (Ext.Dev.Project._entrypoints discoveredProject))
    (Ext.Dev.Project._srcDirs discoveredProject)
    (Ext.Dev.Project._shortId cachedProject)
    []

-- | Discover test suites for a project and store TestInfo on its cache
discoverTests :: Client.State -> Client.ProjectCache -> IO ()
discoverTests _state (Client.ProjectCache proj _ _ _ mTestVar) = do
  let rootDir = Ext.Dev.Project.getRoot proj
  testFiles <- TestDiscover.discoverTestFiles rootDir
  case testFiles of
    [] -> do
      STM.atomically $ STM.writeTVar mTestVar Nothing
    (x:xs) -> do
      STM.atomically $ STM.writeTVar mTestVar (Just (Client.TestInfo
        { Client.testFiles = testFiles
        , Client.testResults = Nothing
        , Client.testCompilation = Nothing
        }))

getProjectShorthand :: FilePath -> Ext.Dev.Project.Project -> FilePath
getProjectShorthand root proj =
  case List.stripPrefix root (Ext.Dev.Project.getRoot proj) of
    Nothing -> "."
    Just "" -> "."
    Just str ->
      str
