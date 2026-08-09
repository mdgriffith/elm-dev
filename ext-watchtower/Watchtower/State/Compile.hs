{-# LANGUAGE OverloadedStrings #-}

module Watchtower.State.Compile (compile, compileRelevantProjects, compileRelevantProjectsWithPrimaryCallback, scheduleDebouncedCompileRelevantProjects, scheduleDebouncedCompileRelevantProjectsWithCallback, scheduleDebouncedCompileRelevantProjectsWithCallbacks, updateVfsFromFs, ensureProjectFresh, compileTests, markFilesystemChanged, clearTestResults, updateProjectFileInfo) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LBS
import qualified Data.NonEmptyList as NE
import qualified Data.Map.Strict as Map
import qualified Ext.CompileHelpers.Generic as CompileHelpers
import qualified Ext.CompileProxy as CompileProxy
import qualified Ext.Dev.Project
import qualified Ext.Dev.Project as Project
import qualified Ext.Sentry as Sentry
import qualified Control.Concurrent.STM as STM
import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Ext.Common
import qualified Gen.Generate
import Json.Encode ((==>))
import qualified Json.Encode as Json
import qualified Reporting.Exit as Exit
import qualified System.Directory as Dir (withCurrentDirectory, doesFileExist, getDirectoryContents, doesDirectoryExist)
import qualified Watchtower.Live.Client as Client
import qualified Reporting.Warning as Warning
import qualified Watchtower.Server.DevWS as DevWS
import qualified Ext.Test.Compile as TestCompile
import qualified Ext.Log
import qualified System.FilePath as FP
import qualified Ext.FileCache
import qualified Watchtower.State.Versions as Versions
import qualified Watchtower.State.StartupMetrics as Metrics
import qualified Watchtower.Trace as Trace
import qualified Control.Monad as Monad
import qualified Ext.Test.Discover
import qualified Data.Set as Set
import qualified Control.Concurrent.MVar as MVar
import qualified System.IO.Unsafe as Unsafe
import qualified Control.Concurrent as Concurrent
import qualified Ext.Trace as PerfTrace
-- no docs fetching needed from Ext.Dev; docs come from CompileProxy


compile :: Client.State -> String -> Client.ProjectCache -> [FilePath] -> IO (Either Client.Error CompileHelpers.CompilationResult)
compile state traceId projCache@(Client.ProjectCache (Ext.Dev.Project.Project projectRoot elmJsonRoot _entrypoints _srcDirs shortId) _docsInfo _flags _mCompileResult _) files =
  PerfTrace.span
    (Client.trace state)
    "compile.project"
    [ PerfTrace.text "trace_id" traceId
    , PerfTrace.text "project_root" projectRoot
    , PerfTrace.text "elm_json_root" elmJsonRoot
    , PerfTrace.int "project_short_id" shortId
    , PerfTrace.int "changed_file_count" (length files)
    ]
    (compileUntraced state traceId projCache files)

compileUntraced :: Client.State -> String -> Client.ProjectCache -> [FilePath] -> IO (Either Client.Error CompileHelpers.CompilationResult)
compileUntraced state@(Client.State _ _ mFileInfo mPackages _ _ _ mWorkspaceDiagsRequested _) traceId projCache@(Client.ProjectCache proj@(Ext.Dev.Project.Project projectRoot elmJsonRoot _entrypoints _srcDirs _shortId) docsInfo flags mCompileResult _) files = do
  versionsAtStart <- Versions.readVersions projectRoot
  let fsSnapshot = Versions.fsVersion versionsAtStart
  Ext.Log.log Ext.Log.Live
    ( concat
        [ "[trace " ++ traceId ++ "] compile.start"
        , " root=" ++ projectRoot
        , " files=" ++ Trace.formatPaths files
        , " fsVersion=" ++ show (Versions.fsVersion versionsAtStart)
        , " compileVersion=" ++ show (Versions.compileVersion versionsAtStart)
        ]
    )
  let markCompileSnapshot = do
        Versions.setCompileVersionTo projectRoot fsSnapshot
  Dir.withCurrentDirectory projectRoot $ do
    -- First run code generation
    codegenResult <- Gen.Generate.run projectRoot
    case codegenResult of
      Right () -> do
        entrypointGroups <- Ext.Dev.Project.entrypointGroupsForChangedFilesAtVersion fsSnapshot files proj
        if null entrypointGroups
          then do
            STM.atomically $ do
              STM.writeTVar mCompileResult (Client.Success CompileHelpers.CompiledSkippedOutput)
              cur <- STM.readTVar mWorkspaceDiagsRequested
              STM.writeTVar mWorkspaceDiagsRequested (Map.map markDiagnosticsOutOfDate cur)
            markCompileSnapshot
            versionsAtEnd <- Versions.readVersions projectRoot
            Ext.Log.log Ext.Log.Live
              ( concat
                  [ "[trace " ++ traceId ++ "] compile.no_affected_entrypoints"
                  , " root=" ++ projectRoot
                  , " files=" ++ Trace.formatPaths files
                  , " fsVersion=" ++ show (Versions.fsVersion versionsAtEnd)
                  , " compileVersion=" ++ show (Versions.compileVersion versionsAtEnd)
                  ]
              )
            pure (Right CompileHelpers.CompiledSkippedOutput)
          else do
            compileResults <- Monad.forM entrypointGroups $ \entrypointGroup -> do
              compiled <- CompileProxy.compile elmJsonRoot entrypointGroup flags (Just mPackages)
              pure (entrypointGroupPath entrypointGroup, compiled)
            let eitherCompiled = combineCompileResults compileResults
                fileInfoByPath = combineFileInfo compileResults
                targetResults = fmap targetCompilationResult compileResults

            let newResult = Client.TargetResults targetResults
            STM.atomically $ STM.writeTVar mCompileResult newResult

            -- Replace this project's file info on successful compile so stale ASTs
            -- from renamed or deleted files do not accumulate indefinitely.
            STM.atomically $ do
              current <- STM.readTVar mFileInfo
              STM.writeTVar mFileInfo (updateProjectFileInfo proj (shouldReplaceProjectFileInfo files) eitherCompiled current fileInfoByPath)

            -- Mark workspace diagnostics snapshots as out-of-date after compile result and file info updates
            STM.atomically $ do
              cur <- STM.readTVar mWorkspaceDiagsRequested
              let updated =
                    Map.map
                      (\s -> Client.LspSession
                        { Client.workspaceDiagnosticsSnapshotFiles = Client.workspaceDiagnosticsSnapshotFiles s
                        , Client.workspaceDiagnosticsSnapshotOutOfDate = True
                        , Client.publishedDiagnosticFiles = Client.publishedDiagnosticFiles s
                        , Client.lspRoot = Client.lspRoot s
                        }
                      )
                      cur
              STM.writeTVar mWorkspaceDiagsRequested updated

            broadcastTargetResults state targetResults

            case eitherCompiled of
              Right result -> do
                markCompileSnapshot
                versionsAtEnd <- Versions.readVersions projectRoot
                Ext.Log.log Ext.Log.Live
                  ( concat
                      [ "[trace " ++ traceId ++ "] compile.success"
                      , " root=" ++ projectRoot
                      , " fsVersion=" ++ show (Versions.fsVersion versionsAtEnd)
                      , " compileVersion=" ++ show (Versions.compileVersion versionsAtEnd)
                      ]
                  )
                pure (Right result)
              Left exit -> do
                -- Broadcast error to Dev websocket clients
                let clientErr = Client.ReactorError exit
                let errJson = Client.encodeCompilationResult (Client.Error clientErr)
                DevWS.broadcastCompilationError state errJson
                markCompileSnapshot
                versionsAtEnd <- Versions.readVersions projectRoot
                Ext.Log.log Ext.Log.Live
                  ( concat
                      [ "[trace " ++ traceId ++ "] compile.error"
                      , " root=" ++ projectRoot
                      , " fsVersion=" ++ show (Versions.fsVersion versionsAtEnd)
                      , " compileVersion=" ++ show (Versions.compileVersion versionsAtEnd)
                      ]
                  )
                pure (Left clientErr)
      Left err -> do
        -- Update compile result TVar with the error
        STM.atomically $ STM.writeTVar mCompileResult (Client.Error (Client.GenerationError err))
        -- On generation failure, also clear FileInfo entries for this project
        STM.atomically $ do
          current <- STM.readTVar mFileInfo
          let filtered = Map.filterWithKey (\path _ -> not (Ext.Dev.Project.contains path proj)) current
          STM.writeTVar mFileInfo filtered
        -- Broadcast generation error to Dev websocket clients
        let clientErr = Client.GenerationError err
        let errJson = Client.encodeCompilationResult (Client.Error clientErr)
        DevWS.broadcastCompilationError state errJson
        -- Mark workspace diagnostics snapshots as out-of-date on generation error
        STM.atomically $ do
          cur <- STM.readTVar mWorkspaceDiagsRequested
          let updated =
                Map.map
                  (\s -> Client.LspSession
                    { Client.workspaceDiagnosticsSnapshotFiles = Client.workspaceDiagnosticsSnapshotFiles s
                    , Client.workspaceDiagnosticsSnapshotOutOfDate = True
                    , Client.publishedDiagnosticFiles = Client.publishedDiagnosticFiles s
                    , Client.lspRoot = Client.lspRoot s
                    }
                  )
                  cur
          STM.writeTVar mWorkspaceDiagsRequested updated
        markCompileSnapshot
        versionsAtEnd <- Versions.readVersions projectRoot
        Ext.Log.log Ext.Log.Live
          ( concat
              [ "[trace " ++ traceId ++ "] compile.codegen_error"
              , " root=" ++ projectRoot
              , " fsVersion=" ++ show (Versions.fsVersion versionsAtEnd)
              , " compileVersion=" ++ show (Versions.compileVersion versionsAtEnd)
              ]
          )
        pure $ Left clientErr

entrypointGroupPath :: NE.List FilePath -> FilePath
entrypointGroupPath (NE.List entrypoint _) =
  entrypoint

targetCompilationResult :: (FilePath, (Either Exit.Reactor CompileHelpers.CompilationResult, Map.Map FilePath Client.FileInfo)) -> (FilePath, Client.CompilationResult)
targetCompilationResult (entrypoint, (eitherCompiled, _)) =
  case eitherCompiled of
    Right result -> (entrypoint, Client.Success result)
    Left exit -> (entrypoint, Client.Error (Client.ReactorError exit))

broadcastTargetResults :: Client.State -> [(FilePath, Client.CompilationResult)] -> IO ()
broadcastTargetResults state results =
  Monad.mapM_ broadcastOne results
  where
    broadcastOne (entrypoint, result) =
      case result of
        Client.Success (CompileHelpers.CompiledJs jsBuilder) ->
          DevWS.broadcastCompiledTarget state entrypoint (Client.builderToString jsBuilder)
        Client.Error (Client.ReactorError _) ->
          DevWS.broadcastCompilationTargetError state entrypoint (Client.encodeCompilationResult result)
        _ -> pure ()

combineCompileResults :: [(FilePath, (Either Exit.Reactor CompileHelpers.CompilationResult, Map.Map FilePath Client.FileInfo))] -> Either Exit.Reactor CompileHelpers.CompilationResult
combineCompileResults results =
  case [err | (_, (Left err, _)) <- results] of
    err : _ -> Left err
    [] ->
      case [result | (_, (Right result, _)) <- results] of
        [] -> Right CompileHelpers.CompiledSkippedOutput
        successes -> Right (last successes)

combineFileInfo :: [(FilePath, (Either Exit.Reactor CompileHelpers.CompilationResult, Map.Map FilePath Client.FileInfo))] -> Map.Map FilePath Client.FileInfo
combineFileInfo results =
  Map.unions [fileInfoByPath | (_, (_, fileInfoByPath)) <- results]

{-# NOINLINE projectCompileLocks #-}
projectCompileLocks :: MVar.MVar (Map.Map FilePath (MVar.MVar ()))
projectCompileLocks = Unsafe.unsafePerformIO (MVar.newMVar Map.empty)

{-# NOINLINE projectTestCompileLocks #-}
projectTestCompileLocks :: MVar.MVar (Map.Map FilePath (MVar.MVar ()))
projectTestCompileLocks = Unsafe.unsafePerformIO (MVar.newMVar Map.empty)

data PendingCompile = PendingCompile
  { pendingPaths :: Set.Set FilePath
  , pendingSubscribers :: Map.Map String (Bool -> IO (), Bool -> IO ())
  , pendingWorker :: Concurrent.ThreadId
  }

{-# NOINLINE projectCompileDebounces #-}
projectCompileDebounces :: MVar.MVar (Map.Map String PendingCompile)
projectCompileDebounces = Unsafe.unsafePerformIO (MVar.newMVar Map.empty)

withProjectCompileLockIfAvailable :: FilePath -> IO a -> IO (Maybe a)
withProjectCompileLockIfAvailable projectRoot action = do
  lock <- MVar.modifyMVar projectCompileLocks $ \locks ->
    case Map.lookup projectRoot locks of
      Just existing -> pure (locks, existing)
      Nothing -> do
        newLock <- MVar.newMVar ()
        pure (Map.insert projectRoot newLock locks, newLock)
  MVar.withMVar lock (\_ -> Just <$> action)

withProjectTestCompileLock :: FilePath -> IO a -> IO a
withProjectTestCompileLock projectRoot action = do
  lock <- MVar.modifyMVar projectTestCompileLocks $ \locks ->
    case Map.lookup projectRoot locks of
      Just existing -> pure (locks, existing)
      Nothing -> do
        newLock <- MVar.newMVar ()
        pure (Map.insert projectRoot newLock locks, newLock)
  MVar.withMVar lock (\_ -> action)

markFilesystemChanged :: Client.State -> [FilePath] -> IO ()
markFilesystemChanged (Client.State _ mProjects _ _ _ _ _ _ _) changedPaths = do
  if List.null changedPaths
    then pure ()
    else do
      projects <- STM.readTVarIO mProjects
      let touchedRoots =
            Set.toList
              ( Set.fromList
                  ( map
                      (\(Client.ProjectCache proj _ _ _ _) -> Ext.Dev.Project.getRoot proj)
                      ( List.filter
                          (\(Client.ProjectCache proj _ _ _ _) -> any (\p -> Ext.Dev.Project.affectsCompilation p proj) changedPaths)
                          projects
                       )
                  )
              )
      Monad.mapM_ Versions.bumpFsVersion touchedRoots
      STM.atomically $
        Monad.mapM_
          (\(Client.ProjectCache proj _ _ _ mTestVar) ->
            if Ext.Dev.Project.getRoot proj `elem` touchedRoots
              then clearTestResults mTestVar
              else pure ()
          )
          projects

clearTestResults :: STM.TVar (Maybe Client.TestInfo) -> STM.STM ()
clearTestResults mTestVar = do
  current <- STM.readTVar mTestVar
  case current of
    Nothing -> pure ()
    Just info ->
      STM.writeTVar mTestVar (Just info { Client.testResults = Nothing, Client.testCompilation = Nothing })

-- | Recursively gather Elm source files under a directory.
listElmFilesRecursive :: FilePath -> IO [FilePath]
listElmFilesRecursive dir = do
  isDir <- Dir.doesDirectoryExist dir
  if not isDir
    then pure []
    else do
      contents <- Dir.getDirectoryContents dir
      let paths = map (dir FP.</>) (filter (\p -> p /= "." && p /= "..") contents)
      files <- Monad.filterM Dir.doesFileExist paths
      dirs <- Monad.filterM Dir.doesDirectoryExist paths
      let elmFiles = filter (\p -> FP.takeExtension p == ".elm") files
      nested <- Monad.mapM listElmFilesRecursive dirs
      pure (elmFiles ++ List.concat nested)

-- | Update the virtual file system cache from the real filesystem for a project.
--   This is very lightweight: it enumerates Elm source files under srcDirs plus key config files,
--   and upserts any changed files into Ext.FileCache. Returns True if any files changed.
updateVfsFromFs :: Ext.Dev.Project.Project -> IO Bool
updateVfsFromFs (Ext.Dev.Project.Project projectRoot _ _ srcDirs _) = do
  elmFilesNested <- Monad.mapM listElmFilesRecursive srcDirs
  let elmFiles = List.concat elmFilesNested
  -- Include test files under projectRoot/tests as part of VFS verification
  testFiles <- Ext.Test.Discover.discoverTestFiles projectRoot
  let configFiles =
        [ projectRoot FP.</> "elm.json"
        , projectRoot FP.</> "elm.dev.json"
        ]
  existingConfigs <- Monad.filterM Dir.doesFileExist configFiles
  let files = elmFiles ++ testFiles ++ existingConfigs
  changed <- Ext.FileCache.handleIfChanged files (\changedPaths -> pure changedPaths)
  case changed of
    [] -> pure False
    _  -> do
      -- Log each changed file
      Monad.mapM_ (\path -> Ext.Log.log Ext.Log.FileWatch ("File updated: " ++ path)) changed
      -- bump filesystem version when anything changed
      _ <- Versions.bumpFsVersion projectRoot
      pure True

ensureProjectFresh :: Client.State -> String -> Client.ProjectCache -> IO Bool
ensureProjectFresh state traceId pc@(Client.ProjectCache proj _ _ mCompileResult mTestVar) = do
  vfsChanged <- updateVfsFromFs proj
  Monad.when vfsChanged $
    STM.atomically $ clearTestResults mTestVar
  let projectRoot = Ext.Dev.Project.getRoot proj
  versions <- Versions.readVersions projectRoot
  currentResult <- STM.readTVarIO mCompileResult
  let needsCompile =
        Versions.compileVersion versions < Versions.fsVersion versions
          || case currentResult of
               Client.NotCompiled -> True
               _ -> False
  Monad.when needsCompile $ do
    _ <- compile state traceId pc []
    pure ()
  pure vfsChanged


-- | Compile any projects that are relevant to the given file paths.
--   A project is considered relevant if it contains at least one of the provided files.
--   Compilation is performed synchronously here so the caller can rely on
--   fresh results when this function returns.
compileRelevantProjects :: Client.State -> String -> [FilePath] -> IO Bool
compileRelevantProjects state traceId elmFiles =
  compileRelevantProjectsWithPrimaryCallback state traceId elmFiles (\_ -> pure ())

compileRelevantProjectsWithPrimaryCallback :: Client.State -> String -> [FilePath] -> (Bool -> IO ()) -> IO Bool
compileRelevantProjectsWithPrimaryCallback state traceId elmFiles afterPrimaryCompile =
  PerfTrace.span
    (Client.trace state)
    "compile.relevant_projects"
    [ PerfTrace.text "trace_id" traceId
    , PerfTrace.int "changed_file_count" (length elmFiles)
    ]
    (compileRelevantProjectsUntraced state traceId elmFiles afterPrimaryCompile)

compileRelevantProjectsUntraced :: Client.State -> String -> [FilePath] -> (Bool -> IO ()) -> IO Bool
compileRelevantProjectsUntraced state@(Client.State _ mProjects _ _ _ _ _ _ _) traceId elmFiles afterPrimaryCompile = do
  if elmFiles == []
    then pure False
    else do
      projects <- STM.readTVarIO mProjects
      let relevant = List.filter (projectTouchesAny elmFiles) projects
          ownerProjects = List.filter (projectOwnedByAny elmFiles relevant) relevant
          downstreamProjects = filterOutProjects ownerProjects relevant
      Ext.Log.log Ext.Log.Live
        ( concat
            [ "[trace " ++ traceId ++ "] compileRelevantProjects"
            , " changed=" ++ Trace.formatPaths elmFiles
            , " relevantProjects=" ++ show (length relevant)
            , " ownerProjects=" ++ show (length ownerProjects)
            , " downstreamProjects=" ++ show (length downstreamProjects)
            ]
        )
      case relevant of
        [] -> do 
          Ext.Log.log Ext.Log.Live ("[trace " ++ traceId ++ "] No relevant projects to compile")
          pure False
        _ ->
          Ext.Common.track "compile relevant projects" $ do
            anyCompiled <- STM.newTVarIO False
            compileResults <- STM.newTVarIO []
            let runOne projCache = do
                  result@(didCompile, _lastPrimarySucceeded, _projectCache) <- compileProjectFiles elmFiles projCache
                  Monad.when didCompile $ do
                    Metrics.increment "compile.production_projects"
                    STM.atomically (STM.writeTVar anyCompiled True)
                  STM.atomically (STM.modifyTVar' compileResults (result :))

            Monad.mapM_ runOne ownerProjects
            ownerDidCompile <- STM.readTVarIO anyCompiled
            PerfTrace.event
              (Client.trace state)
              "compile.relevant_projects.owner_phase_complete"
              [ PerfTrace.text "trace_id" traceId
              , PerfTrace.int "owner_project_count" (length ownerProjects)
              , PerfTrace.int "downstream_project_count" (length downstreamProjects)
              , PerfTrace.bool "owner_did_compile" ownerDidCompile
              , PerfTrace.text "owner_roots" (List.intercalate "," (map projectRootForCache ownerProjects))
              ]
            afterPrimaryCompile ownerDidCompile

            counter <- STM.newTVarIO (List.length downstreamProjects)
            let runDownstream projCache = do
                  runOne projCache
                  STM.atomically $ do
                    n <- STM.readTVar counter
                    STM.writeTVar counter (n - 1)
            mapM_ (\proj -> Ext.Common.trackedForkIO (runDownstream proj)) downstreamProjects
            STM.atomically $ do
              n <- STM.readTVar counter
              STM.check (n == 0)
            didCompile <- STM.readTVarIO anyCompiled
            results <- STM.readTVarIO compileResults
            runTestsForCompileResults results
            pure didCompile
  where
    projectTouchesAny :: [FilePath] -> Client.ProjectCache -> Bool
    projectTouchesAny paths (Client.ProjectCache proj _ _ _ _) =
      any (\p -> Ext.Dev.Project.affectsCompilation p proj) paths

    projectOwnedByAny :: [FilePath] -> [Client.ProjectCache] -> Client.ProjectCache -> Bool
    projectOwnedByAny paths allRelevant candidate =
      any
        (\path ->
            case nearestOwner path allRelevant of
              Just owner -> sameProjectCache owner candidate
              Nothing -> False
        )
        paths

    nearestOwner :: FilePath -> [Client.ProjectCache] -> Maybe Client.ProjectCache
    nearestOwner path =
      Maybe.listToMaybe
        . List.sortBy
            (\(Client.ProjectCache one _ _ _ _) (Client.ProjectCache two _ _ _ _) ->
                compare (length (Ext.Dev.Project.getRoot two)) (length (Ext.Dev.Project.getRoot one))
            )
        . List.filter (\(Client.ProjectCache proj _ _ _ _) -> Ext.Dev.Project.contains path proj)

    sameProjectCache :: Client.ProjectCache -> Client.ProjectCache -> Bool
    sameProjectCache (Client.ProjectCache one _ _ _ _) (Client.ProjectCache two _ _ _ _) =
      Ext.Dev.Project.equal one two

    projectRootForCache :: Client.ProjectCache -> FilePath
    projectRootForCache (Client.ProjectCache proj _ _ _ _) =
      Ext.Dev.Project.getRoot proj

    filterOutProjects :: [Client.ProjectCache] -> [Client.ProjectCache] -> [Client.ProjectCache]
    filterOutProjects excluded =
      List.filter (\project -> not (any (sameProjectCache project) excluded))

    compileProjectFiles :: [FilePath] -> Client.ProjectCache -> IO (Bool, Maybe Bool, Client.ProjectCache)
    compileProjectFiles paths projCache@(Client.ProjectCache proj _ _ mCompileResult _) = do
      let projectRoot = Ext.Dev.Project.getRoot proj
          projectFiles = List.filter (\p -> Ext.Dev.Project.affectsCompilation p proj) paths
          projectShortId = Ext.Dev.Project._shortId proj
          compileUntilClean filesForIteration didCompileAny lastPrimarySucceeded iteration = do
            versions <- Versions.readVersions projectRoot
            if Versions.compileVersion versions < Versions.fsVersion versions
              then do
                PerfTrace.event
                  (Client.trace state)
                  "compile.loop_iteration"
                  [ PerfTrace.text "trace_id" traceId
                  , PerfTrace.text "project_root" projectRoot
                  , PerfTrace.int "project_short_id" projectShortId
                  , PerfTrace.int "iteration" iteration
                  , PerfTrace.int "fs_version" (Versions.fsVersion versions)
                  , PerfTrace.int "compile_version" (Versions.compileVersion versions)
                 , PerfTrace.int "changed_file_count" (length filesForIteration)
                  ]
                Ext.Log.log Ext.Log.Live
                  ( concat
                      [ "[trace " ++ traceId ++ "] compiling project"
                      , " root=" ++ projectRoot
                      , " files=" ++ Trace.formatPaths filesForIteration
                      , " fsVersion=" ++ show (Versions.fsVersion versions)
                      , " compileVersion=" ++ show (Versions.compileVersion versions)
                      ]
                  )
                compileResult <- compile state traceId projCache filesForIteration
                -- A later iteration means the filesystem advanced while compiling.
                -- Compile all entrypoints because the newer changed paths are not in this request.
                compileUntilClean [] True (Just (Either.isRight compileResult)) (iteration + 1)
              else do
                PerfTrace.event
                  (Client.trace state)
                  "compile.loop_clean"
                  [ PerfTrace.text "trace_id" traceId
                  , PerfTrace.text "project_root" projectRoot
                  , PerfTrace.int "project_short_id" projectShortId
                  , PerfTrace.int "iteration_count" iteration
                  , PerfTrace.int "fs_version" (Versions.fsVersion versions)
                  , PerfTrace.int "compile_version" (Versions.compileVersion versions)
                  , PerfTrace.bool "did_compile" didCompileAny
                  , PerfTrace.bool "primary_succeeded" (Maybe.fromMaybe False lastPrimarySucceeded)
                  ]
                Ext.Log.log Ext.Log.Live
                  ( concat
                      [ "[trace " ++ traceId ++ "] skipping compile"
                      , " root=" ++ projectRoot
                      , " files=" ++ Trace.formatPaths projectFiles
                      , " fsVersion=" ++ show (Versions.fsVersion versions)
                      , " compileVersion=" ++ show (Versions.compileVersion versions)
                      ]
                  )
                pure (didCompileAny, lastPrimarySucceeded)
      versionsBeforeWait <- Versions.readVersions projectRoot
      lockResult <- withProjectCompileLockIfAvailable projectRoot (compileUntilClean projectFiles False Nothing 0)
      currentResult <- STM.readTVarIO mCompileResult
      pure $ case lockResult of
        Just (didCompile, lastPrimarySucceeded) ->
          if didCompile
            then (True, lastPrimarySucceeded, projCache)
            else if Versions.compileVersion versionsBeforeWait < Versions.fsVersion versionsBeforeWait
              then
                (True, Just (Client.compilationResultSucceeded currentResult), projCache)
              else (False, lastPrimarySucceeded, projCache)

        Nothing ->
          (False, Nothing, projCache)

    runTestsForCompileResults :: [(Bool, Maybe Bool, Client.ProjectCache)] -> IO ()
    runTestsForCompileResults results = do
      counter <- STM.newTVarIO (List.length results)
      let runOne (didCompile, lastPrimarySucceeded, projCache@(Client.ProjectCache proj _ _ _ _)) = do
            case (didCompile, lastPrimarySucceeded) of
              (True, Just True) ->
                compileTestsWithTrace state traceId projCache

              (True, Just False) ->
                traceSkippedTests state traceId proj "primary_compile_failed"

              _ ->
                pure ()
            STM.atomically $ do
              n <- STM.readTVar counter
              STM.writeTVar counter (n - 1)
      mapM_ (\result -> Ext.Common.trackedForkIO (runOne result)) results
      STM.atomically $ do
        n <- STM.readTVar counter
        STM.check (n == 0)

scheduleDebouncedCompileRelevantProjects :: Client.State -> String -> Int -> [FilePath] -> IO ()
scheduleDebouncedCompileRelevantProjects state traceId delayMicros elmFiles =
  scheduleDebouncedCompileRelevantProjectsWithCallback state traceId delayMicros elmFiles (\_ -> pure ())

scheduleDebouncedCompileRelevantProjectsWithCallback :: Client.State -> String -> Int -> [FilePath] -> (Bool -> IO ()) -> IO ()
scheduleDebouncedCompileRelevantProjectsWithCallback state@(Client.State _ mProjects _ _ _ _ _ _ _) traceId delayMicros elmFiles afterCompile = do
  scheduleDebouncedCompileRelevantProjectsWithCallbacks state traceId traceId delayMicros elmFiles (\_ -> pure ()) afterCompile

scheduleDebouncedCompileRelevantProjectsWithCallbacks :: Client.State -> String -> String -> Int -> [FilePath] -> (Bool -> IO ()) -> (Bool -> IO ()) -> IO ()
scheduleDebouncedCompileRelevantProjectsWithCallbacks state traceId subscriberId delayMicros elmFiles afterPrimaryCompile afterCompile = do
  if elmFiles == []
    then pure ()
    else do
      Metrics.add "debounce.files_received" (length elmFiles)
      projects <- STM.readTVarIO (Client.projects state)
      let groupedPaths = Map.fromListWith Set.union
            [ (batchKeyForPath projects path, Set.singleton path)
            | path <- elmFiles
            ]
      Ext.Log.log Ext.Log.Live
        ( concat
            [ "[trace " ++ traceId ++ "] scheduleDebouncedCompileRelevantProjects"
            , " changed=" ++ Trace.formatPaths elmFiles
            , " delayMicros=" ++ show delayMicros
            ]
        )
      mapM_ (\(batchKey, paths) -> scheduleBatch batchKey paths) (Map.toList groupedPaths)
  where
    scheduleBatch batchKey paths =
      MVar.modifyMVar_ projectCompileDebounces $ \scheduled -> do
        let existing = Map.lookup batchKey scheduled
            mergedPaths = Set.union paths (maybe Set.empty pendingPaths existing)
            mergedSubscribers = Map.insert subscriberId (afterPrimaryCompile, afterCompile) (maybe Map.empty pendingSubscribers existing)
        Monad.mapM_ (Concurrent.killThread . pendingWorker) existing
        threadId <- Concurrent.forkIO $ do
          Concurrent.threadDelay delayMicros
          pending <- MVar.modifyMVar projectCompileDebounces $ \current ->
            pure (Map.delete batchKey current, Map.lookup batchKey current)
          case pending of
            Nothing -> pure ()
            Just batch -> do
              let paths = Set.toList (pendingPaths batch)
                  subscribers = Map.elems (pendingSubscribers batch)
                  notifyPrimary didCompile = mapM_ (\(callback, _) -> callback didCompile) subscribers
              Metrics.increment "debounce.batches"
              Metrics.add "debounce.batch_files" (length paths)
              didCompile <- compileRelevantProjectsWithPrimaryCallback state traceId paths notifyPrimary
              mapM_ (\(_, callback) -> callback didCompile) subscribers
        pure (Map.insert batchKey (PendingCompile mergedPaths mergedSubscribers threadId) scheduled)

    batchKeyForPath projects path =
      let affected =
            [ project
            | Client.ProjectCache project _ _ _ _ <- projects
            , Ext.Dev.Project.affectsCompilation path project
            ]
          owners = List.sortBy
            (\one two -> compare (length (Ext.Dev.Project.getRoot two)) (length (Ext.Dev.Project.getRoot one)))
            (filter (Ext.Dev.Project.contains path) affected)
      in case owners ++ affected of
           project : _ -> Ext.Dev.Project.getRoot project
           [] -> FP.takeDirectory path

-- | Compile tests for a project if test files have been discovered.
compileTests :: Client.State -> Client.ProjectCache -> IO ()
compileTests state projCache =
  compileTestsWithTrace state "compile.tests" projCache

traceSkippedTests :: Client.State -> String -> Ext.Dev.Project.Project -> String -> IO ()
traceSkippedTests state traceId proj outcome =
  PerfTrace.span
    (Client.trace state)
    "compile.tests"
    [ PerfTrace.text "trace_id" traceId
    , PerfTrace.text "project_root" (Ext.Dev.Project.getRoot proj)
    , PerfTrace.int "project_short_id" (Ext.Dev.Project._shortId proj)
    , PerfTrace.bool "skipped" True
    , PerfTrace.text "outcome" outcome
    ]
    (pure ())

compileTestsWithTrace :: Client.State -> String -> Client.ProjectCache -> IO ()
compileTestsWithTrace state@(Client.State _ _ _ _ _ _ _ mWorkspaceDiagsRequested _) traceId (Client.ProjectCache proj _ _ _ mTestVar) = do
  let root = Ext.Dev.Project.getRoot proj
  withProjectTestCompileLock root $ do
    versions <- Versions.readVersions root
    currentTest <- STM.readTVarIO mTestVar
    let alreadyCompiled =
          Versions.testCompileVersion versions == Just (Versions.compileVersion versions)
            && case currentTest of
                 Just info -> Maybe.isJust (Client.testCompilation info)
                 Nothing -> False
    if alreadyCompiled
      then do
        Metrics.increment "compile.test_cache_hits"
        traceSkippedTests state traceId proj "current_version"
      else compileCurrentTests versions currentTest
  where
    compileCurrentTests versions currentTest =
      case currentTest of
        Nothing ->
          PerfTrace.span
            (Client.trace state)
            "compile.tests"
            [ PerfTrace.text "trace_id" traceId
            , PerfTrace.text "project_root" (Ext.Dev.Project.getRoot proj)
            , PerfTrace.int "project_short_id" (Ext.Dev.Project._shortId proj)
            , PerfTrace.bool "has_tests" False
            , PerfTrace.text "outcome" "not_discovered"
            ]
            (pure ())
        Just ti -> do
          let files = Client.testFiles ti
          case files of
            [] ->
              PerfTrace.span
                (Client.trace state)
                "compile.tests"
                [ PerfTrace.text "trace_id" traceId
                , PerfTrace.text "project_root" (Ext.Dev.Project.getRoot proj)
                , PerfTrace.int "project_short_id" (Ext.Dev.Project._shortId proj)
                , PerfTrace.bool "has_tests" True
                , PerfTrace.int "test_file_count" 0
                , PerfTrace.text "outcome" "no_test_files"
                ]
                (pure ())
            (x:xs) -> do
              let root = Ext.Dev.Project.getRoot proj
                  targetVersion = Versions.compileVersion versions
              PerfTrace.span
                (Client.trace state)
                "compile.tests"
                [ PerfTrace.text "trace_id" traceId
                , PerfTrace.text "project_root" root
                , PerfTrace.int "project_short_id" (Ext.Dev.Project._shortId proj)
                , PerfTrace.bool "has_tests" True
                , PerfTrace.int "test_file_count" (length files)
                ]
                $ do
                  compiledR <- TestCompile.compile root (NE.List x xs)
                  Metrics.increment "compile.test_projects"
                  PerfTrace.event
                    (Client.trace state)
                    "compile.tests.result"
                    [ PerfTrace.text "trace_id" traceId
                    , PerfTrace.text "project_root" root
                    , PerfTrace.int "project_short_id" (Ext.Dev.Project._shortId proj)
                    , PerfTrace.text "outcome" (case compiledR of Left _ -> "error"; Right () -> "success")
                    ]
                  versionsVar <- Versions.getOrInit root
                  stored <- STM.atomically $ do
                    latestVersions <- STM.readTVar versionsVar
                    if Versions.fsVersion latestVersions /= targetVersion || Versions.compileVersion latestVersions /= targetVersion
                      then pure False
                      else do
                        cur <- STM.readTVar mTestVar
                        case cur of
                          Nothing -> pure False
                          Just info -> do
                            let testCompilation =
                                  case compiledR of
                                    Left reactorErr -> Client.TestError reactorErr
                                    Right () -> Client.TestSuccess
                            STM.writeTVar mTestVar (Just info { Client.testResults = Nothing, Client.testCompilation = Just testCompilation })
                            STM.writeTVar versionsVar (latestVersions { Versions.testCompileVersion = Just targetVersion })
                            pure True
                  Monad.when stored $ STM.atomically $ do
                    cur <- STM.readTVar mWorkspaceDiagsRequested
                    let updated =
                          Map.map
                            (\s -> Client.LspSession
                              { Client.workspaceDiagnosticsSnapshotFiles = Client.workspaceDiagnosticsSnapshotFiles s
                              , Client.workspaceDiagnosticsSnapshotOutOfDate = True
                              , Client.publishedDiagnosticFiles = Client.publishedDiagnosticFiles s
                              , Client.lspRoot = Client.lspRoot s
                              }
                            )
                            cur
                    STM.writeTVar mWorkspaceDiagsRequested updated

updateProjectFileInfo :: Project.Project -> Bool -> Either a b -> Map.Map FilePath Client.FileInfo -> Map.Map FilePath Client.FileInfo -> Map.Map FilePath Client.FileInfo
updateProjectFileInfo proj replaceProject compileResult current fileInfoByPath =
  let withoutProject = Map.filterWithKey (\path _ -> not (Ext.Dev.Project.contains path proj)) current
      merged = Map.foldlWithKey' (\acc path info -> Map.insert path info acc) current fileInfoByPath
      replaced = Map.union fileInfoByPath withoutProject
   in case compileResult of
        Right _ ->
          if replaceProject then replaced else merged
        Left _ -> merged

shouldReplaceProjectFileInfo :: [FilePath] -> Bool
shouldReplaceProjectFileInfo files =
  null files || any isProjectConfig files
  where
    isProjectConfig path =
      let fileName = FP.takeFileName (FP.normalise path)
       in fileName == "elm.json" || fileName == "elm.dev.json"


markDiagnosticsOutOfDate :: Client.LspSession -> Client.LspSession
markDiagnosticsOutOfDate session =
  session { Client.workspaceDiagnosticsSnapshotOutOfDate = True }
