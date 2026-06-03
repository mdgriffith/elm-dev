{-# LANGUAGE OverloadedStrings #-}

module Watchtower.State.Compile (compile, compileRelevantProjects, scheduleDebouncedCompileRelevantProjects, scheduleDebouncedCompileRelevantProjectsWithCallback, updateVfsFromFs, compileTests, markFilesystemChanged, updateProjectFileInfo) where

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
import qualified Data.List as List
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
import qualified Watchtower.Trace as Trace
import qualified Control.Monad as Monad
import qualified Ext.Test.Discover
import qualified Data.Set as Set
import qualified Control.Concurrent.MVar as MVar
import qualified System.IO.Unsafe as Unsafe
import qualified Control.Exception as Exception
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
        entrypointGroups <- Ext.Dev.Project.entrypointGroupsForChangedFiles files proj
        if null entrypointGroups
          then do
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
              STM.writeTVar mFileInfo (updateProjectFileInfo proj eitherCompiled current fileInfoByPath)

            -- Mark workspace diagnostics snapshots as out-of-date after compile result and file info updates
            STM.atomically $ do
              cur <- STM.readTVar mWorkspaceDiagsRequested
              let updated =
                    Map.map
                      (\s -> Client.LspSession
                        { Client.workspaceDiagnosticsSnapshotFiles = Client.workspaceDiagnosticsSnapshotFiles s
                        , Client.workspaceDiagnosticsSnapshotOutOfDate = True
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

{-# NOINLINE projectCompileDebounces #-}
projectCompileDebounces :: MVar.MVar (Map.Map FilePath Concurrent.ThreadId)
projectCompileDebounces = Unsafe.unsafePerformIO (MVar.newMVar Map.empty)

withProjectCompileLockIfAvailable :: FilePath -> IO Bool -> IO Bool
withProjectCompileLockIfAvailable projectRoot action = do
  lock <- MVar.modifyMVar projectCompileLocks $ \locks ->
    case Map.lookup projectRoot locks of
      Just existing -> pure (locks, existing)
      Nothing -> do
        newLock <- MVar.newMVar ()
        pure (Map.insert projectRoot newLock locks, newLock)
  acquired <- MVar.tryTakeMVar lock
  case acquired of
    Nothing -> pure False
    Just () -> action `Exception.finally` MVar.putMVar lock ()

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


-- | Compile any projects that are relevant to the given file paths.
--   A project is considered relevant if it contains at least one of the provided files.
--   Compilation is performed synchronously here so the caller can rely on
--   fresh results when this function returns.
compileRelevantProjects :: Client.State -> String -> [FilePath] -> IO Bool
compileRelevantProjects state traceId elmFiles =
  PerfTrace.span
    (Client.trace state)
    "compile.relevant_projects"
    [ PerfTrace.text "trace_id" traceId
    , PerfTrace.int "changed_file_count" (length elmFiles)
    ]
    (compileRelevantProjectsUntraced state traceId elmFiles)

compileRelevantProjectsUntraced :: Client.State -> String -> [FilePath] -> IO Bool
compileRelevantProjectsUntraced state@(Client.State _ mProjects _ _ _ _ _ _ _) traceId elmFiles = do
  if elmFiles == []
    then pure False
    else do
      projects <- STM.readTVarIO mProjects
      let relevant = List.filter (projectTouchesAny elmFiles) projects
      Ext.Log.log Ext.Log.Live
        ( concat
            [ "[trace " ++ traceId ++ "] compileRelevantProjects"
            , " changed=" ++ Trace.formatPaths elmFiles
            , " relevantProjects=" ++ show (length relevant)
            ]
        )
      case relevant of
        [] -> do 
          Ext.Log.log Ext.Log.Live ("[trace " ++ traceId ++ "] No relevant projects to compile")
          pure False
        _ ->
          Ext.Common.track "compile relevant projects" $ do
            counter <- STM.newTVarIO (List.length relevant)
            anyCompiled <- STM.newTVarIO False
            let runOne projCache = do
                  didCompile <- compileProjectFiles elmFiles projCache
                  Monad.when didCompile (STM.atomically (STM.writeTVar anyCompiled True))
                  STM.atomically $ do
                    n <- STM.readTVar counter
                    STM.writeTVar counter (n - 1)
            mapM_ (\proj -> Ext.Common.trackedForkIO (runOne proj)) relevant
            STM.atomically $ do
              n <- STM.readTVar counter
              STM.check (n == 0)
            STM.readTVarIO anyCompiled
  where
    projectTouchesAny :: [FilePath] -> Client.ProjectCache -> Bool
    projectTouchesAny paths (Client.ProjectCache proj _ _ _ _) =
      any (\p -> Ext.Dev.Project.affectsCompilation p proj) paths

    compileProjectFiles :: [FilePath] -> Client.ProjectCache -> IO Bool
    compileProjectFiles paths projCache@(Client.ProjectCache proj _ _ _ _) = do
      let projectRoot = Ext.Dev.Project.getRoot proj
          projectFiles = List.filter (\p -> Ext.Dev.Project.affectsCompilation p proj) paths
          compileUntilClean didCompileAny = do
            versions <- Versions.readVersions projectRoot
            if Versions.compileVersion versions < Versions.fsVersion versions
              then do
                Ext.Log.log Ext.Log.Live
                  ( concat
                      [ "[trace " ++ traceId ++ "] compiling project"
                      , " root=" ++ projectRoot
                      , " files=" ++ Trace.formatPaths projectFiles
                      , " fsVersion=" ++ show (Versions.fsVersion versions)
                      , " compileVersion=" ++ show (Versions.compileVersion versions)
                      ]
                  )
                _ <- compile state traceId projCache projectFiles
                -- If this project has tests, compile them using previously discovered test files
                compileTestsWithTrace state traceId projCache
                compileUntilClean True
              else do
                Ext.Log.log Ext.Log.Live
                  ( concat
                      [ "[trace " ++ traceId ++ "] skipping compile"
                      , " root=" ++ projectRoot
                      , " files=" ++ Trace.formatPaths projectFiles
                      , " fsVersion=" ++ show (Versions.fsVersion versions)
                      , " compileVersion=" ++ show (Versions.compileVersion versions)
                      ]
                  )
                pure didCompileAny
      withProjectCompileLockIfAvailable projectRoot (compileUntilClean False)

scheduleDebouncedCompileRelevantProjects :: Client.State -> String -> Int -> [FilePath] -> IO ()
scheduleDebouncedCompileRelevantProjects state traceId delayMicros elmFiles =
  scheduleDebouncedCompileRelevantProjectsWithCallback state traceId delayMicros elmFiles (\_ -> pure ())

scheduleDebouncedCompileRelevantProjectsWithCallback :: Client.State -> String -> Int -> [FilePath] -> (Bool -> IO ()) -> IO ()
scheduleDebouncedCompileRelevantProjectsWithCallback state@(Client.State _ mProjects _ _ _ _ _ _ _) traceId delayMicros elmFiles afterCompile = do
  if elmFiles == []
    then pure ()
    else do
      projects <- STM.readTVarIO mProjects
      let relevant = List.filter (projectTouchesAny elmFiles) projects
      Ext.Log.log Ext.Log.Live
        ( concat
            [ "[trace " ++ traceId ++ "] scheduleDebouncedCompileRelevantProjects"
            , " changed=" ++ Trace.formatPaths elmFiles
            , " relevantProjects=" ++ show (length relevant)
            , " delayMicros=" ++ show delayMicros
            ]
        )
      Monad.mapM_ scheduleOne relevant
  where
    projectTouchesAny :: [FilePath] -> Client.ProjectCache -> Bool
    projectTouchesAny paths (Client.ProjectCache proj _ _ _ _) =
      any (\p -> Ext.Dev.Project.affectsCompilation p proj) paths

    scheduleOne :: Client.ProjectCache -> IO ()
    scheduleOne (Client.ProjectCache proj _ _ _ _) = do
      let projectRoot = Ext.Dev.Project.getRoot proj
      oldThread <- MVar.modifyMVar projectCompileDebounces $ \scheduled ->
        pure (Map.delete projectRoot scheduled, Map.lookup projectRoot scheduled)
      Monad.mapM_ Concurrent.killThread oldThread
      threadId <- Concurrent.forkIO $ do
        Concurrent.threadDelay delayMicros
        MVar.modifyMVar_ projectCompileDebounces (pure . Map.delete projectRoot)
        didCompile <- compileRelevantProjects state traceId elmFiles
        afterCompile didCompile
        pure ()
      MVar.modifyMVar_ projectCompileDebounces $ \scheduled ->
        pure (Map.insert projectRoot threadId scheduled)

-- | Compile tests for a project if test files have been discovered.
compileTests :: Client.State -> Client.ProjectCache -> IO ()
compileTests state projCache =
  compileTestsWithTrace state "compile.tests" projCache

compileTestsWithTrace :: Client.State -> String -> Client.ProjectCache -> IO ()
compileTestsWithTrace state@(Client.State _ _ _ _ _ _ _ mWorkspaceDiagsRequested _) traceId (Client.ProjectCache proj _ _ _ mTestVar) = do
  currentTest <- STM.readTVarIO mTestVar
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
              PerfTrace.event
                (Client.trace state)
                "compile.tests.result"
                [ PerfTrace.text "trace_id" traceId
                , PerfTrace.text "project_root" root
                , PerfTrace.int "project_short_id" (Ext.Dev.Project._shortId proj)
                , PerfTrace.text "outcome" (case compiledR of Left _ -> "error"; Right () -> "success")
                ]
              STM.atomically $ do
                cur <- STM.readTVar mTestVar
                case cur of
                  Nothing -> pure ()
                  Just info ->
                    case compiledR of
                      Left reactorErr ->
                        STM.writeTVar mTestVar (Just info { Client.testCompilation = Just (Client.TestError reactorErr) })
                      Right () ->
                        STM.writeTVar mTestVar (Just info { Client.testCompilation = Just Client.TestSuccess })
              -- Mark workspace diagnostics snapshots as out-of-date when test compilation changes
              STM.atomically $ do
                cur <- STM.readTVar mWorkspaceDiagsRequested
                let updated =
                      Map.map
                        (\s -> Client.LspSession
                          { Client.workspaceDiagnosticsSnapshotFiles = Client.workspaceDiagnosticsSnapshotFiles s
                          , Client.workspaceDiagnosticsSnapshotOutOfDate = True
                          , Client.lspRoot = Client.lspRoot s
                          }
                        )
                        cur
                STM.writeTVar mWorkspaceDiagsRequested updated

updateProjectFileInfo :: Project.Project -> Either a b -> Map.Map FilePath Client.FileInfo -> Map.Map FilePath Client.FileInfo -> Map.Map FilePath Client.FileInfo
updateProjectFileInfo proj compileResult current fileInfoByPath =
  let withoutProject = Map.filterWithKey (\path _ -> not (Ext.Dev.Project.contains path proj)) current
      merged = Map.foldlWithKey' (\acc path info -> Map.insert path info acc) current fileInfoByPath
      replaced = Map.union fileInfoByPath withoutProject
   in case compileResult of
        Right _ -> replaced
        Left _ -> merged
