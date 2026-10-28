{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (when, unless)
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.STM as STM
import qualified Control.Exception as Exception
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as JSONTypes
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSChar8
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy.Char8 as LazyChar8
import qualified Data.Foldable as Foldable
import qualified Ext.FileCache as FileCache
import qualified Ext.Filewatch as Filewatch
import qualified Ext.Test.Result as TestResult
import qualified Ext.Test.Result.Report as TestReport
import qualified Ext.Test.Compile as TestCompile
import qualified Ext.Test.Spec as SpecSupport
import qualified Ext.Test.Generate as TestGenerate
import qualified Ext.Test.Parse as TestParse
import qualified Ext.Test.Runner as TestRunner
import qualified Data.Map.Strict as Map
import qualified Data.Map.Lazy as LazyMap
import qualified Data.Maybe as Maybe
import qualified Data.Name as Name
import qualified Data.NonEmptyList as NE
import qualified Data.Set as Set
import qualified Ext.CompileHelpers.Disk as DiskCompile
import qualified Ext.CompileHelpers.Memory as MemoryCompile
import qualified Ext.Optimization.Level as Optimization
import qualified Gen.Generate as Generate
import qualified Gen.Javascript as Javascript
import qualified Gen.Config as GenConfig
import qualified Ext.Dev.Project as Project
import qualified Ext.CompileHelpers.Generic as CompileHelpers
import qualified Generate.JavaScript.Functions as JsFunctions
import qualified Generate.Mode as Mode
import qualified Make
import qualified Reporting.Exit as ReportingExit
import qualified Reporting.Task as Task
import qualified Terminal
import qualified Terminal.Chomp as Chomp
import qualified Watchtower.Live.Client as Client
import qualified Watchtower.Server.LSP.Protocol as Protocol
import qualified Watchtower.State.Compile as CompileState
import qualified Watchtower.State.Versions as Versions
import qualified Watchtower.State.DocsSnapshot as DocsSnapshot
import qualified Watchtower.State.Project as ProjectState
import qualified Watchtower.State.Discover as DiscoverState
import qualified Watchtower.State.TestJobs as TestJobs
import qualified Watchtower.Server.LSP.EditorsOpen as EditorsOpen
import qualified Watchtower.Server.MCP as MCP
import qualified Watchtower.Server.Daemon.State as DaemonState
import qualified Watchtower.Server.Daemon as Daemon
import qualified Watchtower.Server.Run as ServerRun
import qualified Network.Socket as Socket
import qualified System.IO as IO
import qualified Ext.Log
import qualified AST.Canonical as Canonical
import qualified Reporting.Annotation as Annotation
import qualified System.Exit as Exit
import qualified System.Directory as Dir
import qualified System.FilePath as FilePath
import qualified System.Process as Process
import qualified Data.List as List
import qualified Data.IORef as IORef
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Time.Clock.POSIX as POSIX
import qualified Data.Utf8 as Utf8
import qualified Deps.Solver as Solver
import qualified Deps.Registry as Registry
import qualified Elm.Constraint as Constraint
import qualified Elm.Docs as Docs
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Outline as Outline
import qualified Elm.Package as Package
import qualified Elm.Version as ElmVersion
import qualified Ext.DependencyManager as DependencyManager
import qualified Ext.DependencySize as DependencySize
import qualified Ext.DependencyManager.Cli as DependencyCli
import qualified CommandParser
import qualified Json.Encode as Encode
import qualified Json.String as JsonString
import qualified Watchtower.Server.MCP.Protocol as MCPProtocol
import qualified Gen.Commands.Init as InitCommand
import qualified System.Timeout as Timeout
import qualified System.Environment as Environment
import qualified System.IO.Temp as Temp
import qualified Http
import qualified Stuff
import qualified Text.Read as Read

main :: IO ()
main = do
  args <- Environment.getArgs
  case args of
    ["dev", "serve", "--silent"] -> do
      -- Other regression fixtures may have changed the test parent's cwd to a
      -- scratch directory. Give this disposable daemon a stable private cwd.
      launchRoot <- Environment.getEnv "ELM_DEV_TEST_DAEMON_CWD"
      Dir.setCurrentDirectory launchRoot
      -- Assert that the background launcher provided real OS streams, not just
      -- logical Haskell stdin/stdout Handles pointing at closed descriptors.
      input <- BS.hGet IO.stdin 1
      unless (BS.null input) (fail "Expected null-device stdin")
      IO.hPutStrLn IO.stdout "daemon stdio probe"
      IO.hFlush IO.stdout
      Ext.Log.setMode Ext.Log.Silent
      Daemon.allocateServeParams >>= Daemon.serve
    ["__elm-dev-test-worker", _, responsePath] ->
      BS.writeFile responsePath successWorkerResponse
    _ -> runTests

runTests :: IO ()
runTests = do
  Environment.setEnv "MODE" "memory"
  Environment.unsetEnv "ELM_DEV_TRACE_DB"
  fileCacheResults <- sequence fileCacheTests
  docsResults <- sequence docsTests
  versionResults <- sequence versionTests
  generationResults <- sequence generationTests
  optimizationResults <- sequence optimizationTests
  mcpResults <- sequence mcpTests
  daemonResults <- sequence daemonTests
  dependencyResults <- sequence dependencyTests
  let results = fileCacheResults ++ docsResults ++ versionResults ++ generationResults ++ optimizationResults ++ mcpResults ++ daemonResults ++ dependencyResults
      failures = [name | (name, False) <- results]
  if null failures
    then do
      putStrLn ("All tests passed (" ++ show (length results) ++ ")")
    else do
      putStrLn "Failed tests:"
      mapM_ (putStrLn . ("- " ++)) failures
      Exit.exitFailure

type NamedTest = IO (String, Bool)

runTest :: String -> IO Bool -> NamedTest
runTest name action = do
  passed <- action
  putStrLn ((if passed then "PASS" else "FAIL") ++ "  " ++ name)
  pure (name, passed)

fileCacheTests :: [NamedTest]
fileCacheTests =
  [ runTest "single-line range edit preserves newlines" testSingleLineEdit
  , runTest "multi-line range edit splices correctly" testMultiLineEdit
  , runTest "EOF insertion with line==lineCount works" testEofInsertion
  , runTest "EOF insertion with non-zero char is rejected" testInvalidEofInsertion
  , runTest "CRLF-like content edit remains stable" testCrLfStability
  , runTest "removeDir clears cached descendants recursively" testRemoveDirClearsDescendants
  ]

docsTests :: [NamedTest]
docsTests =
  [ runTest "normal memory compilation returns module docs" testNormalCompilationReturnsDocs
  , runTest "project docs snapshot respects version and flags" testDocsSnapshotFreshness
  , runTest "project docs snapshot supports direct name and path lookup" testDocsSnapshotDirectLookup
  , runTest "direct docs lookup does not force unrelated modules" testDocsSnapshotLookupIsSelective
  , runTest "project docs snapshots remain isolated" testDocsSnapshotProjectIsolation
  , runTest "docs snapshot republish updates both indexes atomically" testDocsSnapshotRepublish
  , runTest "project docs module listing is sorted by module name" testDocsSnapshotListingOrder
  , runTest "normal compile publishes fresh snapshot and file docs" testCompilePublishesDocs
  , runTest "ensureProjectDocs reconstructs a removed snapshot from file info" testEnsureProjectDocsReconstructsSnapshot
  , runTest "restart rebuilds docs from normal analysis without sidecars" testRestartRebuildsDocsInMemory
  , runTest "incremental compile replaces changed docs and retains unchanged docs" testIncrementalCompileUpdatesDocs
  , runTest "deleting a module removes snapshot and file docs" testDeletedModuleRemovesDocs
  ]

testDocsSnapshotFreshness :: IO Bool
testDocsSnapshotFreshness = do
  root <- uniqueRoot
  let flags = CompileHelpers.Flags CompileHelpers.Dev CompileHelpers.NoOutput CompileHelpers.DebuggerNone
      changedFlags = CompileHelpers.Flags CompileHelpers.Debug CompileHelpers.NoOutput CompileHelpers.DebuggerNone
      path = root FilePath.</> "src" FilePath.</> "Main.elm"
      docsModule = emptyDocsModule (Name.fromChars "Main") "Main docs"
  DocsSnapshot.publish root 3 flags (Map.singleton path (Name.fromChars "Main", docsModule))
  current <- DocsSnapshot.lookupFresh root 3 flags
  staleVersion <- DocsSnapshot.lookupFresh root 4 flags
  staleFlags <- DocsSnapshot.lookupFresh root 3 changedFlags
  pure (fmap Map.keys current == Just [path] && Maybe.isNothing staleVersion && Maybe.isNothing staleFlags)

testDocsSnapshotProjectIsolation :: IO Bool
testDocsSnapshotProjectIsolation = do
  rootA <- uniqueRoot
  let rootB = rootA ++ "-other"
      flags = CompileHelpers.Flags CompileHelpers.Dev CompileHelpers.NoOutput CompileHelpers.DebuggerNone
      pathA = rootA FilePath.</> "src" FilePath.</> "Shared.elm"
      pathB = rootB FilePath.</> "src" FilePath.</> "Shared.elm"
      docsA = emptyDocsModule (Name.fromChars "Shared") "Project A"
      docsB = emptyDocsModule (Name.fromChars "Shared") "Project B"
  DocsSnapshot.publish rootA 1 flags (Map.singleton pathA (Name.fromChars "Shared", docsA))
  DocsSnapshot.publish rootB 1 flags (Map.singleton pathB (Name.fromChars "Shared", docsB))
  modulesA <- DocsSnapshot.modulesForProject rootA
  modulesB <- DocsSnapshot.modulesForProject rootB
  foundA <- DocsSnapshot.lookupModuleForProject rootA (Name.fromChars "Shared")
  foundB <- DocsSnapshot.lookupModuleForProject rootB (Name.fromChars "Shared")
  pure
    ( Map.keys modulesA == [pathA]
        && Map.keys modulesB == [pathB]
        && fmap (docsComment . snd) foundA == Just "Project A"
        && fmap (docsComment . snd) foundB == Just "Project B"
    )

testDocsSnapshotDirectLookup :: IO Bool
testDocsSnapshotDirectLookup = do
  root <- uniqueRoot
  let flags = CompileHelpers.Flags CompileHelpers.Dev CompileHelpers.NoOutput CompileHelpers.DebuggerNone
      name = Name.fromChars ("Direct" ++ filter (`elem` ['0'..'9']) root)
      path = root FilePath.</> "src" FilePath.</> "Direct.elm"
      docsModule = emptyDocsModule name "Direct docs"
  DocsSnapshot.publish root 1 flags (Map.singleton path (name, docsModule))
  byName <- DocsSnapshot.lookupModuleForProject root name
  byPath <- Map.lookup path <$> DocsSnapshot.modulesForProject root
  unscoped <- DocsSnapshot.lookupModule name
  pure
    ( fmap fst byName == Just path
        && fmap docsComment byPath == Just "Direct docs"
        && fmap fst unscoped == Just path
    )

testDocsSnapshotLookupIsSelective :: IO Bool
testDocsSnapshotLookupIsSelective = do
  root <- uniqueRoot
  let flags = CompileHelpers.Flags CompileHelpers.Dev CompileHelpers.NoOutput CompileHelpers.DebuggerNone
      targetName = Name.fromChars "Target"
      otherName = Name.fromChars "Other"
      targetPath = root FilePath.</> "src" FilePath.</> "Target.elm"
      otherPath = root FilePath.</> "src" FilePath.</> "Other.elm"
      unevaluatedDocs label = error (label ++ " docs were forced") :: Docs.Module
      entries = LazyMap.fromList
        [ (otherPath, (otherName, unevaluatedDocs "unrelated"))
        , (targetPath, (targetName, unevaluatedDocs "target"))
        ]
  result <- (Exception.try (do
    DocsSnapshot.publish root 1 flags entries
    found <- DocsSnapshot.lookupModuleForProject root targetName
    Exception.evaluate (fmap fst found)) :: IO (Either Exception.SomeException (Maybe FilePath)))
  pure $ case result of
    Right (Just path) -> path == targetPath
    _ -> False

testDocsSnapshotRepublish :: IO Bool
testDocsSnapshotRepublish = do
  root <- uniqueRoot
  let flags = CompileHelpers.Flags CompileHelpers.Dev CompileHelpers.NoOutput CompileHelpers.DebuggerNone
      oldName = Name.fromChars "Old"
      removedName = Name.fromChars "Removed"
      renamedName = Name.fromChars "Renamed"
      renamedPath = root FilePath.</> "src" FilePath.</> "Renamed.elm"
      removedPath = root FilePath.</> "src" FilePath.</> "Removed.elm"
  DocsSnapshot.publish root 1 flags (Map.fromList
    [ (renamedPath, (oldName, emptyDocsModule oldName "old"))
    , (removedPath, (removedName, emptyDocsModule removedName "removed"))
    ])
  DocsSnapshot.publish root 2 flags (Map.singleton renamedPath (renamedName, emptyDocsModule renamedName "new"))
  old <- DocsSnapshot.lookupModuleForProject root oldName
  removed <- DocsSnapshot.lookupModuleForProject root removedName
  renamed <- DocsSnapshot.lookupModuleForProject root renamedName
  paths <- DocsSnapshot.modulesForProject root
  pure
    ( Maybe.isNothing old
        && Maybe.isNothing removed
        && fmap fst renamed == Just renamedPath
        && Map.keys paths == [renamedPath]
    )

testDocsSnapshotListingOrder :: IO Bool
testDocsSnapshotListingOrder = do
  root <- uniqueRoot
  let flags = CompileHelpers.Flags CompileHelpers.Dev CompileHelpers.NoOutput CompileHelpers.DebuggerNone
      aName = Name.fromChars "A"
      zName = Name.fromChars "Z"
      aPath = root FilePath.</> "src" FilePath.</> "Z-path.elm"
      zPath = root FilePath.</> "src" FilePath.</> "A-path.elm"
  DocsSnapshot.publish root 1 flags (Map.fromList
    [ (zPath, (zName, emptyDocsModule zName "z"))
    , (aPath, (aName, emptyDocsModule aName "a"))
    ])
  listed <- DocsSnapshot.listedModulesForProject root
  pure (map fst listed == [aPath, zPath])

emptyDocsModule :: Name.Name -> String -> Docs.Module
emptyDocsModule moduleName comment =
  Docs.Module moduleName (JsonString.fromChars comment) Map.empty Map.empty Map.empty Map.empty

docsComment :: Docs.Module -> String
docsComment (Docs.Module _ comment _ _ _ _) =
  Text.unpack (Text.strip (Text.pack (JsonString.toChars comment)))

testNormalCompilationReturnsDocs :: IO Bool
testNormalCompilationReturnsDocs = do
  root <- uniqueRoot
  let srcDir = root FilePath.</> "src"
      mainPath = srcDir FilePath.</> "Main.elm"
      flags = CompileHelpers.Flags CompileHelpers.Dev CompileHelpers.NoOutput CompileHelpers.DebuggerNone
  writeElmApp root
  Dir.createDirectoryIfMissing True srcDir
  writeFile mainPath optimizationRuntimeElm
  (normalResult, normalFileInfo) <- MemoryCompile.compile root (NE.List ("src" FilePath.</> "Main.elm") []) flags Nothing
  pure $
    either (const False) (const True) normalResult
      && any hasModuleDocs (Map.elems normalFileInfo)
  where
    hasModuleDocs info =
      case Client.docs info of
        Just (Docs.Module name _ _ _ _ _) -> name == Name.fromChars "Main"
        Nothing -> False

testCompilePublishesDocs :: IO Bool
testCompilePublishesDocs = do
  (root, mainPath, otherPath, state, projectCache) <- docsLifecycleProject "Main initial" "Other initial"
  result <- CompileState.compile state "test.docs.publish" projectCache []
  snapshot <- freshDocsSnapshot root docsLifecycleFlags
  fileInfo <- Client.getAllFileInfos state
  sidecarExists <- Dir.doesDirectoryExist (root FilePath.</> "elm-stuff" FilePath.</> "0.19.1" FilePath.</> "elm-dev-docs")
  pure $
    either (const False) (const True) result
      && fmap docsComments snapshot == Just (Map.fromList [(mainPath, "Main initial"), (otherPath, "Other initial")])
      && docsComments (Map.mapMaybe Client.docs fileInfo) == Map.fromList [(mainPath, "Main initial"), (otherPath, "Other initial")]
      && not sidecarExists

testEnsureProjectDocsReconstructsSnapshot :: IO Bool
testEnsureProjectDocsReconstructsSnapshot = do
  (root, _, _, state, projectCache) <- docsLifecycleProject "Main current" "Other current"
  result <- CompileState.compile state "test.docs.reconstruct.initial" projectCache []
  fileInfo <- Client.getAllFileInfos state
  DocsSnapshot.remove root
  _ <- CompileState.ensureProjectDocs state "test.docs.reconstruct" projectCache
  snapshot <- freshDocsSnapshot root docsLifecycleFlags
  pure $
    either (const False) (const True) result
      && fmap docsComments snapshot == Just (docsComments (Map.mapMaybe Client.docs fileInfo))

testRestartRebuildsDocsInMemory :: IO Bool
testRestartRebuildsDocsInMemory = do
  (root, mainPath, otherPath, state, projectCache) <- docsLifecycleProject "Main restart" "Other restart"
  initial <- CompileState.compile state "test.docs.restart.initial" projectCache []
  DocsSnapshot.remove root
  (restartedState, restartedProjectCache, _) <- twoEntrypointProject root docsLifecycleFlags
  restarted <- CompileState.compile restartedState "test.docs.restart.rebuild" restartedProjectCache []
  snapshot <- freshDocsSnapshot root docsLifecycleFlags
  fileInfo <- Client.getAllFileInfos restartedState
  let expected = Map.fromList [(mainPath, "Main restart"), (otherPath, "Other restart")]
  pure $
    either (const False) (const True) initial
      && either (const False) (const True) restarted
      && fmap docsComments snapshot == Just expected
      && docsComments (Map.mapMaybe Client.docs fileInfo) == expected

testIncrementalCompileUpdatesDocs :: IO Bool
testIncrementalCompileUpdatesDocs = do
  (root, mainPath, otherPath, state, projectCache) <- docsLifecycleProject "Main before" "Other stable"
  initial <- CompileState.compile state "test.docs.incremental.initial" projectCache []
  writeFile mainPath (documentedWorkerElm "Main" "Main after" False)
  updated <- CompileState.compile state "test.docs.incremental.updated" projectCache [mainPath]
  snapshot <- freshDocsSnapshot root docsLifecycleFlags
  mainByName <- DocsSnapshot.lookupModuleForProject root (Name.fromChars "Main")
  otherByName <- DocsSnapshot.lookupModuleForProject root (Name.fromChars "Other")
  fileInfo <- Client.getAllFileInfos state
  let expected = Map.fromList [(mainPath, "Main after"), (otherPath, "Other stable")]
  pure $
    either (const False) (const True) initial
      && either (const False) (const True) updated
      && fmap docsComments snapshot == Just expected
      && fmap (docsComment . snd) mainByName == Just "Main after"
      && fmap (docsComment . snd) otherByName == Just "Other stable"
      && docsComments (Map.mapMaybe Client.docs fileInfo) == expected

testDeletedModuleRemovesDocs :: IO Bool
testDeletedModuleRemovesDocs = do
  (root, mainPath, otherPath, state, projectCache) <- docsLifecycleProject "Main" "Other"
  let sharedPath = root FilePath.</> "src" FilePath.</> "Shared.elm"
  writeFile mainPath (documentedWorkerElm "Main" "Main" True)
  writeFile otherPath (documentedWorkerElm "Other" "Other" True)
  writeFile sharedPath (documentedValueElm "Shared" "Shared docs")
  initial <- CompileState.compile state "test.docs.delete.initial" projectCache []
  initialSnapshot <- DocsSnapshot.modulesForProject root
  initialFileInfo <- Client.getAllFileInfos state
  writeFile mainPath (documentedWorkerElm "Main" "Main" False)
  writeFile otherPath (documentedWorkerElm "Other" "Other" False)
  Dir.removeFile sharedPath
  FileCache.delete sharedPath
  updated <- CompileState.compile state "test.docs.delete.updated" projectCache [mainPath, otherPath, sharedPath]
  snapshot <- DocsSnapshot.modulesForProject root
  deletedByName <- DocsSnapshot.lookupModuleForProject root (Name.fromChars "Shared")
  fileInfo <- Client.getAllFileInfos state
  pure $
    either (const False) (const True) initial
      && either (const False) (const True) updated
      && Map.member sharedPath initialSnapshot
      && Map.member sharedPath initialFileInfo
      && not (Map.member sharedPath snapshot)
      && Maybe.isNothing deletedByName
      && not (Map.member sharedPath fileInfo)

docsLifecycleFlags :: CompileHelpers.Flags
docsLifecycleFlags =
  CompileHelpers.Flags CompileHelpers.Dev CompileHelpers.NoOutput CompileHelpers.DebuggerNone

docsLifecycleProject :: String -> String -> IO (FilePath, FilePath, FilePath, Client.State, Client.ProjectCache)
docsLifecycleProject mainComment otherComment = do
  tempRoot <- uniqueRoot
  writeElmApp tempRoot
  root <- Dir.canonicalizePath tempRoot
  let srcDir = root FilePath.</> "src"
      mainPath = srcDir FilePath.</> "Main.elm"
      otherPath = srcDir FilePath.</> "Other.elm"
  Dir.createDirectoryIfMissing True srcDir
  writeFile mainPath (documentedWorkerElm "Main" mainComment False)
  writeFile otherPath (documentedWorkerElm "Other" otherComment False)
  (state, projectCache, _) <- twoEntrypointProject root docsLifecycleFlags
  pure (root, mainPath, otherPath, state, projectCache)

freshDocsSnapshot :: FilePath -> CompileHelpers.Flags -> IO (Maybe (Map.Map FilePath Docs.Module))
freshDocsSnapshot root flags = do
  versions <- Versions.readVersions root
  DocsSnapshot.lookupFresh root (Versions.fsVersion versions) flags

docsComments :: Map.Map FilePath Docs.Module -> Map.Map FilePath String
docsComments =
  Map.map (\(Docs.Module _ comment _ _ _ _) -> Text.unpack (Text.strip (Text.pack (JsonString.toChars comment))))

documentedWorkerElm :: String -> String -> Bool -> String
documentedWorkerElm moduleName comment importsShared =
  let source = workerElm moduleName importsShared
   in unlines (take 1 (lines source) ++ ["", "{-| " ++ comment ++ " -}"] ++ drop 1 (lines source))

documentedValueElm :: String -> String -> String
documentedValueElm moduleName comment =
  unlines
    [ "module " ++ moduleName ++ " exposing (value)"
    , ""
    , "{-| " ++ comment ++ " -}"
    , ""
    , "value : Int"
    , "value = 1"
    ]

versionTests :: [NamedTest]
versionTests =
  [ runTest "versions initialize to zero" testVersionsInit
  , runTest "versions bump and set compile" testVersionsBumpAndSet
  , runTest "watch path filter keeps elm sources/config" testWatchPathFilterRelevant
  , runTest "watch path filter excludes elm-stuff artifacts" testWatchPathFilterElmStuff
  , runTest "watch path filter excludes non-Elm build outputs" testWatchPathFilterBuildOutput
  , runTest "watch path filter treats explicit artifact roots as roots" testWatchPathFilterExplicitArtifactRoot
  , runTest "project discovery excludes artifacts and hidden directories" testProjectDiscoveryExclusions
  , runTest "project registration accepts sibling and symlink source directories" testSiblingSourceRegistration
  , runTest "failed discovery retries after sources are repaired" testDiscoveryRetry
  , runTest "discovery reports generation, outline, and entrypoint failures" testDiscoveryErrors
  , runTest "project contains is segment-aware" testProjectContainsSegmentAware
  , runTest "project affectsCompilation is source-aware" testProjectAffectsCompilation
  , runTest "application entrypoints compile as singleton groups" testApplicationEntrypointGroupsSplit
  , runTest "application entrypoints are targeted by imports" testApplicationEntrypointGroupsTargeted
  , runTest "application unused modules are detected" testApplicationUnusedModuleDetection
  , runTest "package changed source is targeted directly" testPackageChangedSourceIsTargeted
  , runTest "package unused modules are detected from exposed roots" testPackageUnusedModuleDetection
  , runTest "target results preserve per-entrypoint success" testTargetResultsSuccess
  , runTest "no-output entrypoints share one analysis build" testNoOutputEntrypointsBatch
  , runTest "no-output analysis retains errors from every entrypoint" testNoOutputEntrypointErrors
  , runTest "javascript entrypoints remain independent targets" testOutputEntrypointsRemainIndependent
  , runTest "skipped compile replaces stale project result" testSkippedCompileReplacesStaleResult
  , runTest "open file keeps in-memory precedence over fs watcher" testOpenFileSkipsFilesystemSync
  , runTest "compile refresh preserves open editor contents" testCompileRefreshPreservesOpenFile
  , runTest "disconnect cleanup removes editor and diagnostics state" testCleanupConnectionState
  , runTest "successful project fileInfo update replaces stale entries" testUpdateProjectFileInfo
  , runTest "duplicate didChange/save/fileChanged content only marks dirty once" testDuplicateChangeEventsMarkDirtyOnce
  ]

generationTests :: [NamedTest]
generationTests =
  [ runTest "code generation succeeds with basic config" testGenerateBasicConfig
  , runTest "code generation repairs missing outputs with unchanged inputs" testGenerateRepairsOutputs
  , runTest "code generation returns generator errors clearly" testGenerateThemeDecodeError
  , runTest "scaffold init stops before writing files when elm.json init fails" testScaffoldInitStopsOnElmJsonFailure
  , runTest "scaffold init surfaces generation failure" testScaffoldInitSurfacesGenerationFailure
  ]

optimizationTests :: [NamedTest]
optimizationTests =
  [ runTest "O2 function helpers use arity-specific raw function fields" testO2FunctionHelpers
  , runTest "O0 function helpers preserve default wrapper fields" testO0FunctionHelpers
  , runTest "O2 function helpers preserve runtime call behavior" testO2FunctionHelpersRuntime
  , runTest "make mode maps -O2 and -O3 to optimization levels" testMakeOptimizationModes
  , runTest "make flags parse -O2 shorthand" testMakeO2FlagParsing
  , runTest "compiled O0/O2/O3 JS preserves worker runtime output" testCompiledOptimizationRuntime
  , runTest "ported elm-optimize-level-2 replacement suite passes O0/O2/O3" testPortedReplacementSuite
  , runTest "O2/O3 optimization edge cases preserve runtime behavior" testOptimizationEdgeCases
  ]

mcpTests :: [NamedTest]
mcpTests =
  [ runTest "MCP exposes composite tools" testMcpCompositeToolsExposed
  , runTest "MCP serves behavioral spec documentation without a focused project" testMcpBehavioralSpecsGuide
  , runTest "MCP rejects relative dirs before tool execution" testMcpRelativeDirs
  , runTest "MCP check surfaces discovery errors" testMcpDiscoveryError
  , runTest "MCP hides retired single-purpose tools" testMcpRetiredToolsHidden
  , runTest "MCP tool surface is exact" testMcpToolSurfaceExact
  , runTest "MCP composite schemas advertise tagged unions" testMcpCompositeSchemas
  , runTest "MCP docs resolves local projects only when needed" testMcpDocsProjectResolution
  , runTest "MCP stale test summaries can be cleared" testMcpClearTestResults
  , runTest "MCP XML blocks preserve tag attributes and body" testMcpXmlBlockShape
  , runTest "MCP XML attributes are escaped" testMcpXmlAttributeEscaping
  , runTest "MCP test jobs complete and retain results" testMcpTestJobCompletion
  , runTest "MCP test jobs summarize retained failure details" testMcpTestJobFailureSummary
  , runTest "MCP test jobs cancel running actions" testMcpTestJobCancellation
  , runTest "MCP test jobs cancel while queued" testMcpQueuedTestJobCancellation
  , runTest "MCP test jobs limit active work per project" testMcpTestJobActiveLimit
  , runTest "MCP test jobs retain only recent terminal results" testMcpTestJobRetentionBound
  , runTest "MCP test jobs bound retained output" testMcpTestJobOutputBound
  , runTest "hover type index preserves region annotations" testHoverIndexPreservesAnnotations
  , runTest "test aggregator rewrites refresh cached source" testAggregatorRewriteRefreshesCache
  , runTest "package test manifest is persisted as an application" testPackageTestManifestIsPersistedAsApplication
  , runTest "spec sources and generated harness remain test-only" testSpecSourcesRemainTestOnly
  , runTest "test source directories deduplicate relative and symlink aliases" testCanonicalSpecSourceDirectories
  , runTest "spec readiness reports missing setup without changing the manifest" testSpecReadinessIsReadOnly
  , runTest "MCP local docs use the test context and preserve compilation errors" testMcpTestContextDocs
  , runTest "consumer spec MCP docs resolve with the configured app" testConsumerSpecDocs
  , runTest "consumer specs execute in the isolated MCP test worker" testConsumerSpecWorker
  , runTest "missing local spec sources report their path" testMissingSpecSourceReportsPath
  , runTest "test discovery preserves exposed declaration casing" testDiscoveryPreservesExposedDeclarationCasing
  , runTest "test reports decode and render exported-value durations" testReportDuration
  , runTest "JavaScript cancellation does not fail during stdin cleanup" testJavascriptCancellation
  , runTest "isolated test launcher exchanges bounded worker results" testIsolatedTestLauncherSuccess
  , runTest "isolated test launcher decodes worker timeouts" testIsolatedTestLauncherTimeout
  , runTest "isolated test launcher kills worker descendants" testIsolatedTestLauncherCancellation
  , runTest "isolated test launcher supports repeated runs" testIsolatedTestLauncherRepeatedRuns
  ]

daemonTests :: [NamedTest]
daemonTests =
  [ runTest "daemon state round-trips and ignores legacy fields" testDaemonStateRoundTrip
  , runTest "detached daemon can capture isolated worker output" testDetachedDaemonWorker
  ]

dependencyTests :: [NamedTest]
dependencyTests =
  [ runTest "package requirements parse latest, major, and exact versions" testPackageRequirementParsing
  , runTest "malformed package requirements are rejected" testPackageRequirementRejection
  , runTest "atLeast excludes dependency downgrades" testAtLeastConstraint
  , runTest "application solutions classify production and test closures" testDependencyClassification
  , runTest "production transitive packages are not duplicated as test roots" testSharedDependencyClassification
  , runTest "unreachable solver goals are excluded from application maps" testUnreachableDependencyClassification
  , runTest "cyclic dependency metadata terminates classification" testCyclicDependencyClassification
  , runTest "production install constraints promote test roots transactionally" testProductionInstallConstraints
  , runTest "test install constraints do not duplicate production packages" testTestInstallConstraints
  , runTest "test install fallback constraints keep production roots exact" testTestInstallFallbackConstraints
  , runTest "test requirements validate packages already available in production" testAvailableProductionRequirementValidation
  , runTest "package requirements retain major constraints and reject exact constraints" testPackageRequirementPersistence
  , runTest "constraint intersections reject excluded touching endpoints" testConstraintIntersectionEndpoints
  , runTest "duplicate install requirements are rejected" testDuplicateInstallRequirements
  , runTest "uninstall constraints affect only the requested scope" testUninstallConstraints
  , runTest "upgrade constraints preserve the non-target scope" testScopedUpgradeConstraints
  , runTest "all-scopes upgrades relax production and test roots" testAllScopesUpgradeConstraints
  , runTest "production dependency trees exclude test-only packages" testProductionDependencyTree
  , runTest "test dependency trees include and label test roots" testCombinedTestDependencyTree
  , runTest "dependency tree filtering keeps every path to a package" testDependencyTreeFiltering
  , runTest "unused dependency candidates retain used and protected packages" testUnusedDependencyCandidates
  , runTest "dependency changes are deterministic" testDependencyChangesDeterministic
  , runTest "dependency diff reports scope promotion" testDependencyScopePromotion
  , runTest "dependency diff reports direct to indirect moves" testDependencyReclassification
  , runTest "dependency transaction writes disk and cache on success" testDependencyTransactionSuccess
  , runTest "dependency transaction restores exact bytes after verification failure" testDependencyTransactionVerificationRollback
  , runTest "dependency transaction restores exact bytes after refresh failure" testDependencyTransactionRefreshRollback
  , runTest "dependency transaction rejects stale plans" testDependencyTransactionRejectsStalePlan
  , runTest "dependency transaction rejects unsaved cached manifests" testDependencyTransactionRejectsUnsavedCache
  , runTest "dependency transaction rolls back thrown exceptions" testDependencyTransactionExceptionRollback
  , runTest "outline-only normalization is treated as a changed plan" testDependencyPlanDetectsOutlineChange
  , runTest "MCP dependency arguments reject malformed optional fields" testMcpDependencyArgumentValidation
  , runTest "dependency size filters match module namespace segments" testDependencySizeNamespaceFilters
  , runTest "dependency size attribution reconciles with development bundle" testDependencySizeAttribution
  , runTest "MCP dependency JSON includes operation and scope" testMcpDependencyPlanJson
  , runTest "MCP dependency tree mirrors CLI dependency context" testMcpDependencyTreeJson
  , runTest "legacy MCP install responses retain docs links" testLegacyMcpInstallResponse
  , runTest "command parser detects JSON error mode" testCommandParserJsonMode
  , runTest "dependency CLI separates direct and indirect removals" testDependencyCliRemovalRendering
  , runTest "solver distinguishes warm offline cache behavior" testOfflineSolverCacheBehavior
  , runTest "offline application plans cover install uninstall and upgrades" testOfflineApplicationPlanMatrix
  , runTest "offline package plans persist solved major lower bounds" testOfflinePackagePlanMatrix
  ]

testPackageRequirementParsing :: IO Bool
testPackageRequirementParsing =
  pure $
    DependencyManager.parsePackageRequirement "elm/http"
      == Just (DependencyManager.PackageRequirement Package.http DependencyManager.Latest)
      && DependencyManager.parsePackageRequirement "elm/http@2"
      == Just (DependencyManager.PackageRequirement Package.http (DependencyManager.Major 2))
      && DependencyManager.parsePackageRequirement "elm/http@2.1.3"
      == Just (DependencyManager.PackageRequirement Package.http (DependencyManager.Exact (ElmVersion.Version 2 1 3)))

testPackageRequirementRejection :: IO Bool
testPackageRequirementRejection =
  pure $
    all ((== Nothing) . DependencyManager.parsePackageRequirement)
      [ "elm/http@"
      , "elm/http@2.0"
      , "elm/http@65536"
      , "Elm/http"
      , "elm//http"
      , "elm/http@latest"
      ]

testAtLeastConstraint :: IO Bool
testAtLeastConstraint =
  let current = ElmVersion.Version 2 1 0
  in pure $
    not (Constraint.satisfies (Constraint.atLeast current) (ElmVersion.Version 2 0 0))
      && Constraint.satisfies (Constraint.atLeast current) current
      && Constraint.satisfies (Constraint.atLeast current) (ElmVersion.Version 3 0 0)

testDependencyClassification :: IO Bool
testDependencyClassification =
  let
    production = testPackage "author/production"
    shared = testPackage "author/shared"
    testRoot = testPackage "author/test-root"
    testOnly = testPackage "author/test-only"
    version = ElmVersion.Version 1 0 0
    details dependencies = Solver.Details version (Map.fromList (map (\name -> (name, Constraint.anything)) dependencies))
    solution = Map.fromList
      [ (production, details [shared])
      , (shared, details [])
      , (testRoot, details [shared, testOnly])
      , (testOnly, details [])
      ]
    app = Solver.classifyApp emptyDependencyApp (Map.singleton production Constraint.anything) (Map.singleton testRoot Constraint.anything) solution
  in pure $
    Map.keys (Outline._app_deps_direct app) == [production]
      && Map.keys (Outline._app_deps_indirect app) == [shared]
      && Map.keys (Outline._app_test_direct app) == [testRoot]
      && Map.keys (Outline._app_test_indirect app) == [testOnly]

testSharedDependencyClassification :: IO Bool
testSharedDependencyClassification =
  let
    production = testPackage "author/production"
    shared = testPackage "author/shared"
    version = ElmVersion.Version 1 0 0
    solution = Map.fromList
      [ (production, Solver.Details version (Map.singleton shared Constraint.anything))
      , (shared, Solver.Details version Map.empty)
      ]
    app = Solver.classifyApp emptyDependencyApp (Map.singleton production Constraint.anything) (Map.singleton shared Constraint.anything) solution
  in pure $
    Map.member shared (Outline._app_deps_indirect app)
      && Map.notMember shared (Outline._app_test_direct app)
      && Map.notMember shared (Outline._app_test_indirect app)

testUnreachableDependencyClassification :: IO Bool
testUnreachableDependencyClassification =
  let
    production = testPackage "author/production"
    unreachable = testPackage "author/unreachable"
    version = ElmVersion.Version 1 0 0
    solution = Map.fromList
      [ (production, Solver.Details version Map.empty)
      , (unreachable, Solver.Details version Map.empty)
      ]
    app = Solver.classifyApp emptyDependencyApp (Map.singleton production Constraint.anything) Map.empty solution
  in pure $
    Map.member production (Outline._app_deps_direct app)
      && all (Map.notMember unreachable)
        [ Outline._app_deps_direct app
        , Outline._app_deps_indirect app
        , Outline._app_test_direct app
        , Outline._app_test_indirect app
        ]

testCyclicDependencyClassification :: IO Bool
testCyclicDependencyClassification =
  let
    production = testPackage "author/production"
    indirect = testPackage "author/indirect"
    version = ElmVersion.Version 1 0 0
    solution = Map.fromList
      [ (production, Solver.Details version (Map.singleton indirect Constraint.anything))
      , (indirect, Solver.Details version (Map.singleton production Constraint.anything))
      ]
    app = Solver.classifyApp emptyDependencyApp (Map.singleton production Constraint.anything) Map.empty solution
  in pure $
    Map.keys (Outline._app_deps_direct app) == [production]
      && Map.keys (Outline._app_deps_indirect app) == [indirect]

testProductionInstallConstraints :: IO Bool
testProductionInstallConstraints =
  let
    existing = testPackage "author/existing"
    promoted = testPackage "author/promoted"
    exactTarget = testPackage "author/exact-target"
    oldVersion = ElmVersion.Version 1 0 0
    exactVersion = ElmVersion.Version 3 2 1
    app = emptyDependencyApp
      { Outline._app_deps_direct = Map.singleton existing oldVersion
      , Outline._app_test_direct = Map.singleton promoted oldVersion
      }
    requirements =
      [ DependencyManager.PackageRequirement promoted DependencyManager.Latest
      , DependencyManager.PackageRequirement exactTarget (DependencyManager.Exact exactVersion)
      ]
    (production, test) = DependencyManager.installAppRoots Constraint.exactly DependencyManager.Production requirements app
  in pure $
    Map.lookup existing production == Just (Constraint.exactly oldVersion)
      && Map.lookup promoted production == Just (Constraint.exactly oldVersion)
      && Map.lookup exactTarget production == Just (Constraint.exactly exactVersion)
      && Map.notMember promoted test

testTestInstallConstraints :: IO Bool
testTestInstallConstraints =
  let
    production = testPackage "author/production"
    newTest = testPackage "author/new-test"
    version = ElmVersion.Version 1 0 0
    app = emptyDependencyApp { Outline._app_deps_direct = Map.singleton production version }
    requirements =
      [ DependencyManager.PackageRequirement production DependencyManager.Latest
      , DependencyManager.PackageRequirement newTest (DependencyManager.Major 2)
      ]
    (productionRoots, testRoots) = DependencyManager.installAppRoots Constraint.exactly DependencyManager.Test requirements app
  in pure $
    Map.member production productionRoots
      && Map.notMember production testRoots
      && maybe False (\constraint -> Constraint.satisfies constraint (ElmVersion.Version 2 9 0) && not (Constraint.satisfies constraint (ElmVersion.Version 3 0 0))) (Map.lookup newTest testRoots)

testTestInstallFallbackConstraints :: IO Bool
testTestInstallFallbackConstraints =
  let
    production = testPackage "author/production"
    existingTest = testPackage "author/existing-test"
    newTest = testPackage "author/new-test"
    productionVersion = ElmVersion.Version 1 2 3
    testVersion = ElmVersion.Version 2 3 4
    app = emptyDependencyApp
      { Outline._app_deps_direct = Map.singleton production productionVersion
      , Outline._app_test_direct = Map.singleton existingTest testVersion
      }
    requirements = [DependencyManager.PackageRequirement newTest DependencyManager.Latest]
    (productionRoots, testRoots) = DependencyManager.installAppRootsWith Constraint.exactly Constraint.untilNextMajor DependencyManager.Test requirements app
  in pure $
    Map.lookup production productionRoots == Just (Constraint.exactly productionVersion)
      && Map.lookup existingTest testRoots == Just (Constraint.untilNextMajor testVersion)

testAvailableProductionRequirementValidation :: IO Bool
testAvailableProductionRequirementValidation =
  let
    package = testPackage "author/production"
    version = ElmVersion.Version 2 1 0
    app = emptyDependencyApp { Outline._app_deps_direct = Map.singleton package version }
    outline = Outline.App app
    compatible = DependencyManager.PackageRequirement package (DependencyManager.Major 2)
    incompatible = DependencyManager.PackageRequirement package (DependencyManager.Exact (ElmVersion.Version 3 0 0))
    unusedEnv = error "compatible production requirements must not invoke the solver"
  in do
    compatibleResult <- DependencyManager.planInstallRequirementsWith unusedEnv DependencyManager.Test [compatible] outline
    incompatibleResult <- DependencyManager.planInstallRequirementsWith unusedEnv DependencyManager.Test [incompatible] outline
    pure (isUnchangedPlan compatibleResult && isNoSolutionPlan incompatibleResult)

isUnchangedPlan :: Either DependencyManager.Error DependencyManager.Plan -> Bool
isUnchangedPlan result =
  case result of
    Right plan -> not (DependencyManager.planChanged plan)
    Left _ -> False

isNoSolutionPlan :: Either DependencyManager.Error DependencyManager.Plan -> Bool
isNoSolutionPlan result =
  case result of
    Left DependencyManager.NoSolution -> True
    _ -> False

testPackageRequirementPersistence :: IO Bool
testPackageRequirementPersistence =
  let
    package = testPackage "author/package"
    solved = ElmVersion.Version 2 7 0
    exact = ElmVersion.Version 2 1 3
    exactRequirement = DependencyManager.PackageRequirement package (DependencyManager.Exact exact)
    majorConstraint = DependencyManager.packageConstraintForRequirement (DependencyManager.PackageRequirement package (DependencyManager.Major 2)) solved
  in pure $
    not (DependencyManager.packageRequirementSupported exactRequirement)
      && Constraint.satisfies majorConstraint solved
      && not (Constraint.satisfies majorConstraint (ElmVersion.Version 2 0 0))
      && not (Constraint.satisfies majorConstraint (ElmVersion.Version 3 0 0))

testConstraintIntersectionEndpoints :: IO Bool
testConstraintIntersectionEndpoints =
  let
    one = Constraint.untilNextMajor (ElmVersion.Version 1 0 0)
    two = Constraint.untilNextMajor (ElmVersion.Version 2 0 0)
  in pure (Constraint.intersect one two == Nothing)

testDuplicateInstallRequirements :: IO Bool
testDuplicateInstallRequirements =
  let
    package = testPackage "author/package"
    outline = Outline.App emptyDependencyApp
    requirements =
      [ DependencyManager.PackageRequirement package (DependencyManager.Major 1)
      , DependencyManager.PackageRequirement package (DependencyManager.Major 2)
      ]
    unusedEnv = error "duplicate requirements must not invoke the solver"
  in do
    result <- DependencyManager.planInstallRequirementsWith unusedEnv DependencyManager.Production requirements outline
    pure $
      case result of
        Left (DependencyManager.DuplicateRequirement duplicate) -> duplicate == package
        _ -> False

testUninstallConstraints :: IO Bool
testUninstallConstraints =
  let
    production = testPackage "author/production"
    testRoot = testPackage "author/test-root"
    version = ElmVersion.Version 1 0 0
    app = emptyDependencyApp
      { Outline._app_deps_direct = Map.singleton production version
      , Outline._app_test_direct = Map.singleton testRoot version
      }
    (productionAfterTestRemoval, testAfterTestRemoval) = DependencyManager.uninstallAppRoots DependencyManager.Test [testRoot] app
    (productionAfterProdRemoval, testAfterProdRemoval) = DependencyManager.uninstallAppRoots DependencyManager.Production [production] app
  in pure $
    Map.member production productionAfterTestRemoval
      && Map.null testAfterTestRemoval
      && Map.null productionAfterProdRemoval
      && Map.member testRoot testAfterProdRemoval

testScopedUpgradeConstraints :: IO Bool
testScopedUpgradeConstraints =
  let
    production = testPackage "author/production"
    testRoot = testPackage "author/test-root"
    version = ElmVersion.Version 1 2 3
    app = emptyDependencyApp
      { Outline._app_deps_direct = Map.singleton production version
      , Outline._app_test_direct = Map.singleton testRoot version
      }
    (productionRoots, testRoots) = DependencyManager.upgradeAppRoots DependencyManager.Production DependencyManager.Compatible Nothing False app
  in pure $
    maybe False (\constraint -> Constraint.satisfies constraint (ElmVersion.Version 1 9 0) && not (Constraint.satisfies constraint (ElmVersion.Version 2 0 0))) (Map.lookup production productionRoots)
      && Map.lookup testRoot testRoots == Just (Constraint.exactly version)

testAllScopesUpgradeConstraints :: IO Bool
testAllScopesUpgradeConstraints =
  let
    production = testPackage "author/production"
    testRoot = testPackage "author/test-root"
    version = ElmVersion.Version 1 2 3
    app = emptyDependencyApp
      { Outline._app_deps_direct = Map.singleton production version
      , Outline._app_test_direct = Map.singleton testRoot version
      }
    (productionRoots, testRoots) = DependencyManager.upgradeAppRoots DependencyManager.Production DependencyManager.AllowMajor Nothing True app
    acceptsMajor constraints name = maybe False (\constraint -> Constraint.satisfies constraint (ElmVersion.Version 3 0 0)) (Map.lookup name constraints)
  in pure (acceptsMajor productionRoots production && acceptsMajor testRoots testRoot)

testProductionDependencyTree :: IO Bool
testProductionDependencyTree =
  let
    (outline, solution, production, shared, testRoot, testOnly) = dependencyTreeFixture
    tree = DependencyManager.dependencyTreeFromSolution DependencyManager.Production outline solution
    names = map DependencyManager.treePackage (DependencyManager.treeNodes tree)
    nodeKind name = DependencyManager.treeKind <$> List.find ((== name) . DependencyManager.treePackage) (DependencyManager.treeNodes tree)
  in pure $
    DependencyManager.treeRoot tree == "project"
      && all (`elem` names) [production, shared]
      && all (`notElem` names) [testRoot, testOnly]
      && nodeKind production == Just DependencyManager.ProductionRoot
      && nodeKind shared == Just DependencyManager.Indirect

testCombinedTestDependencyTree :: IO Bool
testCombinedTestDependencyTree =
  let
    (outline, solution, _, _, testRoot, testOnly) = dependencyTreeFixture
    tree = DependencyManager.dependencyTreeFromSolution DependencyManager.Test outline solution
    names = map DependencyManager.treePackage (DependencyManager.treeNodes tree)
    nodeKind name = DependencyManager.treeKind <$> List.find ((== name) . DependencyManager.treePackage) (DependencyManager.treeNodes tree)
  in pure $
    all (`elem` names) [testRoot, testOnly]
      && nodeKind testRoot == Just DependencyManager.TestRoot
      && nodeKind testOnly == Just DependencyManager.Indirect

testDependencyTreeFiltering :: IO Bool
testDependencyTreeFiltering =
  let
    (outline, solution, production, shared, testRoot, testOnly) = dependencyTreeFixture
    tree = DependencyManager.dependencyTreeFromSolution DependencyManager.Test outline solution
    filtered = DependencyManager.filterDependencyTree shared tree
    names = map DependencyManager.treePackage (DependencyManager.treeNodes filtered)
  in pure $
    all (`elem` names) [production, shared, testRoot]
      && testOnly `notElem` names

dependencyTreeFixture :: (Outline.Outline, Map.Map Package.Name Solver.Details, Package.Name, Package.Name, Package.Name, Package.Name)
dependencyTreeFixture =
  let
    production = testPackage "author/production"
    shared = testPackage "author/shared"
    testRoot = testPackage "author/test-root"
    testOnly = testPackage "author/test-only"
    version = ElmVersion.Version 1 0 0
    app = emptyDependencyApp
      { Outline._app_deps_direct = Map.singleton production version
      , Outline._app_deps_indirect = Map.singleton shared version
      , Outline._app_test_direct = Map.singleton testRoot version
      , Outline._app_test_indirect = Map.singleton testOnly version
      }
    details dependencies = Solver.Details version (Map.fromList (map (\name -> (name, Constraint.anything)) dependencies))
    solution = Map.fromList
      [ (production, details [shared])
      , (shared, details [])
      , (testRoot, details [shared, testOnly])
      , (testOnly, details [])
      ]
  in (Outline.App app, solution, production, shared, testRoot, testOnly)

testUnusedDependencyCandidates :: IO Bool
testUnusedDependencyCandidates =
  let
    used = testPackage "author/used"
    unused = testPackage "author/unused"
    candidates = [Package.core, used, unused]
  in pure (DependencyManager.unusedDirectDependencies candidates (Set.singleton used) == [unused])

testDependencyChangesDeterministic :: IO Bool
testDependencyChangesDeterministic =
  let
    alpha = testPackage "author/alpha"
    beta = testPackage "author/beta"
    old = Map.fromList [(beta, 1 :: Int), (alpha, 1)]
    new = Map.fromList [(beta, 2 :: Int)]
  in pure $
    DependencyManager.detectChanges old new
      == [(alpha, DependencyManager.Removed 1), (beta, DependencyManager.Changed 1 2)]

testDependencyScopePromotion :: IO Bool
testDependencyScopePromotion =
  let
    package = testPackage "author/package"
    version = ElmVersion.Version 1 0 0
    old = emptyDependencyApp { Outline._app_test_direct = Map.singleton package version }
    new = emptyDependencyApp { Outline._app_deps_direct = Map.singleton package version }
  in pure $
    DependencyManager.diffAppOutlines old new
      == [DependencyManager.DependencyMoved DependencyManager.Test DependencyManager.Production package (DependencyManager.AppVersion version) (DependencyManager.AppVersion version)]

testDependencyReclassification :: IO Bool
testDependencyReclassification =
  let
    package = testPackage "author/package"
    version = ElmVersion.Version 1 0 0
    old = emptyDependencyApp { Outline._app_deps_direct = Map.singleton package version }
    new = emptyDependencyApp { Outline._app_deps_indirect = Map.singleton package version }
  in pure $
    DependencyManager.diffAppOutlines old new
      == [DependencyManager.DependencyReclassified DependencyManager.Production package DependencyManager.DirectDependency DependencyManager.IndirectDependency (DependencyManager.AppVersion version)]

testDependencyTransactionSuccess :: IO Bool
testDependencyTransactionSuccess =
  withDependencyTransactionFixture $ \root path original plan ->
    do  result <- DependencyManager.applyPlanWith root (\_ -> pure (Right ())) (pure (Right ())) plan
        disk <- BS.readFile path
        cached <- FileCache.lookup path
        pure (isRightUnit result && disk /= original && maybe False ((== disk) . snd) cached)

testDependencyTransactionVerificationRollback :: IO Bool
testDependencyTransactionVerificationRollback =
  withDependencyTransactionFixture $ \root path original plan ->
    do  result <- DependencyManager.applyPlanWith root (\_ -> pure (Left DependencyManager.NoSolution)) (pure (Right ())) plan
        disk <- BS.readFile path
        cached <- FileCache.lookup path
        pure (isNoSolution result && disk == original && maybe False ((== original) . snd) cached)

testDependencyTransactionRefreshRollback :: IO Bool
testDependencyTransactionRefreshRollback =
  withDependencyTransactionFixture $ \root path original plan ->
    do  let testPath = root FilePath.</> "elm-stuff" FilePath.</> "elm-dev-test" FilePath.</> "elm.json"
            oldTestBytes = BS.pack [1, 2, 3]
            newTestBytes = BS.pack [4, 5, 6]
        Dir.createDirectoryIfMissing True (root FilePath.</> "elm-stuff" FilePath.</> "elm-dev-test")
        BS.writeFile testPath oldTestBytes
        FileCache.insert testPath oldTestBytes
        let failingRefresh = BS.writeFile testPath newTestBytes >> FileCache.insert testPath newTestBytes >> pure (Left DependencyManager.NoOfflineSolution)
        result <- DependencyManager.applyPlanWith root (\_ -> pure (Right ())) failingRefresh plan
        disk <- BS.readFile path
        cached <- FileCache.lookup path
        testDisk <- BS.readFile testPath
        testCached <- FileCache.lookup testPath
        pure (isNoOfflineSolution result && disk == original && maybe False ((== original) . snd) cached && testDisk == oldTestBytes && maybe False ((== oldTestBytes) . snd) testCached)

testDependencyTransactionRejectsStalePlan :: IO Bool
testDependencyTransactionRejectsStalePlan =
  withDependencyTransactionFixture $ \root path _ plan ->
    do  let staleOutline = addTestDependency (testPackage "author/concurrent") (DependencyManager.oldOutline plan)
            staleBytes = dependencyOutlineBytes staleOutline
        BS.writeFile path staleBytes
        FileCache.insert path staleBytes
        result <- DependencyManager.applyPlanWith root (\_ -> pure (Right ())) (pure (Right ())) plan
        disk <- BS.readFile path
        pure (isStalePlan result && disk == staleBytes)

testDependencyTransactionRejectsUnsavedCache :: IO Bool
testDependencyTransactionRejectsUnsavedCache =
  withDependencyTransactionFixture $ \root path original plan ->
    do  let unsaved = BS.snoc original 32
        FileCache.insert path unsaved
        result <- DependencyManager.applyPlanWith root (\_ -> pure (Right ())) (pure (Right ())) plan
        disk <- BS.readFile path
        cached <- FileCache.lookup path
        pure (isUnsavedChanges result && disk == original && maybe False ((== unsaved) . snd) cached)

testDependencyTransactionExceptionRollback :: IO Bool
testDependencyTransactionExceptionRollback =
  withDependencyTransactionFixture $ \root path original plan ->
    do  result <- Exception.try (DependencyManager.applyPlanWith root (\_ -> Exception.throwIO (userError "verification crashed")) (pure (Right ())) plan) :: IO (Either Exception.SomeException (Either DependencyManager.Error ()))
        disk <- BS.readFile path
        cached <- FileCache.lookup path
        pure (isLeftException result && disk == original && maybe False ((== original) . snd) cached)

testDependencyPlanDetectsOutlineChange :: IO Bool
testDependencyPlanDetectsOutlineChange =
  let
    package = testPackage "author/shared"
    version = ElmVersion.Version 1 0 0
    oldApp = emptyDependencyApp
      { Outline._app_deps_indirect = Map.singleton package version
      , Outline._app_test_direct = Map.singleton package version
      }
    newApp = oldApp { Outline._app_test_direct = Map.empty }
    plan = DependencyManager.Plan (Outline.App oldApp) (Outline.App newApp) []
  in pure (DependencyManager.planChanged plan)

testMcpDependencyArgumentValidation :: IO Bool
testMcpDependencyArgumentValidation =
  let
    valid = jsonObject ["dir" JSON..= ("/project" :: Text.Text), "scope" JSON..= ("test" :: Text.Text), "packages" JSON..= (["elm/http"] :: [Text.Text]), "dryRun" JSON..= True]
    badPackages = jsonObject ["packages" JSON..= ("elm/http" :: Text.Text)]
    badScope = jsonObject ["scope" JSON..= JSON.Null]
    badDryRun = jsonObject ["dryRun" JSON..= ("false" :: Text.Text)]
    badDir = jsonObject ["dir" JSON..= JSON.Null]
    unknownField = jsonObject ["operation" JSON..= ("upgrade" :: Text.Text), "dryrun" JSON..= True]
    installWithUnsafe = jsonObject ["unsafe" JSON..= True]
    unusedWithPackages = jsonObject ["packages" JSON..= (["elm/http"] :: [Text.Text])]
    validSize = jsonObject ["entrypoint" JSON..= ("src/Main.elm" :: Text.Text), "filter" JSON..= (["App", "Ui.Button"] :: [Text.Text])]
    sizeWithoutEntrypoint = jsonObject ["filter" JSON..= (["App"] :: [Text.Text])]
    sizeWithScope = jsonObject ["entrypoint" JSON..= ("src/Main.elm" :: Text.Text), "scope" JSON..= ("production" :: Text.Text)]
  in pure $
    MCP.dependencyArgumentsValidForTests valid
      && all (not . MCP.dependencyArgumentsValidForTests) [badPackages, badScope, badDryRun, badDir, unknownField]
      && not (MCP.dependencyOperationArgumentsValidForTests "install" installWithUnsafe)
      && not (MCP.dependencyOperationArgumentsValidForTests "uninstall-unused" unusedWithPackages)
      && MCP.dependencyArgumentsValidForTests validSize
      && MCP.dependencyOperationArgumentsValidForTests "size" validSize
      && not (MCP.dependencyOperationArgumentsValidForTests "size" sizeWithoutEntrypoint)
      && not (MCP.dependencyOperationArgumentsValidForTests "size" sizeWithScope)

testDependencySizeNamespaceFilters :: IO Bool
testDependencySizeNamespaceFilters =
  pure $
    DependencySize.matchesFilter "Glyph" "Glyph"
      && DependencySize.matchesFilter "Glyph" "Glyph.Editor.View"
      && DependencySize.matchesFilter "Ui.Button" "Ui.Button"
      && DependencySize.matchesFilter "Ui.Button" "Ui.Button.Icon"
      && not (DependencySize.matchesFilter "Glyph" "Glyphs")
      && not (DependencySize.matchesFilter "Ui.Button" "Ui.Buttons")

testDependencySizeAttribution :: IO Bool
testDependencySizeAttribution =
  Temp.withSystemTempDirectory "elm-dev-dependency-size" $ \root -> do
    let src = root FilePath.</> "src"
        mainPath = src FilePath.</> "Main.elm"
    Dir.createDirectoryIfMissing True src
    writeFile (root FilePath.</> "elm.json") basicElmJson
    writeFile mainPath $ unlines
      [ "module Main exposing (main)"
      , "import Debug"
      , "import Platform"
      , "main = Platform.worker { init = \\() -> (Debug.log \"init\" (), Cmd.none), update = \\_ model -> (model, Cmd.none), subscriptions = \\_ -> Sub.none }"
      ]
    result <- DependencySize.analyze root "src/Main.elm" [] Nothing
    pure $ case result of
      Left _ -> False
      Right report ->
        let attributed = sum (map DependencySize.moduleBytes (DependencySize.modules report)) + DependencySize.elmLanguageBytes report
            development = DependencySize.developmentBytes (DependencySize.bundle report)
        in attributed == development
          && any ((== "Main") . DependencySize.moduleName) (DependencySize.modules report)
          && DependencySize.productionOptimizedBytes (DependencySize.bundle report) == Nothing
          && maybe False (List.isInfixOf "Debug") (DependencySize.productionUnavailable (DependencySize.bundle report))

testMcpDependencyPlanJson :: IO Bool
testMcpDependencyPlanJson =
  let
    outline = Outline.App emptyDependencyApp
    plan = DependencyManager.Plan outline outline []
    bytes = TextEncoding.encodeUtf8 (MCP.dependencyPlanJsonForTests "upgrade" DependencyManager.Test "planned" False plan)
  in pure $
    case JSON.decodeStrict bytes of
      Just (JSON.Object object) ->
        KeyMap.lookup "operation" object == Just (JSON.String "upgrade")
          && KeyMap.lookup "scope" object == Just (JSON.String "test")
      _ -> False

testMcpDependencyTreeJson :: IO Bool
testMcpDependencyTreeJson =
  let
    directPackage = testPackage "author/direct"
    indirectPackage = testPackage "author/indirect"
    directNode = DependencyManager.TreeNode directPackage (ElmVersion.Version 2 0 0) DependencyManager.ProductionRoot [indirectPackage]
    indirectNode = DependencyManager.TreeNode indirectPackage (ElmVersion.Version 1 0 0) DependencyManager.Indirect []
    tree = DependencyManager.DependencyTree "project" DependencyManager.Production [directNode, indirectNode]
    bytes = TextEncoding.encodeUtf8 (MCP.dependencyTreeJsonForTests (Set.singleton directPackage) tree)
  in pure $
    case JSON.decodeStrict bytes of
      Just (JSON.Object object) ->
        case (KeyMap.lookup "direct" object, KeyMap.lookup "indirect" object) of
          (Just (JSON.Array directValues), Just (JSON.Array indirectValues)) ->
            case (Foldable.toList directValues, Foldable.toList indirectValues) of
              ([JSON.Object direct], [JSON.Object indirect]) ->
                KeyMap.lookup "unused" direct == Just (JSON.Bool True)
                  && KeyMap.lookup "usedBy" direct == Just (JSON.Array mempty)
                  && dependencyReferenceMatches indirectPackage (KeyMap.lookup "dependencies" direct)
                  && KeyMap.lookup "unused" indirect == Just (JSON.Bool False)
                  && KeyMap.lookup "usedBy" indirect == Just (JSON.toJSON (["author/direct"] :: [String]))
              _ -> False
          _ -> False
      _ -> False


dependencyReferenceMatches :: Package.Name -> Maybe JSON.Value -> Bool
dependencyReferenceMatches expected value =
  case value of
    Just (JSON.Array dependencies) ->
      case Foldable.toList dependencies of
        [JSON.Object dependency] ->
          KeyMap.lookup "package" dependency == Just (JSON.String (Text.pack (Package.toChars expected)))
            && KeyMap.lookup "version" dependency == Just (JSON.String "1.0.0")
            && KeyMap.lookup "kind" dependency == Just (JSON.String "indirect")
        _ -> False
    _ -> False


testDependencyCliRemovalRendering :: IO Bool
testDependencyCliRemovalRendering =
  let
    directPackage = testPackage "author/direct"
    indirectPackage = testPackage "author/indirect"
    directVersion = ElmVersion.Version 2 0 0
    indirectVersion = ElmVersion.Version 1 0 0
    oldApp = emptyDependencyApp
      { Outline._app_deps_direct = Map.singleton directPackage directVersion
      , Outline._app_deps_indirect = Map.singleton indirectPackage indirectVersion
      }
    plan = DependencyManager.Plan
      (Outline.App oldApp)
      (Outline.App emptyDependencyApp)
      [ DependencyManager.DependencyRemoved DependencyManager.Production directPackage (DependencyManager.AppVersion directVersion)
      , DependencyManager.DependencyRemoved DependencyManager.Production indirectPackage (DependencyManager.AppVersion indirectVersion)
      ]
  in pure $
    DependencyCli.renderPlanForTests "uninstall-unused" DependencyManager.Production plan
      == [ "Unused direct dependency"
         , ""
         , "  author/direct @ 2.0.0"
         , ""
         , "Also removing 1 now-unneeded indirect dependency"
         , ""
         , "  author/indirect @ 1.0.0"
         ]


testOfflineSolverCacheBehavior :: IO Bool
testOfflineSolverCacheBehavior =
  withOfflineDependencyFixture $ \fixture ->
    do  let Solver.Env cache manager _ registry = offlineEnv fixture
            package = fixtureUpgradePackage fixture
            exactTwo = Map.singleton package (Constraint.exactly (ElmVersion.Version 2 0 0))
            impossible = Map.singleton package (Constraint.exactly (ElmVersion.Version 3 0 0))
            source = Stuff.package cache package (ElmVersion.Version 2 0 0) FilePath.</> "src"
        warmOffline <- Solver.verify cache Solver.Offline registry exactTwo
        Dir.removeDirectoryRecursive source
        missingSourceOffline <- Solver.verify cache Solver.Offline registry exactTwo
        metadataOnlyOnline <- Solver.verify cache (Solver.Online manager) registry exactTwo
        impossibleOffline <- Solver.verify cache Solver.Offline registry impossible
        impossibleOnline <- Solver.verify cache (Solver.Online manager) registry impossible
        pure $
          isSolverOk warmOffline
            && isSolverNoOfflineSolution missingSourceOffline
            && isSolverOk metadataOnlyOnline
            && isSolverNoOfflineSolution impossibleOffline
            && isSolverNoSolution impossibleOnline


testOfflineApplicationPlanMatrix :: IO Bool
testOfflineApplicationPlanMatrix =
  withOfflineDependencyFixture $ \fixture ->
    do  let coreVersion = ElmVersion.Version 1 0 5
            jsonVersion = ElmVersion.Version 1 1 3
            oldUpgradeVersion = ElmVersion.Version 1 0 0
            upgradePackage = fixtureUpgradePackage fixture
            extraPackage = fixtureExtraPackage fixture
            baseApp = emptyDependencyApp
              { Outline._app_deps_direct = Map.fromList
                  [ (Package.core, coreVersion)
                  , (Package.json, jsonVersion)
                  , (upgradePackage, oldUpgradeVersion)
                  ]
              }
            baseOutline = Outline.App baseApp
        installed <- DependencyManager.planInstallWith (offlineEnv fixture) DependencyManager.Production [extraPackage] baseOutline
        uninstalled <- DependencyManager.planUninstallWith (offlineEnv fixture) DependencyManager.Production [upgradePackage] baseOutline
        safeUpgrade <- DependencyManager.planUpgradeWith (offlineEnv fixture) DependencyManager.Production DependencyManager.Compatible (Just [upgradePackage]) False baseOutline
        unsafeUpgrade <- DependencyManager.planUpgradeWith (offlineEnv fixture) DependencyManager.Production DependencyManager.AllowMajor (Just [upgradePackage]) False baseOutline
        pure $
          appDirectVersion extraPackage installed == Just (ElmVersion.Version 1 0 0)
            && appDirectVersion upgradePackage uninstalled == Nothing
            && appDirectVersion upgradePackage safeUpgrade == Just (ElmVersion.Version 1 1 0)
            && appDirectVersion upgradePackage unsafeUpgrade == Just (ElmVersion.Version 2 0 0)


testOfflinePackagePlanMatrix :: IO Bool
testOfflinePackagePlanMatrix =
  withOfflineDependencyFixture $ \fixture ->
    do  let projectRoot = fixtureRoot fixture FilePath.</> "project"
            package = fixtureUpgradePackage fixture
        Dir.createDirectoryIfMissing True projectRoot
        writeFile (projectRoot FilePath.</> "elm.json") basicPackageElmJson
        eitherOutline <- Outline.read projectRoot
        case eitherOutline of
          Left _ -> pure False
          Right outline ->
            do  planned <- DependencyManager.planInstallRequirementsWith
                  (offlineEnv fixture)
                  DependencyManager.Production
                  [DependencyManager.PackageRequirement package (DependencyManager.Major 1)]
                  outline
                pure $
                  case planned of
                    Right plan ->
                      case DependencyManager.newOutline plan of
                        Outline.Pkg pkg ->
                          case Map.lookup package (Outline._pkg_deps pkg) of
                            Just constraint ->
                              Constraint.satisfies constraint (ElmVersion.Version 1 1 0)
                                && not (Constraint.satisfies constraint (ElmVersion.Version 1 0 0))
                                && not (Constraint.satisfies constraint (ElmVersion.Version 2 0 0))
                            Nothing -> False
                        Outline.App _ -> False
                    Left _ -> False


data OfflineDependencyFixture =
  OfflineDependencyFixture
    { fixtureRoot :: FilePath
    , offlineEnv :: Solver.Env
    , fixtureUpgradePackage :: Package.Name
    , fixtureExtraPackage :: Package.Name
    }


withOfflineDependencyFixture :: (OfflineDependencyFixture -> IO Bool) -> IO Bool
withOfflineDependencyFixture run =
  Temp.withSystemTempDirectory "elm-dev-dependency-cache" $ \root ->
    withEnvironmentVariable "ELM_DEV_HOME" root $
      do  cache <- Stuff.getPackageCache
          manager <- Http.getManager
          let upgradePackage = testPackage "author/upgrade"
              extraPackage = testPackage "author/extra"
              one = ElmVersion.Version 1 0 0
              oneOne = ElmVersion.Version 1 1 0
              two = ElmVersion.Version 2 0 0
              registry = Registry.Registry 7 (Map.fromList
                [ (Package.core, Registry.KnownVersions (ElmVersion.Version 1 0 5) [])
                , (Package.json, Registry.KnownVersions (ElmVersion.Version 1 1 3) [])
                , (upgradePackage, Registry.KnownVersions two [oneOne, one])
                , (extraPackage, Registry.KnownVersions one [])
                ])
              env = Solver.Env cache manager Solver.Offline registry
          writeCachedPackage cache Package.core (ElmVersion.Version 1 0 5) False
          writeCachedPackage cache Package.json (ElmVersion.Version 1 1 3) True
          mapM_ (\version -> writeCachedPackage cache upgradePackage version True) [one, oneOne, two]
          writeCachedPackage cache extraPackage one True
          run (OfflineDependencyFixture root env upgradePackage extraPackage)


withEnvironmentVariable :: String -> String -> IO a -> IO a
withEnvironmentVariable name value action =
  Exception.bracket
    (do previous <- Environment.lookupEnv name; Environment.setEnv name value; pure previous)
    (\previous -> case previous of Nothing -> Environment.unsetEnv name; Just old -> Environment.setEnv name old)
    (\_ -> action)


writeCachedPackage :: Stuff.PackageCache -> Package.Name -> ElmVersion.Version -> Bool -> IO ()
writeCachedPackage cache package version dependsOnCore =
  do  let root = Stuff.package cache package version
          dependencies =
            if dependsOnCore
            then "{ \"elm/core\": \"1.0.0 <= v < 2.0.0\" }"
            else "{}"
          contents = unlines
            [ "{"
            , "  \"type\": \"package\","
            , "  \"name\": \"" ++ Package.toChars package ++ "\","
            , "  \"summary\": \"Synthetic dependency fixture\","
            , "  \"license\": \"BSD-3-Clause\","
            , "  \"version\": \"" ++ ElmVersion.toChars version ++ "\","
            , "  \"exposed-modules\": [\"Synthetic\"],"
            , "  \"elm-version\": \"0.19.0 <= v < 0.20.0\","
            , "  \"dependencies\": " ++ dependencies ++ ","
            , "  \"test-dependencies\": {}"
            , "}"
            ]
      Dir.createDirectoryIfMissing True (root FilePath.</> "src")
      writeFile (root FilePath.</> "elm.json") contents


appDirectVersion :: Package.Name -> Either DependencyManager.Error DependencyManager.Plan -> Maybe ElmVersion.Version
appDirectVersion package result =
  case result of
    Right plan ->
      case DependencyManager.newOutline plan of
        Outline.App app -> Map.lookup package (Outline._app_deps_direct app)
        Outline.Pkg _ -> Nothing
    Left _ -> Nothing


isSolverOk :: Solver.Result value -> Bool
isSolverOk result =
  case result of
    Solver.Ok _ -> True
    _ -> False


isSolverNoSolution :: Solver.Result value -> Bool
isSolverNoSolution result =
  case result of
    Solver.NoSolution -> True
    _ -> False


isSolverNoOfflineSolution :: Solver.Result value -> Bool
isSolverNoOfflineSolution result =
  case result of
    Solver.NoOfflineSolution -> True
    _ -> False


testLegacyMcpInstallResponse :: IO Bool
testLegacyMcpInstallResponse =
  pure $
    case MCP.legacyInstallResponseForTests "Already installed" Package.http of
      MCPProtocol.ToolCallResponse [MCPProtocol.ToolResponseText _ "Already installed", MCPProtocol.ToolResponseResourceLink _ link] ->
        MCPProtocol.resourceLinkUri link == "elm://docs/package/elm/http"
      _ -> False

testCommandParserJsonMode :: IO Bool
testCommandParserJsonMode =
  let
    beforePackage = CommandParser.parseArgs ["install", "--format", "json", "elm/http"]
    betweenPackages = CommandParser.parseArgs ["install", "elm/http", "--format", "json", "elm/json"]
  in pure $
    CommandParser.jsonRequested (CommandParser.parseArgs ["install", "elm/http", "--format=json"])
      && CommandParser.jsonRequested beforePackage
      && not (CommandParser.jsonRequested (CommandParser.parseArgs ["install", "elm/http"]))
      && CommandParser.parsedCommands beforePackage == ["install", "elm/http"]
      && CommandParser.parsedCommands betweenPackages == ["install", "elm/http", "elm/json"]
      && CommandParser.isVersionRequest ["--version"]
      && CommandParser.isVersionRequest ["--version-full"]
      && not (CommandParser.isVersionRequest ["--version-full", "extra"])

jsonObject :: [JSONTypes.Pair] -> JSON.Object
jsonObject pairs =
  case JSON.object pairs of
    JSON.Object object -> object
    _ -> KeyMap.empty

isRightUnit :: Either DependencyManager.Error () -> Bool
isRightUnit result =
  case result of
    Right () -> True
    Left _ -> False

isNoSolution :: Either DependencyManager.Error () -> Bool
isNoSolution result =
  case result of
    Left DependencyManager.NoSolution -> True
    _ -> False

isNoOfflineSolution :: Either DependencyManager.Error () -> Bool
isNoOfflineSolution result =
  case result of
    Left DependencyManager.NoOfflineSolution -> True
    _ -> False

isStalePlan :: Either DependencyManager.Error () -> Bool
isStalePlan result =
  case result of
    Left DependencyManager.StalePlan -> True
    _ -> False

isUnsavedChanges :: Either DependencyManager.Error () -> Bool
isUnsavedChanges result =
  case result of
    Left DependencyManager.UnsavedChanges -> True
    _ -> False

isLeftException :: Either Exception.SomeException value -> Bool
isLeftException result =
  case result of
    Left _ -> True
    Right _ -> False

withDependencyTransactionFixture :: (FilePath -> FilePath -> BS.ByteString -> DependencyManager.Plan -> IO Bool) -> IO Bool
withDependencyTransactionFixture run =
  Exception.bracket create cleanup use
  where
    create =
      do  root <- uniqueRoot
          Dir.createDirectoryIfMissing True root
          Dir.createDirectoryIfMissing True (root FilePath.</> "src")
          let path = root FilePath.</> "elm.json"
              oldApp = emptyDependencyApp
                { Outline._app_deps_direct = Map.fromList
                    [ (Package.core, ElmVersion.Version 1 0 5)
                    , (Package.json, ElmVersion.Version 1 1 3)
                    ]
                }
              oldOutline = Outline.App oldApp
              original = dependencyOutlineBytes oldOutline
          BS.writeFile path original
          FileCache.insert path original
          pure (root, path, original, oldOutline)

    cleanup (root, _, _, _) =
      do  FileCache.removeDir root
          exists <- Dir.doesDirectoryExist root
          when exists (Dir.removeDirectoryRecursive root)

    use (root, path, original, oldOutline) =
      let
        package = testPackage "author/package"
        version = ElmVersion.Version 1 0 0
        old = oldOutline
        new = addTestDependency package old
        change = DependencyManager.DependencyAdded DependencyManager.Production package (DependencyManager.AppVersion version)
        plan = DependencyManager.Plan old new [change]
      in run root path original plan

dependencyOutlineBytes :: Outline.Outline -> BS.ByteString
dependencyOutlineBytes outline =
  LazyChar8.toStrict (Builder.toLazyByteString (Encode.encode (Outline.encode outline) <> Builder.char7 '\n'))

addTestDependency :: Package.Name -> Outline.Outline -> Outline.Outline
addTestDependency package outline =
  case outline of
    Outline.App app -> Outline.App (app { Outline._app_deps_direct = Map.insert package (ElmVersion.Version 1 0 0) (Outline._app_deps_direct app) })
    Outline.Pkg pkg -> Outline.Pkg pkg

emptyDependencyApp :: Outline.AppOutline
emptyDependencyApp =
  Outline.AppOutline ElmVersion.compiler (NE.singleton (Outline.RelativeSrcDir "src")) Map.empty Map.empty Map.empty Map.empty

testPackage :: String -> Package.Name
testPackage raw =
  case break (== '/') raw of
    (author, '/' : project) -> Package.toName (Utf8.fromChars author) project
    _ -> error "invalid dependency test package"

testDaemonStateRoundTrip :: IO Bool
testDaemonStateRoundTrip = do
  let state = DaemonState.StateInfo
        { DaemonState.pid = 1
        , DaemonState.domain = "127.0.0.1"
        , DaemonState.lspPort = 2
        , DaemonState.mcpPort = 3
        , DaemonState.httpPort = 4
        , DaemonState.startedAt = "now"
        , DaemonState.version = "1.0.0"
        }
      legacy = "{\"pid\":1,\"domain\":\"127.0.0.1\",\"lspPort\":2,\"mcpPort\":3,\"httpPort\":4,\"startedAt\":\"now\",\"version\":\"1.0.0\",\"executableFingerprint\":\"old\"}"
  pure $ case (JSON.eitherDecode (JSON.encode state), JSON.eitherDecode legacy) of
    (Right decoded, Right legacyDecoded) -> decoded == state && legacyDecoded == state
    _ -> False

-- Exercise the real background startup path and its TCP MCP handler. Calling
-- runIsolatedWith from this test process alone retains normal terminal streams
-- and misses the descriptor reuse caused by a daemon launched with NoStream.
testDetachedDaemonWorker :: IO Bool
testDetachedDaemonWorker =
  Temp.withSystemTempDirectory "elm-dev-detached-worker" $ \root ->
    withEnvironmentVariable "ELM_DEV_HOME" (root FilePath.</> "home") $
    withEnvironmentVariable "ELM_DEV_TEST_DAEMON_CWD" root $ do
      let project = root FilePath.</> "project"
      writeElmApp project
      Dir.createDirectoryIfMissing True (project FilePath.</> "src")
      BS.writeFile (project FilePath.</> "src/Main.elm") "module Main exposing (main)\nimport Platform\nmain = Platform.worker { init = \\_ -> ( (), Cmd.none ), update = \\_ model -> ( model, Cmd.none ), subscriptions = \\_ -> Sub.none }\n"
      Exception.bracket Daemon.start (\_ -> Daemon.stop) $ \daemon -> do
        addresses <- Socket.getAddrInfo Nothing (Just (Daemon.domain daemon)) (Just (show (Daemon.mcpPort daemon)))
        let address = head addresses
        Exception.bracket (Socket.socket (Socket.addrFamily address) Socket.Stream Socket.defaultProtocol) Socket.close $ \socket -> do
          Socket.connect socket (Socket.addrAddress address)
          handle <- Socket.socketToHandle socket IO.ReadWriteMode
          IO.hSetBuffering handle IO.NoBuffering
          result <- Timeout.timeout (20 * 1000000) $ do
            _ <- daemonRequest handle 1 "initialize" (JSON.object
              [ "protocolVersion" JSON..= ("2024-11-05" :: String)
              , "capabilities" JSON..= JSON.object []
              , "clientInfo" JSON..= JSON.object ["name" JSON..= ("detached-worker-test" :: String), "version" JSON..= ("1" :: String)]
              ])
            responses <- mapM (\requestId -> daemonRequest handle requestId "tools/call" (JSON.object
              [ "name" JSON..= ("check" :: String)
              , "arguments" JSON..= JSON.object
                  [ "dir" JSON..= project
                  , "tests" JSON..= JSON.object ["kind" JSON..= ("all" :: String), "waitSeconds" JSON..= (5 :: Int)]
                  ]
              ])) [2..4]
            let passed = all (List.isInfixOf "worker result" . LazyChar8.unpack) responses
            unless passed (mapM_ (putStrLn . LazyChar8.unpack) responses)
            pure passed
          IO.hClose handle
          pure (result == Just True)

daemonRequest :: IO.Handle -> Int -> String -> JSON.Value -> IO LazyChar8.ByteString
daemonRequest handle requestId method params = do
  ServerRun.sendWithContentLengthHandle handle (JSON.encode (JSON.object
    [ "jsonrpc" JSON..= ("2.0" :: String), "id" JSON..= requestId, "method" JSON..= method, "params" JSON..= params ]))
  let receive = do
        response <- ServerRun.readHandleMessage handle
        case response of
          Nothing -> fail "Detached daemon disconnected"
          Just bytes -> case JSON.eitherDecode bytes of
            Right (JSON.Object object) | KeyMap.lookup "id" object == Just (JSON.toJSON requestId) -> pure bytes
            _ -> receive
  receive

testMcpRelativeDirs :: IO Bool
testMcpRelativeDirs = do
  state <- Client.initState testUrls
  results <- sequence
    [ MCPProtocol.call tool (KeyMap.singleton "dir" (JSON.String dir)) state (\_ -> pure ()) "path-test"
    | tool <- MCP.availableToolsForTests
    , dir <- ["app", ".", "../app", ""]
    ]
  pure (all rejected results)
  where
    rejected (MCPProtocol.ToolCallResponse [MCPProtocol.ToolResponseError _ msg]) =
      Text.isInfixOf "absolute filesystem path" msg
    rejected _ = False

testMcpDiscoveryError :: IO Bool
testMcpDiscoveryError = do
  root <- uniqueRoot
  writeElmApp root
  state <- Client.initState testUrls
  let args = KeyMap.fromList
        [ ("dir", JSON.String (Text.pack root))
        , ("targets", JSON.object ["kind" JSON..= ("all" :: Text.Text)])
        ]
  case List.find ((== "check") . MCPProtocol.toolName) MCP.availableToolsForTests of
    Nothing -> pure False
    Just tool -> do
      response <- MCPProtocol.call tool args state (\_ -> pure ()) "discovery-test"
      pure $ case response of
        MCPProtocol.ToolCallResponse [MCPProtocol.ToolResponseError _ msg] ->
          Text.isInfixOf "src" msg && Text.isInfixOf "SOURCE" msg
        _ -> False

testMcpBehavioralSpecsGuide :: IO Bool
testMcpBehavioralSpecsGuide = do
  state <- Client.initState testUrls
  case List.find ((== "Behavioral Specs") . MCPProtocol.resourceName) MCP.availableResourcesForTests of
    Nothing -> pure False
    Just resource -> do
      response <- MCPProtocol.read resource
        (MCPProtocol.ReadResourceRequest "file://guides/behavioral-specs") state (\_ -> pure ()) "spec-guide-test"
      pure $ case MCPProtocol.readResourceContents response of
        [content] -> MCPProtocol.resourceContentUri content == "file://guides/behavioral-specs"
          && all (`Text.isInfixOf` MCPProtocol.resourceContentText content)
            ["test init --install", "Spec.outgoingPort", "Spec.advanceTime", "pyreStoreOut", "customize App.Spec.Effect"]
        _ -> False


testMcpCompositeToolsExposed :: IO Bool
testMcpCompositeToolsExposed = do
  let names = MCP.availableToolNamesForTests
  pure (all (`elem` names) ["docs", "add", "check"])

testMcpRetiredToolsHidden :: IO Bool
testMcpRetiredToolsHidden = do
  let names = MCP.availableToolNamesForTests
      retired =
        [ "docs_package"
        , "docs_module"
        , "docs_file"
        , "docs_value"
        , "docs_search"
        , "docs_modules"
        , "docs_project"
        , "add_page"
        , "add_store"
        , "add_effect"
        , "add_listener"
        , "add_theme"
        , "diagnostics"
        , "test_run"
        , "compile"
        , "project_select"
        ]
  pure (not (any (`elem` names) retired))

testMcpToolSurfaceExact :: IO Bool
testMcpToolSurfaceExact = do
  let names = List.sort MCP.availableToolNamesForTests
      expected = List.sort
        [ "add"
        , "check"
        , "docs"
        , "generate_scaffold_app"
        , "generate_scaffold_package"
        , "install"
        , "dependencies"
        , "test_install"
        ]
  pure (names == expected)

testMcpCompositeSchemas :: IO Bool
testMcpCompositeSchemas = do
  let schemas = MCP.availableToolSchemasForTests
      docsOk = schemaHasProperty "queries" schemas "docs"
        && schemaRequires "queries" schemas "docs"
        && schemaDescriptionMentions ["project", "modules", "module", "file", "value", "search", "package"] schemas "docs" "queries"
      addOk = schemaHasProperty "additions" schemas "add"
        && schemaRequires "additions" schemas "add"
        && schemaHasProperty "dir" schemas "add"
        && schemaDescriptionMentions ["page", "store", "effect", "listener", "theme"] schemas "add" "additions"
      checkOk = schemaHasProperty "dir" schemas "check"
        && schemaHasProperty "targets" schemas "check"
        && schemaHasProperty "tests" schemas "check"
        && schemaHasProperty "testJobs" schemas "check"
        && schemaDescriptionMentions ["all", "only"] schemas "check" "targets"
        && schemaDescriptionMentions ["all", "only"] schemas "check" "tests"
      dependenciesOk = schemaHasProperty "dir" schemas "dependencies"
        && schemaHasProperty "operation" schemas "dependencies"
        && schemaHasProperty "scope" schemas "dependencies"
        && schemaHasProperty "packages" schemas "dependencies"
        && schemaHasProperty "unsafe" schemas "dependencies"
        && schemaHasProperty "allScopes" schemas "dependencies"
        && schemaHasProperty "dryRun" schemas "dependencies"
        && schemaRequires "operation" schemas "dependencies"
        && schemaDescriptionMentions ["author/project", "MAJOR"] schemas "dependencies" "packages"
  pure (docsOk && addOk && checkOk && dependenciesOk)

testMcpDocsProjectResolution :: IO Bool
testMcpDocsProjectResolution = do
  let localModule = JSON.object ["kind" JSON..= ("module" :: Text.Text), "module" JSON..= ("Ui.Input" :: Text.Text)]
      package = JSON.object ["kind" JSON..= ("package" :: Text.Text), "package" JSON..= ("elm/json" :: Text.Text)]
      packageSearch = JSON.object ["kind" JSON..= ("search" :: Text.Text), "query" JSON..= ("decoder" :: Text.Text), "package" JSON..= ("elm/json" :: Text.Text)]
      projectSearch = JSON.object ["kind" JSON..= ("search" :: Text.Text), "query" JSON..= ("decoder" :: Text.Text)]
  pure $
    MCP.docsQueriesNeedProjectForTests Nothing [localModule]
      && MCP.docsQueriesNeedProjectForTests Nothing [projectSearch]
      && MCP.docsQueriesNeedProjectForTests Nothing [package, localModule]
      && MCP.docsQueriesNeedProjectForTests (Just "/tmp/project") [packageSearch]
      && not (MCP.docsQueriesNeedProjectForTests Nothing [package])
      && not (MCP.docsQueriesNeedProjectForTests (Just "/tmp/project") [package])
      && not (MCP.docsQueriesNeedProjectForTests Nothing [packageSearch])

schemaHasProperty :: Text.Text -> [(Text.Text, JSON.Value)] -> Text.Text -> Bool
schemaHasProperty propertyName schemas toolName =
  case lookup toolName schemas >>= objectProperties of
    Just props -> KeyMap.member (Key.fromText propertyName) props
    Nothing -> False

schemaRequires :: Text.Text -> [(Text.Text, JSON.Value)] -> Text.Text -> Bool
schemaRequires fieldName schemas toolName =
  case lookup toolName schemas >>= objectRequired of
    Just required -> JSON.String fieldName `elem` required
    Nothing -> False

schemaDescriptionMentions :: [Text.Text] -> [(Text.Text, JSON.Value)] -> Text.Text -> Text.Text -> Bool
schemaDescriptionMentions needles schemas toolName propertyName =
  case lookup toolName schemas >>= objectProperties >>= KeyMap.lookup (Key.fromText propertyName) of
    Just (JSON.Object prop) ->
      case KeyMap.lookup "description" prop of
        Just (JSON.String desc) -> all (`Text.isInfixOf` desc) needles
        _ -> False
    _ -> False

objectProperties :: JSON.Value -> Maybe JSON.Object
objectProperties value =
  case value of
    JSON.Object obj ->
      case KeyMap.lookup "properties" obj of
        Just (JSON.Object props) -> Just props
        _ -> Nothing
    _ -> Nothing

objectRequired :: JSON.Value -> Maybe [JSON.Value]
objectRequired value =
  case value of
    JSON.Object obj ->
      case KeyMap.lookup "required" obj of
        Just (JSON.Array required) -> Just (Foldable.toList required)
        _ -> Nothing
    _ -> Nothing

testMcpClearTestResults :: IO Bool
testMcpClearTestResults = do
  testVar <- STM.newTVarIO (Just (Client.TestInfo
    { Client.testFiles = ["tests/Main.elm"]
    , Client.testResults = Just (Client.TestResults 3 3 0 [])
    , Client.testCompilation = Just Client.TestSuccess
    }))
  STM.atomically $ CompileState.clearTestResults testVar
  cleared <- STM.readTVarIO testVar
  pure $ case cleared of
    Just info ->
      Client.testFiles info == ["tests/Main.elm"]
        && maybe True (const False) (Client.testResults info)
        && maybe True (const False) (Client.testCompilation info)
    Nothing -> False

testMcpXmlBlockShape :: IO Bool
testMcpXmlBlockShape = do
  let block = MCP.xmlBlockForTests "docs" [("index", "0"), ("kind", "module"), ("status", "ok")] "body"
  pure (block == "<docs index=\"0\" kind=\"module\" status=\"ok\">\nbody\n</docs>")

testMcpXmlAttributeEscaping :: IO Bool
testMcpXmlAttributeEscaping = do
  let block = MCP.xmlBlockForTests "diagnostics" [("file", "A&B<\"C\">")] "body"
  pure (block == "<diagnostics file=\"A&amp;B&lt;&quot;C&quot;&gt;\">\nbody\n</diagnostics>")


testMcpTestJobCompletion :: IO Bool
testMcpTestJobCompletion = do
  registry <- TestJobs.newRegistry
  started <- MVar.newEmptyMVar
  release <- MVar.newEmptyMVar
  submitted <- TestJobs.submitWith registry "/tmp/project" $ \reportProgress -> do
    reportProgress "Running test configuration 1/1"
    MVar.putMVar started ()
    MVar.takeMVar release
    pure (TestJobs.ActionCompleted TestJobs.Completion
      { TestJobs.completionOutcome = TestJobs.TestsPassed
      , TestJobs.completionBody = "report"
      , TestJobs.completionPassed = 3
      , TestJobs.completionFailed = 0
      , TestJobs.completionTotal = 3
      })
  didStart <- Timeout.timeout 1000000 (MVar.takeMVar started)
  running <- TestJobs.lookupJobWith registry (TestJobs.jobId submitted)
  MVar.putMVar release ()
  completed <- TestJobs.waitForWith registry (TestJobs.jobId submitted) 1
  pure $ case (didStart, running, completed) of
    (Just (), Just runningJob, Just completedJob) ->
      TestJobs.jobStatus runningJob == TestJobs.JobRunning
        && TestJobs.jobMessage runningJob == "Running test configuration 1/1"
        && TestJobs.jobStatus completedJob == TestJobs.JobCompleted
        && case TestJobs.jobCompletion completedJob of
             Just completion ->
               TestJobs.completionPassed completion == 3
                 && TestJobs.completionBody completion == "report"
             Nothing -> False
    _ -> False


testMcpTestJobFailureSummary :: IO Bool
testMcpTestJobFailureSummary = do
  registry <- TestJobs.newRegistry
  submitted <- TestJobs.submitWith registry "/tmp/project" $ \_ ->
    pure (TestJobs.ActionFailed "<tests status=\"error\">\nTests timed out after 60000ms\n</tests>")
  completed <- TestJobs.waitForWith registry (TestJobs.jobId submitted) 1
  pure $ case completed of
    Just job ->
      TestJobs.jobStatus job == TestJobs.JobFailed
        && TestJobs.jobMessage job == "Tests timed out after 60000ms"
        && TestJobs.jobFailure job == Just "<tests status=\"error\">\nTests timed out after 60000ms\n</tests>"
    Nothing -> False


testMcpTestJobCancellation :: IO Bool
testMcpTestJobCancellation = do
  registry <- TestJobs.newRegistry
  started <- MVar.newEmptyMVar
  blocker <- MVar.newEmptyMVar
  cleaned <- MVar.newEmptyMVar
  submitted <- TestJobs.submitWith registry "/tmp/project" $ \_ -> do
    _ <- (MVar.putMVar started () >> MVar.takeMVar blocker)
      `Exception.finally` MVar.putMVar cleaned ()
    pure (TestJobs.ActionFailed "unexpected completion")
  didStart <- Timeout.timeout 1000000 (MVar.takeMVar started)
  cancelled <- TestJobs.cancelWith registry (TestJobs.jobId submitted)
  didClean <- Timeout.timeout 1000000 (MVar.takeMVar cleaned)
  final <- TestJobs.lookupJobWith registry (TestJobs.jobId submitted)
  pure $ case (didStart, cancelled, didClean, final) of
    (Just (), Just cancelledJob, Just (), Just finalJob) ->
      TestJobs.jobStatus cancelledJob == TestJobs.JobCancelled
        && TestJobs.jobStatus finalJob == TestJobs.JobCancelled
    _ -> False


testMcpQueuedTestJobCancellation :: IO Bool
testMcpQueuedTestJobCancellation = do
  registry <- TestJobs.newRegistry
  firstStarted <- MVar.newEmptyMVar
  releaseFirst <- MVar.newEmptyMVar
  secondStarted <- MVar.newEmptyMVar
  first <- TestJobs.submitWith registry "/tmp/project" $ \_ -> do
    MVar.putMVar firstStarted ()
    MVar.takeMVar releaseFirst
    pure (TestJobs.ActionFailed "first finished")
  didStart <- Timeout.timeout 1000000 (MVar.takeMVar firstStarted)
  second <- TestJobs.submitWith registry "/tmp/project" $ \_ -> do
    MVar.putMVar secondStarted ()
    pure (TestJobs.ActionFailed "second started")
  cancelled <- TestJobs.cancelWith registry (TestJobs.jobId second)
  MVar.putMVar releaseFirst ()
  _ <- TestJobs.waitForWith registry (TestJobs.jobId first) 1
  unexpectedlyStarted <- MVar.tryTakeMVar secondStarted
  final <- TestJobs.lookupJobWith registry (TestJobs.jobId second)
  pure $ case (didStart, cancelled, final) of
    (Just (), Just cancelledJob, Just finalJob) ->
      TestJobs.jobStatus cancelledJob == TestJobs.JobCancelled
        && TestJobs.jobStatus finalJob == TestJobs.JobCancelled
        && unexpectedlyStarted == Nothing
    _ -> False


testMcpTestJobActiveLimit :: IO Bool
testMcpTestJobActiveLimit = do
  registry <- TestJobs.newRegistry
  firstStarted <- MVar.newEmptyMVar
  releaseFirst <- MVar.newEmptyMVar
  secondStarted <- MVar.newEmptyMVar
  rejectedStarted <- MVar.newEmptyMVar
  first <- TestJobs.submitWith registry "/tmp/project" $ \_ -> do
    MVar.putMVar firstStarted ()
    MVar.takeMVar releaseFirst
    pure (TestJobs.ActionFailed "first finished")
  didStart <- Timeout.timeout 1000000 (MVar.takeMVar firstStarted)
  second <- TestJobs.submitWith registry "/tmp/project" $ \_ -> do
    MVar.putMVar secondStarted ()
    pure (TestJobs.ActionFailed "second finished")
  rejected <- TestJobs.submitWith registry "/tmp/project" $ \_ -> do
    MVar.putMVar rejectedStarted ()
    pure (TestJobs.ActionFailed "unexpected start")
  secondBeforeRelease <- TestJobs.lookupJobWith registry (TestJobs.jobId second)
  rejectedAfterSubmit <- TestJobs.lookupJobWith registry (TestJobs.jobId rejected)
  unexpectedlyStarted <- MVar.tryTakeMVar rejectedStarted
  _ <- TestJobs.cancelWith registry (TestJobs.jobId second)
  MVar.putMVar releaseFirst ()
  _ <- TestJobs.waitForWith registry (TestJobs.jobId first) 1
  pure $ case (didStart, secondBeforeRelease, rejectedAfterSubmit) of
    (Just (), Just queuedJob, Just rejectedJob) ->
      TestJobs.jobStatus queuedJob == TestJobs.JobQueued
        && TestJobs.jobStatus rejectedJob == TestJobs.JobFailed
        && TestJobs.jobFailure rejectedJob == Just (TestJobs.jobMessage rejectedJob)
        && unexpectedlyStarted == Nothing
    _ -> False


testMcpTestJobRetentionBound :: IO Bool
testMcpTestJobRetentionBound = do
  registry <- TestJobs.newRegistry
  mapM_
    (\_ -> do
      submitted <- TestJobs.submitWith registry "/tmp/project" $ \_ ->
        pure (TestJobs.ActionCompleted TestJobs.Completion
          { TestJobs.completionOutcome = TestJobs.TestsPassed
          , TestJobs.completionBody = "report"
          , TestJobs.completionPassed = 1
          , TestJobs.completionFailed = 0
          , TestJobs.completionTotal = 1
          })
      _ <- TestJobs.waitForWith registry (TestJobs.jobId submitted) 1
      pure ()
    )
    [(1 :: Int)..22]
  retained <- TestJobs.listJobsForRootWith registry "/tmp/project"
  let retainedIds = map TestJobs.jobId retained
  pure (length retained == 20 && "test-1" `notElem` retainedIds && "test-2" `notElem` retainedIds)


testMcpTestJobOutputBound :: IO Bool
testMcpTestJobOutputBound = do
  registry <- TestJobs.newRegistry
  let oversized = Text.replicate (300 * 1024) "x"
  submitted <- TestJobs.submitWith registry "/tmp/project" $ \_ ->
    pure (TestJobs.ActionCompleted TestJobs.Completion
      { TestJobs.completionOutcome = TestJobs.TestsFailed
      , TestJobs.completionBody = oversized
      , TestJobs.completionPassed = 0
      , TestJobs.completionFailed = 1
      , TestJobs.completionTotal = 1
      })
  completed <- TestJobs.waitForWith registry (TestJobs.jobId submitted) 1
  pure $ case completed >>= TestJobs.jobCompletion of
    Just completion ->
      Text.length (TestJobs.completionBody completion) < Text.length oversized
        && "[Output truncated by elm-dev" `Text.isInfixOf` TestJobs.completionBody completion
    Nothing -> False


testHoverIndexPreservesAnnotations :: IO Bool
testHoverIndexPreservesAnnotations = do
  let region1 = Annotation.Region (Annotation.Position 1 2) (Annotation.Position 3 4)
      region2 = Annotation.Region (Annotation.Position 5 6) (Annotation.Position 7 8)
      region3 = Annotation.Region (Annotation.Position 9 10) (Annotation.Position 11 12)
      unitAnnotation = Canonical.Forall Map.empty Canonical.TUnit
      functionAnnotation = Canonical.Forall Map.empty (Canonical.TLambda Canonical.TUnit Canonical.TUnit)
      index = Client.makeHoverIndex (Map.fromList
        [ (region1, unitAnnotation)
        , (region2, unitAnnotation)
        , (region3, functionAnnotation)
        ])
      first = Client.lookupHoverType region1 index
      second = Client.lookupHoverType region2 index
      third = Client.lookupHoverType region3 index
  pure
    ( first == Just unitAnnotation
        && second == Just unitAnnotation
        && third == Just functionAnnotation
    )


testAggregatorRewriteRefreshesCache :: IO Bool
testAggregatorRewriteRefreshesCache =
  Temp.withSystemTempDirectory "elm-dev-test-aggregator" $ \root -> do
    let first = [(Name.fromChars "Example.Tests", Name.fromChars "tests")]
        second = [(Name.fromChars "Example.Tests", Name.fromChars "newTest")]
        path = TestGenerate.generatedDir root FilePath.</> "Everything.elm"
    _ <- TestGenerate.writeAggregator root first
    _ <- FileCache.readUtf8 path
    _ <- TestGenerate.writeAggregator root second
    refreshed <- FileCache.readUtf8 path
    pure
      ( BS.isInfixOf "Example.Tests.newTest" refreshed
          && not (BS.isInfixOf "Example.Tests.tests" refreshed)
      )


testPackageTestManifestIsPersistedAsApplication :: IO Bool
testPackageTestManifestIsPersistedAsApplication =
  Temp.withSystemTempDirectory "elm-dev-package-test-manifest" $ \root -> do
    writeElmPackage root
    Dir.createDirectoryIfMissing True (root FilePath.</> "src")
    Dir.createDirectoryIfMissing True (root FilePath.</> "tests")
    result <- TestCompile.regenerateTestElmJsonPersisted root
    let testRoot = TestGenerate.generatedDir root
        testElmJsonPath = testRoot FilePath.</> "elm.json"
    exists <- Dir.doesFileExist testElmJsonPath
    generated <- Outline.read testRoot
    pure $ case (result, generated) of
      (Right (), Right (Outline.App _)) -> exists
      _ -> False


testSpecSourcesRemainTestOnly :: IO Bool
testSpecSourcesRemainTestOnly =
  Temp.withSystemTempDirectory "elm-dev-spec-sources" $ \root -> do
    writeElmPackage root
    mapM_ (Dir.createDirectoryIfMissing True . (root FilePath.</>))
      ["src", "tests", "spec-library", "elm-stuff/generated-tests"]
    original <- BS.readFile (root FilePath.</> "elm.json")
    BS.writeFile (root FilePath.</> "elm.spec.json") "{\"source-directories\":[\"spec-library\"]}"
    result <- TestCompile.regenerateTestElmJsonPersisted root
    generated <- BS.readFile (TestGenerate.generatedDir root FilePath.</> "elm.json")
    unchanged <- BS.readFile (root FilePath.</> "elm.json")
    pure $ case result of
      Right () -> original == unchanged
        && "spec-library" `BS.isInfixOf` generated
        && "../generated-tests" `BS.isInfixOf` generated
      _ -> False


testCanonicalSpecSourceDirectories :: IO Bool
testCanonicalSpecSourceDirectories =
  Temp.withSystemTempDirectory "elm-dev-canonical-test-sources" $ \root -> do
    let source = root FilePath.</> "elm-stuff/generated-tests"
    Dir.createDirectoryIfMissing True source
    Dir.createDirectoryIfMissing True (root FilePath.</> "elm-stuff/elm-dev-test")
    Dir.createDirectoryLink source (root FilePath.</> "linked-tests")
    paths <- SpecSupport.canonicalSources
      [source, root FilePath.</> "elm-stuff/elm-dev-test/../generated-tests", root FilePath.</> "linked-tests"]
    expected <- Dir.canonicalizePath source
    Dir.createDirectoryIfMissing True (root FilePath.</> "src")
    Dir.createDirectoryIfMissing True (root FilePath.</> "tests")
    let manifest = TextEncoding.encodeUtf8 (Text.replace
          "\"source-directories\": [\"src\"]"
          "\"source-directories\": [\"src\", \"linked-tests\"]"
          (Text.pack basicElmJson))
    BS.writeFile (root FilePath.</> "elm.json") manifest
    result <- TestCompile.regenerateTestElmJsonPersisted root
    outline <- case result of
      Left _ -> pure Nothing
      Right () -> Just <$> Outline.read (TestGenerate.generatedDir root)
    after <- BS.readFile (root FilePath.</> "elm.json")
    case (result, outline) of
      (Right (), Just (Right (Outline.App app))) -> do
        let generatedRoot = TestGenerate.generatedDir root
            emitted = map (Outline.toAbsolute generatedRoot) (NE.toList (Outline._app_source_dirs app))
        identities <- traverse Dir.canonicalizePath emitted
        pure (paths == [expected] && length identities == length (List.nub identities)
          && length (filter (== expected) identities) == 1 && manifest == after)
      _ -> pure False


testSpecReadinessIsReadOnly :: IO Bool
testSpecReadinessIsReadOnly =
  Temp.withSystemTempDirectory "elm-dev-readonly-spec-preflight" $ \root -> do
    writeElmApp root
    Dir.createDirectoryIfMissing True (root FilePath.</> "src")
    Dir.createDirectoryIfMissing True (root FilePath.</> "tests")
    let target = root FilePath.</> "tests/Smoke.elm"
    BS.writeFile target "module Smoke exposing (tests)\nimport App.Spec\ntests = App.Spec.start\n"
    original <- BS.readFile (root FilePath.</> "elm.json")
    result <- SpecSupport.checkEntrypoints root [target]
    after <- BS.readFile (root FilePath.</> "elm.json")
    configured <- Dir.doesFileExist (root FilePath.</> "elm.spec.json")
    pure $ case result of
      Left problem -> "--install" `List.isInfixOf` problem && original == after && not configured
      Right () -> False


testMcpTestContextDocs :: IO Bool
testMcpTestContextDocs =
  Temp.withSystemTempDirectory "elm-dev-mcp-test-docs" $ \root -> do
    writeElmApp root
    Dir.createDirectoryIfMissing True (root FilePath.</> "src")
    Dir.createDirectoryIfMissing True (root FilePath.</> "tests")
    BS.writeFile (root FilePath.</> "src/Main.elm") "module Main exposing (main)\nimport Platform\nmain = Platform.worker { init = \\_ -> ( (), Cmd.none ), update = \\_ model -> ( model, Cmd.none ), subscriptions = \\_ -> Sub.none }\n"
    BS.writeFile (root FilePath.</> "tests/TestHelper.elm") "module TestHelper exposing (answer)\nanswer : Int\nanswer = 42\n"
    BS.writeFile (root FilePath.</> "tests/BrokenHelper.elm") "module BrokenHelper exposing (answer)\nanswer : Int\nanswer = \"wrong type\"\n"
    state <- Client.initState testUrls
    original <- BS.readFile (root FilePath.</> "elm.json")
    case List.find ((== "docs") . MCPProtocol.toolName) MCP.availableToolsForTests of
      Nothing -> pure False
      Just tool -> do
        response <- MCPProtocol.call tool (KeyMap.fromList
          [ ("dir", JSON.String (Text.pack root))
          , ("queries", JSON.toJSON
              [ JSON.object ["kind" JSON..= ("module" :: Text.Text), "module" JSON..= ("TestHelper" :: Text.Text)]
              , JSON.object ["kind" JSON..= ("value" :: Text.Text), "value" JSON..= ("TestHelper.answer" :: Text.Text)]
              , JSON.object ["kind" JSON..= ("file" :: Text.Text), "file" JSON..= (root FilePath.</> "tests/TestHelper.elm")]
              , JSON.object ["kind" JSON..= ("module" :: Text.Text), "module" JSON..= ("BrokenHelper" :: Text.Text)]
              ])
          ]) state (\_ -> pure ()) "test-docs-context"
        after <- BS.readFile (root FilePath.</> "elm.json")
        pure $ case response of
          MCPProtocol.ToolCallResponse [MCPProtocol.ToolResponseText _ body] -> original == after
            && Text.count "status=\"ok\"" body == 3
            && "answer" `Text.isInfixOf` body
            && "TYPE MISMATCH" `Text.isInfixOf` body
            && not ("No module found named BrokenHelper" `Text.isInfixOf` body)
          _ -> False


testConsumerSpecDocs :: IO Bool
testConsumerSpecDocs = do
  consumer <- Environment.lookupEnv "ELM_DEV_SPEC_CONSUMER"
  case consumer of
    Nothing -> pure True
    Just root -> do
      state <- Client.initState testUrls
      before <- BS.readFile (root FilePath.</> "elm.json")
      setupWorked <- case List.find ((== "add") . MCPProtocol.toolName) MCP.availableToolsForTests of
        Nothing -> pure False
        Just tool -> do
          result <- MCPProtocol.call tool (KeyMap.fromList
            [ ("dir", JSON.String (Text.pack root))
            , ("additions", JSON.toJSON [JSON.object ["kind" JSON..= ("spec" :: Text.Text)]])
            ]) state (\_ -> pure ()) "consumer-spec-setup"
          pure $ case result of
            MCPProtocol.ToolCallResponse [MCPProtocol.ToolResponseText _ body] -> "status=\"ok\"" `Text.isInfixOf` body
              && "Application elm.json unchanged" `Text.isInfixOf` body
            _ -> False
      case List.find ((== "docs") . MCPProtocol.toolName) MCP.availableToolsForTests of
        Nothing -> pure False
        Just tool -> do
          response <- MCPProtocol.call tool (KeyMap.fromList
            [ ("dir", JSON.String (Text.pack root))
            , ("queries", JSON.toJSON
                [ JSON.object ["kind" JSON..= ("module" :: Text.Text), "module" JSON..= ("App.Spec" :: Text.Text)]
                , JSON.object ["kind" JSON..= ("module" :: Text.Text), "module" JSON..= ("Spec" :: Text.Text)]
                , JSON.object ["kind" JSON..= ("value" :: Text.Text), "value" JSON..= ("Spec.Program.start" :: Text.Text)]
                , JSON.object ["kind" JSON..= ("module" :: Text.Text), "module" JSON..= ("ProgramTest" :: Text.Text)]
                , JSON.object ["kind" JSON..= ("file" :: Text.Text), "file" JSON..= (root FilePath.</> "tests/AppSpecHarnessTest.elm")]
                ])
            ]) state (\_ -> pure ()) "consumer-spec-docs"
          after <- BS.readFile (root FilePath.</> "elm.json")
          pure $ case response of
            MCPProtocol.ToolCallResponse [MCPProtocol.ToolResponseText _ body] -> setupWorked && before == after && Text.count "status=\"ok\"" body == 5
            _ -> False


testConsumerSpecWorker :: IO Bool
testConsumerSpecWorker = do
  consumer <- Environment.lookupEnv "ELM_DEV_SPEC_CONSUMER"
  binary <- Environment.lookupEnv "ELM_DEV_SPEC_BIN"
  case (consumer, binary) of
    (Just root, Just executable) -> do
      result <- TestRunner.runIsolatedWith executable (Just 60) (Just ["LoreBehaviorSpec.login"]) (Just 12345) (Just 100) root
      case result of
        Right success -> do
          let passed = TestRunner.summaryTotal (TestRunner.isolatedSummary success) == 2
                && TestRunner.summaryFailed (TestRunner.isolatedSummary success) == 0
          unless passed (putStrLn (TestRunner.isolatedRendered success))
          pure passed
        Left (TestRunner.RunFailed problem) -> putStrLn problem >> pure False
        Left (TestRunner.TimedOut seconds) -> putStrLn ("Consumer spec worker timed out after " ++ show seconds ++ " seconds") >> pure False
    _ -> pure True


testMissingSpecSourceReportsPath :: IO Bool
testMissingSpecSourceReportsPath =
  Temp.withSystemTempDirectory "elm-dev-missing-spec-source" $ \root -> do
    writeElmPackage root
    BS.writeFile (root FilePath.</> "elm.spec.json") "{\"source-directories\":[\"missing-spec-library\"]}"
    result <- Exception.try (TestCompile.regenerateTestElmJsonPersisted root)
    pure $ case result of
      Left problem -> "missing-spec-library" `List.isInfixOf` show (problem :: IOError)
      Right _ -> False


testDiscoveryPreservesExposedDeclarationCasing :: IO Bool
testDiscoveryPreservesExposedDeclarationCasing = do
  let source = BS.intercalate "\n"
        [ "module AppSpatialInteractionTest exposing (tests, generatedSrdFireballNaturalTest, generatedSrdDiscoveryProbe)"
        , ""
        , "tests = ()"
        , "generatedSrdFireballNaturalTest = ()"
        , "generatedSrdDiscoveryProbe = ()"
        ]
      expected =
        [ "AppSpatialInteractionTest.tests"
        , "AppSpatialInteractionTest.generatedSrdFireballNaturalTest"
        , "AppSpatialInteractionTest.generatedSrdDiscoveryProbe"
        ]
  pure $ case TestParse.parseModule "AppSpatialInteractionTest.elm" source of
    Left _ -> False
    Right parsed ->
      let discovered =
            map
              (\(moduleName, declaration) -> ModuleName.toChars moduleName ++ "." ++ Name.toChars declaration)
              (TestParse.potentialTestValues parsed)
      in discovered == expected
          && all (\name -> TestRunner.matchesAnyGlob [name] name) expected


testReportDuration :: IO Bool
testReportDuration = do
  let encoded = "[{\"id\":\"Suite.slow\",\"runs\":[],\"isOnly\":false,\"durationMs\":123}]"
      legacy = "[{\"id\":\"Suite.legacy\",\"runs\":[],\"isOnly\":false}]"
  pure $ case (TestResult.decodeReportsString encoded, TestResult.decodeReportsString legacy) of
    (Right [report], Right [legacyReport]) ->
      TestResult.reportDurationMs report == Just 123
        && TestResult.reportDurationMs legacyReport == Nothing
        && "123ms  Suite.slow" `List.isInfixOf` TestReport.renderReportsWithDuration False Nothing Nothing Nothing [report]
    _ -> False

testO2FunctionHelpers :: IO Bool
testO2FunctionHelpers = do
  let js = renderBuilder (JsFunctions.functions (Mode.Prod Optimization.O2 Map.empty Map.empty Map.empty Map.empty))
  pure
    ( List.isInfixOf "curried.a2 = fun;" js
        && List.isInfixOf "return fun.a2 ? fun.a2(a, b) : fun(a)(b);" js
        && List.isInfixOf "curried.a9 = fun;" js
        && List.isInfixOf "return fun.a9 ? fun.a9(a, b, c, d, e, f, g, h, i)" js
        && not (List.isInfixOf "wrapper.a = arity;" js)
    )

testO0FunctionHelpers :: IO Bool
testO0FunctionHelpers = do
  let js = renderBuilder (JsFunctions.functions (Mode.Prod Optimization.O0 Map.empty Map.empty Map.empty Map.empty))
  pure
    ( List.isInfixOf "wrapper.a = arity;" js
        && List.isInfixOf "wrapper.f = fun;" js
        && List.isInfixOf "return fun.a === 2 ? fun.f(a, b) : fun(a)(b);" js
        && not (List.isInfixOf "curried.a2 = fun;" js)
    )

testO2FunctionHelpersRuntime :: IO Bool
testO2FunctionHelpersRuntime = do
  let helpers = renderBuilder (JsFunctions.functions (Mode.Prod Optimization.O2 Map.empty Map.empty Map.empty Map.empty))
      script =
        helpers ++ unlines
          [ "var add = F2(function(a, b) { return a + b; });"
          , "var sum3 = F3(function(a, b, c) { return a + b + c; });"
          , "if (A2(add, 1, 2) !== 3) process.exit(1);"
          , "if (add(1)(2) !== 3) process.exit(2);"
          , "if (A3(sum3, 1, 2, 3) !== 6) process.exit(3);"
          , "if (sum3(1)(2)(3) !== 6) process.exit(4);"
          ]
  (exitCode, _, _) <- Process.readProcessWithExitCode "node" ["-e", script] ""
  pure (exitCode == Exit.ExitSuccess)

testMakeOptimizationModes :: IO Bool
testMakeOptimizationModes = do
  dev <- Task.run (Make.getMode False False False False)
  optimize <- Task.run (Make.getMode False True False False)
  o2 <- Task.run (Make.getMode False False True False)
  o3 <- Task.run (Make.getMode False False False True)
  debugO2 <- Task.run (Make.getMode True False True False)
  o2o3 <- Task.run (Make.getMode False False True True)
  pure
    ( isRightMode Make.Dev dev
        && isRightMode (Make.Prod Optimization.O0) optimize
        && isRightMode (Make.Prod Optimization.O2) o2
        && isRightMode (Make.Prod Optimization.O3) o3
        && isOptimizeDebugError debugO2
        && isOptimizationLevelError o2o3
    )

isRightMode :: Make.DesiredMode -> Either ReportingExit.Make Make.DesiredMode -> Bool
isRightMode expected result =
  case result of
    Right actual -> actual == expected
    Left _ -> False

isOptimizeDebugError :: Either ReportingExit.Make Make.DesiredMode -> Bool
isOptimizeDebugError result =
  case result of
    Left ReportingExit.MakeCannotOptimizeAndDebug -> True
    _ -> False

isOptimizationLevelError :: Either ReportingExit.Make Make.DesiredMode -> Bool
isOptimizationLevelError result =
  case result of
    Left ReportingExit.MakeCannotCombineOptimizationLevels -> True
    _ -> False

testMakeO2FlagParsing :: IO Bool
testMakeO2FlagParsing = do
  let (_, result) = Chomp.chomp Nothing ["-O2"] Terminal.noArgs testMakeFlags
  pure $ case result of
    Right ((), Make.Flags False False True False Nothing Nothing Nothing) -> True
    _ -> False

testMakeFlags :: Terminal.Flags Make.Flags
testMakeFlags =
  Terminal.flags Make.Flags
    Terminal.|-- Terminal.onOff "debug" "debug"
    Terminal.|-- Terminal.onOff "optimize" "optimize"
    Terminal.|-- Terminal.onOff "O2" "O2"
    Terminal.|-- Terminal.onOff "O3" "O3"
    Terminal.|-- Terminal.flag "output" Make.output "output"
    Terminal.|-- Terminal.flag "report" Make.reportType "report"
    Terminal.|-- Terminal.flag "docs" Make.docsFile "docs"

testCompiledOptimizationRuntime :: IO Bool
testCompiledOptimizationRuntime = do
  root <- uniqueRoot
  let srcDir = root FilePath.</> "src"
      mainPath = srcDir FilePath.</> "Main.elm"
  writeElmApp root
  Dir.createDirectoryIfMissing True srcDir
  writeFile mainPath optimizationRuntimeElm

  o0 <- compileOptimizationApp root (CompileHelpers.Prod Optimization.O0)
  o2 <- compileOptimizationApp root (CompileHelpers.Prod Optimization.O2)
  o3 <- compileOptimizationApp root (CompileHelpers.Prod Optimization.O3)

  case (o0, o2, o3) of
    (Right jsO0, Right jsO2, Right jsO3) -> do
      runO0 <- runCompiledWorker root "o0" jsO0
      runO2 <- runCompiledWorker root "o2" jsO2
      runO3 <- runCompiledWorker root "o3" jsO3
      pure
        ( runO0 == Just "117"
            && runO2 == Just "117"
            && runO3 == Just "117"
            && List.isInfixOf "wrapper.a = arity;" jsO0
            && not (List.isInfixOf "curried.a2 = fun;" jsO0)
            && List.isInfixOf "curried.a2 = fun;" jsO2
            && List.isInfixOf "curried.a2 = fun;" jsO3
            && not (List.isInfixOf "add_fn" jsO0)
            && List.isInfixOf "add_fn" jsO2
            && List.isInfixOf "sum3_fn" jsO2
            && List.isInfixOf "add_fn(" jsO2
            && List.isInfixOf "sum3_fn(" jsO2
            && List.isInfixOf "apply2_unwrapped" jsO2
            && List.isInfixOf "$elm$core$List$map = F2(" jsO2
            && List.isInfixOf "$elm$core$List$filter = F2(" jsO2
            && List.isInfixOf "$elm$core$List$concatMap = F2(" jsO2
            && List.isInfixOf "$elm$core$List$partition = F2(" jsO2
            && List.isInfixOf "expected === (1 + 2)" jsO2
            && List.isInfixOf "add_fn" jsO3
            && List.isInfixOf "sum3_fn" jsO3
            && List.isInfixOf "add_fn(" jsO3
            && List.isInfixOf "sum3_fn(" jsO3
            && List.isInfixOf "apply2_unwrapped" jsO3
            && List.isInfixOf "$elm$core$List$map = F2(" jsO3
            && List.isInfixOf "$elm$core$List$filter = F2(" jsO3
            && List.isInfixOf "$elm$core$List$concatMap = F2(" jsO3
            && List.isInfixOf "$elm$core$List$partition = F2(" jsO3
            && List.isInfixOf "expected === (1 + 2)" jsO3
        )

    _ ->
      pure False

compileOptimizationApp :: FilePath -> CompileHelpers.DesiredMode -> IO (Either String String)
compileOptimizationApp root desiredMode = do
  result <- DiskCompile.compile
    root
    (NE.List ("src" FilePath.</> "Main.elm") [])
    (CompileHelpers.Flags desiredMode (CompileHelpers.OutputTo CompileHelpers.Js) CompileHelpers.DebuggerNone)
  pure $ case result of
    Right (CompileHelpers.CompiledJs builder) ->
      Right (renderBuilder builder)

    Right _ ->
      Left "expected compiled JS"

    Left err ->
      Left (show err)

runCompiledWorker :: FilePath -> String -> String -> IO (Maybe String)
runCompiledWorker root label js = do
  let jsPath = root FilePath.</> (label ++ ".js")
      runnerPath = root FilePath.</> (label ++ "-runner.js")
  writeFile jsPath js
  writeFile runnerPath $ unlines
    [ "const elm = require('./" ++ label ++ ".js');"
    , "const app = elm.Elm.Main.init({ flags: null });"
    , "app.ports.output.subscribe(value => { console.log(value); process.exit(0); });"
    , "setTimeout(() => process.exit(2), 1000);"
    ]
  (exitCode, stdout, _) <- Process.readProcessWithExitCode "node" [runnerPath] ""
  pure $ case exitCode of
    Exit.ExitSuccess -> Just (trim stdout)
    _ -> Nothing

runCompiledSuite :: FilePath -> String -> String -> IO (Maybe String)
runCompiledSuite root label js = do
  let jsPath = root FilePath.</> (label ++ ".js")
      runnerPath = root FilePath.</> (label ++ "-runner.js")
  writeFile jsPath js
  writeFile runnerPath $ unlines
    [ "const elm = require('./" ++ label ++ ".js');"
    , "const app = elm.Elm.Main.init({ flags: {} });"
    , "app.ports.onSuccessSend.subscribe(value => { console.log(value); process.exit(0); });"
    , "app.ports.onFailureSend.subscribe(value => { console.error(value); process.exit(1); });"
    , "setTimeout(() => process.exit(2), 1000);"
    ]
  (exitCode, stdout, stderr) <- Process.readProcessWithExitCode "node" [runnerPath] ""
  pure $ case exitCode of
    Exit.ExitSuccess -> Just (trim stdout)
    _ -> Just ("FAIL: " ++ trim stderr)

testPortedReplacementSuite :: IO Bool
testPortedReplacementSuite = do
  root <- uniqueRoot
  let srcDir = root FilePath.</> "src"
      mainPath = srcDir FilePath.</> "Main.elm"
  writeElmApp root
  Dir.createDirectoryIfMissing True srcDir
  writeFile mainPath portedReplacementSuiteElm

  o0 <- compileOptimizationApp root (CompileHelpers.Prod Optimization.O0)
  o2 <- compileOptimizationApp root (CompileHelpers.Prod Optimization.O2)
  o3 <- compileOptimizationApp root (CompileHelpers.Prod Optimization.O3)

  case (o0, o2, o3) of
    (Right jsO0, Right jsO2, Right jsO3) -> do
      runO0 <- runCompiledSuite root "ported-o0" jsO0
      runO2 <- runCompiledSuite root "ported-o2" jsO2
      runO3 <- runCompiledSuite root "ported-o3" jsO3
      pure
        ( runO0 == Just "Pass!"
            && runO2 == Just "Pass!"
            && runO3 == Just "Pass!"
            && not (List.isInfixOf "var result = '';" jsO0)
            && List.isInfixOf "var result = '';" jsO2
            && List.isInfixOf "chunk = chunk + chunk;" jsO2
            && List.isInfixOf "var acc = '' + strs.a;" jsO2
            && List.isInfixOf "while (strs.b)" jsO2
            && List.isInfixOf "var result = '';" jsO3
            && List.isInfixOf "chunk = chunk + chunk;" jsO3
            && List.isInfixOf "var acc = '' + strs.a;" jsO3
            && List.isInfixOf "while (strs.b)" jsO3
        )

    _ ->
      pure False

testOptimizationEdgeCases :: IO Bool
testOptimizationEdgeCases = do
  root <- uniqueRoot
  let srcDir = root FilePath.</> "src"
      mainPath = srcDir FilePath.</> "Main.elm"
  writeElmApp root
  Dir.createDirectoryIfMissing True srcDir
  writeFile mainPath optimizationEdgeCasesElm

  o0 <- compileOptimizationApp root (CompileHelpers.Prod Optimization.O0)
  o2 <- compileOptimizationApp root (CompileHelpers.Prod Optimization.O2)
  o3 <- compileOptimizationApp root (CompileHelpers.Prod Optimization.O3)

  case (o0, o2, o3) of
    (Right jsO0, Right jsO2, Right jsO3) -> do
      runO0 <- runCompiledSuite root "edge-o0" jsO0
      runO2 <- runCompiledSuite root "edge-o2" jsO2
      runO3 <- runCompiledSuite root "edge-o3" jsO3
      let passed =
            runO0 == Just "Pass!"
            && runO2 == Just "Pass!"
            && runO3 == Just "Pass!"
            && not (List.isInfixOf "apply2_unwrapped" jsO0)
            && List.isInfixOf "$author$project$Main$apply2_unwrapped($author$project$Main$add_fn, 6, 7)" jsO2
            && List.isInfixOf "$author$project$Main$apply2Record_unwrapped($author$project$Main$add_fn, 8, 9)" jsO2
            && List.isInfixOf "$author$project$Main$apply2Tuple_unwrapped($author$project$Main$add_fn, 10, 11)" jsO2
            && List.isInfixOf "n: fn(a, b)" jsO2
            && List.isInfixOf "$author$project$Main$apply2_fn($author$project$Main$sum3, 1, 2)(3)" jsO2
            && List.isInfixOf "$author$project$Main$apply3_unwrapped($author$project$Main$sum3_fn, 4, 5, 6)" jsO2
            && List.isInfixOf "$author$project$Main$sum9_fn(1, 2, 3, 4, 5, 6, 7, 8, 9)" jsO2
            && List.isInfixOf "$author$project$Main$add_fn(1, 2) === (1 + 2)" jsO2
            && List.isInfixOf "$author$project$Main$apply2_unwrapped($author$project$Main$add_fn, 6, 7)" jsO3
            && List.isInfixOf "$author$project$Main$apply2Record_unwrapped($author$project$Main$add_fn, 8, 9)" jsO3
            && List.isInfixOf "$author$project$Main$apply2Tuple_unwrapped($author$project$Main$add_fn, 10, 11)" jsO3
            && List.isInfixOf "$author$project$Main$apply2_fn($author$project$Main$sum3, 1, 2)(3)" jsO3
            && List.isInfixOf "$author$project$Main$apply3_unwrapped($author$project$Main$sum3_fn, 4, 5, 6)" jsO3
            && List.isInfixOf "$author$project$Main$sum9_fn(1, 2, 3, 4, 5, 6, 7, 8, 9)" jsO3
            && List.isInfixOf "$author$project$Main$add_fn(1, 2) === (1 + 2)" jsO3
            && not (List.isInfixOf "function $$Record" jsO2)
            && List.isInfixOf "function $$Record" jsO3
            && List.isInfixOf ".$c = function" jsO3
            && List.isInfixOf "$old.$c()" jsO3
      pure passed

    _ ->
      pure False

compileAndRunSuite :: String -> IO (Maybe (String, String, String))
compileAndRunSuite elmSource = do
  root <- uniqueRoot
  let srcDir = root FilePath.</> "src"
      mainPath = srcDir FilePath.</> "Main.elm"
  writeElmApp root
  Dir.createDirectoryIfMissing True srcDir
  writeFile mainPath elmSource

  o0 <- compileOptimizationApp root (CompileHelpers.Prod Optimization.O0)
  o2 <- compileOptimizationApp root (CompileHelpers.Prod Optimization.O2)
  o3 <- compileOptimizationApp root (CompileHelpers.Prod Optimization.O3)

  case (o0, o2, o3) of
    (Right jsO0, Right jsO2, Right jsO3) -> do
      runO0 <- runCompiledSuite root "suite-o0" jsO0
      runO2 <- runCompiledSuite root "suite-o2" jsO2
      runO3 <- runCompiledSuite root "suite-o3" jsO3
      pure $ case (runO0, runO2, runO3) of
        (Just a, Just b, Just c) -> Just (a, b, c)
        _ -> Nothing

    _ ->
      pure Nothing

trim :: String -> String
trim = reverse . dropWhile isLineBreak . reverse . dropWhile isLineBreak

isLineBreak :: Char -> Bool
isLineBreak char =
  char == '\n' || char == '\r'

optimizationRuntimeElm :: String
optimizationRuntimeElm =
  unlines
    [ "port module Main exposing (main)"
    , ""
    , "import Platform"
    , "import Platform.Cmd as Cmd"
    , "import Platform.Sub as Sub"
    , "import String"
    , ""
    , "port output : String -> Cmd msg"
    , ""
    , "add : Int -> Int -> Int"
    , "add a b ="
    , "    a + b"
    , ""
    , "sum3 : Int -> Int -> Int -> Int"
    , "sum3 a b c ="
    , "    a + b + c"
    , ""
    , "applyTwice : (Int -> Int) -> Int -> Int"
    , "applyTwice fn value ="
    , "    fn (fn value)"
    , ""
    , "apply2 : (Int -> Int -> Int) -> Int -> Int -> Int"
     , "apply2 fn a b ="
     , "    fn a b"
     , ""
     , "primitiveEquality : Int"
     , "primitiveEquality ="
     , "    let"
     , "        expected ="
     , "            add 1 2"
     , "    in"
     , "    if expected == 1 + 2 then"
     , "        5"
     , ""
     , "    else"
     , "        0"
     , ""
     , "listResult : Int"
     , "listResult ="
     , "    let"
     , "        ( lessThanThree, atLeastThree ) ="
     , "            List.partition (\\n -> n < 3) [ 1, 2, 3 ]"
     , ""
     , "        ( unzipLeft, unzipRight ) ="
     , "            List.unzip [ ( 1, 2 ), ( 3, 4 ) ]"
     , "    in"
     , "    List.sum (List.map ((+) 1) (List.filter (\\n -> n < 4) [ 1, 2, 3, 4 ]))"
     , "        + (if List.all (\\n -> n < 5) [ 1, 2, 3 ] then 1 else 0)"
     , "        + List.length (List.concat [ [ 1 ], [ 2, 3 ] ])"
     , "        + List.length (List.concatMap (\\n -> [ n, n ]) [ 1, 2 ])"
     , "        + List.length (List.intersperse 0 [ 1, 2, 3 ])"
     , "        + List.length lessThanThree * 10"
     , "        + List.length atLeastThree"
     , "        + List.length (List.take 2 [ 1, 2, 3 ])"
     , "        + List.sum unzipLeft"
     , "        + List.sum unzipRight"
     , "        + List.sum (List.indexedMap (+) [ 5, 6 ])"
     , "        + List.length ([ 1, 2 ] ++ [ 3 ])"
     , ""
     , "result : String"
     , "result ="
     , "    String.fromInt (add 1 2 + sum3 3 4 5 + applyTwice ((+) 2) 10 + apply2 add 6 7 + listResult + primitiveEquality)"
    , ""
    , "main : Program () () msg"
    , "main ="
    , "    Platform.worker"
    , "        { init = \\_ -> ( (), output result )"
    , "        , update = \\_ model -> ( model, Cmd.none )"
    , "        , subscriptions = \\_ -> Sub.none"
     , "        }"
     ]

portedReplacementSuiteElm :: String
portedReplacementSuiteElm =
  unlines
    [ "port module Main exposing (main)"
    , ""
    , "port onSuccessSend : String -> Cmd msg"
    , "port onFailureSend : String -> Cmd msg"
    , ""
    , "main : Program {} () ()"
    , "main ="
    , "    Platform.worker"
    , "        { init = \\_ ->"
    , "            ( ()"
    , "            , case run suite of"
    , "                [] ->"
    , "                    onSuccessSend \"Pass!\""
    , ""
    , "                errors ->"
    , "                    onFailureSend (String.join \", \" errors)"
    , "            )"
    , "        , update = \\_ model -> ( model, Cmd.none )"
    , "        , subscriptions = \\_ -> Sub.none"
    , "        }"
    , ""
    , "type Test"
    , "    = Describe String (List Test)"
    , "    | Test String (() -> Bool)"
    , ""
    , "run : Test -> List String"
    , "run testcase ="
    , "    case testcase of"
    , "        Test name toResult ->"
    , "            if toResult () then [] else [ name ]"
    , ""
    , "        Describe name tests ->"
    , "            List.foldl (\\t results -> run t ++ results) [] tests"
    , "                |> List.map (\\failure -> name ++ \" -> \" ++ failure)"
    , ""
    , "test ="
    , "    Test"
    , ""
    , "describe ="
    , "    Describe"
    , ""
    , "suite : Test"
    , "suite ="
    , "    describe \"Tests for js code replacements\""
    , "        [ list"
    , "        , string"
    , "        ]"
    , ""
    , "list : Test"
    , "list ="
    , "    describe \"List\""
    , "        [ test \"List.all\" <| \\_ -> True == List.all (\\i -> i == 1) [ 1, 1, 1, 1 ]"
    , "        , test \"List.all empty\" <| \\_ -> True == List.all (\\_ -> False) []"
    , "        , test \"List.all false\" <| \\_ -> False == List.all (\\i -> i == 1) [ 1, 2, 1 ]"
    , "        , test \"List.append\" <| \\_ -> [ 1, 2, 3, 4, 5, 6 ] == List.append [ 1, 2, 3 ] [ 4, 5, 6 ]"
    , "        , test \"List.append left empty\" <| \\_ -> [ 4, 5, 6 ] == List.append [] [ 4, 5, 6 ]"
    , "        , test \"List.append right empty\" <| \\_ -> [ 1, 2, 3 ] == List.append [ 1, 2, 3 ] []"
    , "        , test \"List.concat\" <| \\_ -> [ 1, 2, 3, 4, 5, 6 ] == List.concat [ [ 1, 2, 3 ], [ 4, 5, 6 ] ]"
    , "        , test \"List.concat empty\" <| \\_ -> [] == List.concat []"
    , "        , test \"List.concat internal empties\" <| \\_ -> [ 1, 2, 3 ] == List.concat [ [], [ 1 ], [], [ 2, 3 ], [] ]"
    , "        , test \"List.concatMap\" <| \\_ -> [ 1, 2, 3, 4, 5, 6 ] == List.concatMap identity [ [ 1, 2, 3 ], [ 4, 5, 6 ] ]"
    , "        , test \"List.concatMap empty input\" <| \\_ -> [] == List.concatMap (\\n -> [ n, n ]) []"
    , "        , test \"List.concatMap empty outputs\" <| \\_ -> [] == List.concatMap (\\_ -> []) [ 1, 2, 3 ]"
    , "        , test \"List.filter\" <| \\_ -> [ 1, 2, 3, 4, 5, 6 ] == List.filter (\\_ -> True) [ 1, 2, 3, 4, 5, 6 ]"
    , "        , test \"List.filter none\" <| \\_ -> [] == List.filter (\\_ -> False) [ 1, 2, 3 ]"
    , "        , test \"List.filter only even\" <| \\_ -> [ 2, 4, 6 ] == List.filter (\\i -> modBy 2 i == 0) [ 1, 2, 3, 4, 5, 6 ]"
    , "        , test \"List.indexedMap\" <| \\_ -> [ 0, 1, 2, 3, 4, 5 ] == List.indexedMap (\\i _ -> i) [ 1, 2, 3, 4, 5, 6 ]"
    , "        , test \"List.indexedMap empty\" <| \\_ -> [] == List.indexedMap (\\i _ -> i) []"
    , "        , test \"List.intersperse\" <| \\_ -> [ 1, 10, 2, 10, 3, 10, 4, 10, 5, 10, 6 ] == List.intersperse 10 [ 1, 2, 3, 4, 5, 6 ]"
    , "        , test \"List.intersperse empty\" <| \\_ -> [] == List.intersperse 10 []"
    , "        , test \"List.intersperse singleton\" <| \\_ -> [ 1 ] == List.intersperse 10 [ 1 ]"
    , "        , test \"List.partition\" <| \\_ -> ( [ 2, 4, 6 ], [ 1, 3, 5 ] ) == List.partition (\\i -> modBy 2 i == 0) [ 1, 2, 3, 4, 5, 6 ]"
    , "        , test \"List.partition empty\" <| \\_ -> ( [], [] ) == List.partition (\\_ -> True) []"
    , "        , test \"List.take\" <| \\_ -> [ 1, 2 ] == List.take 2 [ 1, 2, 3, 4, 5, 6 ]"
    , "        , test \"List.take zero\" <| \\_ -> [] == List.take 0 [ 1, 2, 3 ]"
    , "        , test \"List.take negative\" <| \\_ -> [] == List.take -1 [ 1, 2, 3 ]"
    , "        , test \"List.take too many\" <| \\_ -> [ 1, 2, 3 ] == List.take 10 [ 1, 2, 3 ]"
    , "        , test \"List.unzip\" <| \\_ -> ( [ 1, 2 ], [ 3, 4 ] ) == List.unzip [ ( 1, 3 ), ( 2, 4 ) ]"
    , "        , test \"List.unzip empty\" <| \\_ -> ( [], [] ) == List.unzip []"
    , "        ]"
    , ""
    , "string : Test"
    , "string ="
    , "    describe \"String\""
    , "        [ test \"String.repeat 4\" <| \\_ -> \"nananana\" == String.repeat 4 \"na\""
    , "        , test \"String.repeat 10\" <| \\_ -> \"nananananananananana\" == String.repeat 10 \"na\""
    , "        , test \"String.repeat zero\" <| \\_ -> \"\" == String.repeat 0 \"na\""
    , "        , test \"String.repeat negative\" <| \\_ -> \"\" == String.repeat -1 \"na\""
    , "        , test \"String.join\" <| \\_ -> \"a,b,c\" == String.join \",\" [ \"a\", \"b\", \"c\" ]"
    , "        , test \"String.join empty\" <| \\_ -> \"\" == String.join \",\" []"
    , "        , test \"String.join singleton\" <| \\_ -> \"a\" == String.join \",\" [ \"a\" ]"
    , "        ]"
    ]

optimizationEdgeCasesElm :: String
optimizationEdgeCasesElm =
  unlines
    [ "port module Main exposing (main)"
    , ""
    , "port onSuccessSend : String -> Cmd msg"
    , "port onFailureSend : String -> Cmd msg"
    , ""
    , "main : Program {} () ()"
    , "main ="
    , "    Platform.worker"
    , "        { init = \\_ ->"
    , "            if allPassed then"
    , "                ( (), onSuccessSend \"Pass!\" )"
    , ""
    , "            else"
    , "                ( (), onFailureSend failureMessage )"
    , "        , update = \\_ model -> ( model, Cmd.none )"
    , "        , subscriptions = \\_ -> Sub.none"
    , "        }"
    , ""
    , "add : Int -> Int -> Int"
    , "add a b ="
    , "    a + b"
    , ""
    , "sum3 : Int -> Int -> Int -> Int"
    , "sum3 a b c ="
    , "    a + b + c"
    , ""
    , "sum4 : Int -> Int -> Int -> Int -> Int"
    , "sum4 a b c d ="
    , "    a + b + c + d"
    , ""
    , "sum9 : Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int"
    , "sum9 a b c d e f g h i ="
    , "    a + b + c + d + e + f + g + h + i"
    , ""
    , "sumDown : Int -> Int -> Int"
    , "sumDown n acc ="
    , "    if n <= 0 then"
    , "        acc"
    , ""
    , "    else"
    , "        sumDown (n - 1) (acc + n)"
    , ""
    , "apply2 : (Int -> Int -> value) -> Int -> Int -> value"
    , "apply2 fn a b ="
    , "    fn a b"
    , ""
    , "apply2Record : (Int -> Int -> Int) -> Int -> Int -> Int"
    , "apply2Record fn a b ="
    , "    { value = fn a b }.value"
    , ""
    , "apply2Tuple : (Int -> Int -> Int) -> Int -> Int -> Int"
    , "apply2Tuple fn a b ="
    , "    Tuple.first ( fn a b, 0 )"
    , ""
    , "apply3 : (Int -> Int -> Int -> Int) -> Int -> Int -> Int -> Int"
    , "apply3 fn a b c ="
    , "    fn a b c"
    , ""
    , "type alias Rec ="
    , "    { a : Int, b : Int, c : Int }"
    , ""
    , "recordUpdate : Int"
    , "recordUpdate ="
    , "    let"
    , "        r ="
    , "            { a = 1, b = 2, c = 3 }"
    , ""
    , "        updated ="
    , "            { r | b = 20 }"
    , "    in"
    , "    updated.a + updated.b + updated.c + r.b"
    , ""
    , "recordParamUpdate : Rec -> Int"
    , "recordParamUpdate rec ="
    , "    let"
    , "        updated ="
    , "            { rec | a = 10, c = 30 }"
    , "    in"
    , "    updated.a + updated.b + updated.c + rec.a"
    , ""
    , "type Shape"
    , "    = Empty"
    , "    | One Int"
    , "    | Two Int Int"
    , ""
    , "shapeValue : Shape -> Int"
    , "shapeValue shape ="
    , "    case shape of"
    , "        Empty ->"
    , "            0"
    , ""
    , "        One a ->"
    , "            a"
    , ""
    , "        Two a b ->"
    , "            a + b"
    , ""
    , "checks : List ( String, Bool )"
    , "checks ="
    , "    [ ( \"direct arity 2\", add 1 2 == 3 )"
    , "    , ( \"partial arity 2\", (add 10) 5 == 15 )"
    , "    , ( \"direct arity 3\", sum3 1 2 3 == 6 )"
    , "    , ( \"direct arity 4\", sum4 1 2 3 4 == 10 )"
    , "    , ( \"direct arity 9\", sum9 1 2 3 4 5 6 7 8 9 == 45 )"
    , "    , ( \"tail recursion\", sumDown 10 0 == 55 )"
    , "    , ( \"unwrapped matching arity\", apply2 add 6 7 == 13 )"
    , "    , ( \"unwrapped nested record\", apply2Record add 8 9 == 17 )"
    , "    , ( \"unwrapped nested tuple\", apply2Tuple add 10 11 == 21 )"
    , "    , ( \"unwrapped arity mismatch keeps partial application\", (apply2 sum3 1 2) 3 == 6 )"
    , "    , ( \"unwrapped arity 3\", apply3 sum3 4 5 6 == 15 )"
    , "    , ( \"list callback partial\", List.map (add 1) [ 1, 2, 3 ] == [ 2, 3, 4 ] )"
    , "    , ( \"list callback raw exact\", List.map (apply2 add 1) [ 1, 2, 3 ] == [ 2, 3, 4 ] )"
    , "    , ( \"numeric primitive equality\", add 1 2 == 1 + 2 )"
    , "    , ( \"string equality remains correct\", String.fromInt (add 1 2) == \"3\" )"
    , "    , ( \"record update\", recordUpdate == 26 )"
    , "    , ( \"record param update\", recordParamUpdate { a = 1, b = 2, c = 3 } == 43 )"
    , "    , ( \"variant cases\", List.map shapeValue [ Empty, One 5, Two 10 20 ] == [ 0, 5, 30 ] )"
    , "    ]"
    , ""
    , "allPassed : Bool"
    , "allPassed ="
    , "    List.all Tuple.second checks"
    , ""
    , "failureMessage : String"
    , "failureMessage ="
    , "    checks"
    , "        |> List.filter (Tuple.second >> not)"
    , "        |> List.map Tuple.first"
    , "        |> String.join \", \""
    ]

renderBuilder :: Builder.Builder -> String
renderBuilder = LazyChar8.unpack . Builder.toLazyByteString

mkRange :: (Int, Int) -> (Int, Int) -> FileCache.Range
mkRange (sl, sc) (el, ec) =
  FileCache.Range
    { FileCache.rangeStart = FileCache.Position sl sc
    , FileCache.rangeEnd = FileCache.Position el ec
    }

expectRightText :: Either String Text.Text -> Text.Text -> Bool
expectRightText result expected =
  case result of
    Right actual -> actual == expected
    Left _ -> False

testSingleLineEdit :: IO Bool
testSingleLineEdit = do
  let content = Text.pack "ab\ncd\n"
      range = mkRange (0, 1) (0, 2)
      result = FileCache.applyRangeEdit content range (Text.pack "X")
  pure (expectRightText result (Text.pack "aX\ncd\n"))

testMultiLineEdit :: IO Bool
testMultiLineEdit = do
  let content = Text.pack "abc\ndef\nghi\n"
      range = mkRange (0, 2) (1, 2)
      result = FileCache.applyRangeEdit content range (Text.pack "ZZ")
  pure (expectRightText result (Text.pack "abZZf\nghi\n"))

testEofInsertion :: IO Bool
testEofInsertion = do
  let content = Text.pack "a\nb"
      range = mkRange (2, 0) (2, 0)
      result = FileCache.applyRangeEdit content range (Text.pack "!")
  pure (expectRightText result (Text.pack "a\nb!"))

testInvalidEofInsertion :: IO Bool
testInvalidEofInsertion = do
  let content = Text.pack "a\nb"
      range = mkRange (2, 1) (2, 1)
      result = FileCache.applyRangeEdit content range (Text.pack "!")
  pure $ case result of
    Left _ -> True
    Right _ -> False

testCrLfStability :: IO Bool
testCrLfStability = do
  let content = Text.pack "a\r\nb\r\n"
      range = mkRange (1, 1) (1, 1)
      result = FileCache.applyRangeEdit content range (Text.pack "X")
  pure (expectRightText result (Text.pack "a\r\nbX\r\n"))

testRemoveDirClearsDescendants :: IO Bool
testRemoveDirClearsDescendants = do
  root <- uniqueRoot
  let doomedDir = root FilePath.</> "src"
      doomedA = doomedDir FilePath.</> "Main.elm"
      doomedB = doomedDir FilePath.</> "Nested" FilePath.</> "Widget.elm"
      survivor = root FilePath.</> "keep.txt"
  FileCache.insert doomedA (BS.pack [1])
  FileCache.insert doomedB (BS.pack [2])
  FileCache.insert survivor (BS.pack [3])
  FileCache.removeDir doomedDir
  doomedA' <- FileCache.lookup doomedA
  doomedB' <- FileCache.lookup doomedB
  survivor' <- FileCache.lookup survivor
  FileCache.delete survivor
  pure (doomedA' == Nothing && doomedB' == Nothing && survivor' /= Nothing)

uniqueRoot :: IO FilePath
uniqueRoot = do
  t <- POSIX.getPOSIXTime
  pure ("/tmp/elm-dev-tests-" ++ show (round (t * 1000000) :: Integer))

testVersionsInit :: IO Bool
testVersionsInit = do
  root <- uniqueRoot
  versions <- Versions.readVersions root
  pure (Versions.fsVersion versions == 0 && Versions.compileVersion versions == 0 && Versions.testCompileVersion versions == Nothing)

testVersionsBumpAndSet :: IO Bool
testVersionsBumpAndSet = do
  root <- uniqueRoot
  _ <- Versions.bumpFsVersion root
  _ <- Versions.bumpFsVersion root
  Versions.setCompileVersionTo root 1
  versions <- Versions.readVersions root
  pure (Versions.fsVersion versions == 2 && Versions.compileVersion versions == 1)

testWatchPathFilterRelevant :: IO Bool
testWatchPathFilterRelevant = do
  let ok1 = ProjectState.isRelevantWatchedPath "/tmp/app/src/Main.elm"
      ok2 = ProjectState.isRelevantWatchedPath "/tmp/app/elm.json"
      ok3 = ProjectState.isRelevantWatchedPath "/tmp/app/elm.dev.json"
  pure (ok1 && ok2 && ok3)

testWatchPathFilterElmStuff :: IO Bool
testWatchPathFilterElmStuff = do
  let path = "/tmp/app/elm-stuff/generated/Main.elm"
  pure (not (Filewatch.shouldTriggerPath path))

testWatchPathFilterBuildOutput :: IO Bool
testWatchPathFilterBuildOutput = do
  let outputs =
        [ "/tmp/app/dist/build-version.js"
        , "/tmp/app/dist/types.d.ts"
        , "/tmp/app/dist/types.js.map"
        ]
  pure (all (not . Filewatch.shouldTriggerPath) outputs)

testWatchPathFilterExplicitArtifactRoot :: IO Bool
testWatchPathFilterExplicitArtifactRoot = do
  let root = "/tmp/app/dist/generated-project"
  pure
    ( Filewatch.shouldTriggerPathRelativeTo root (root FilePath.</> "src" FilePath.</> "Main.elm")
        && not (Filewatch.shouldTriggerPathRelativeTo root (root FilePath.</> "elm-stuff" FilePath.</> "Main.elm"))
    )

testProjectDiscoveryExclusions :: IO Bool
testProjectDiscoveryExclusions = do
  root <- uniqueRoot
  let normal = root FilePath.</> "example"
      nested = normal FilePath.</> "benchmark"
      artifactRoots =
        [ root FilePath.</> ".hidden"
        , root FilePath.</> ".git" FilePath.</> "fixture"
        , root FilePath.</> "node_modules" FilePath.</> "fixture"
        , root FilePath.</> "elm-stuff" FilePath.</> "fixture"
        , root FilePath.</> "dist" FilePath.</> "fixture"
        ]
      makeProject projectRoot = do
        writeElmApp projectRoot
        let srcDir = projectRoot FilePath.</> "src"
        Dir.createDirectoryIfMissing True srcDir
        writeFile (srcDir FilePath.</> "Main.elm") "module Main exposing (main)\n\nmain = 1\n"
  Dir.createDirectoryIfMissing True root
  mapM_ makeProject (normal : nested : artifactRoots)
  discovered <- Project.discover root
  directArtifact <- Project.discover (last artifactRoots)
  let roots = map Project.getRoot discovered
  canonicalNormal <- Dir.canonicalizePath normal
  canonicalNested <- Dir.canonicalizePath nested
  canonicalArtifact <- Dir.canonicalizePath (last artifactRoots)
  pure
    ( canonicalNormal `elem` roots
        && canonicalNested `elem` roots
        && all (`notElem` roots) artifactRoots
        && map Project.getRoot directArtifact == [canonicalArtifact]
    )

testSiblingSourceRegistration :: IO Bool
testSiblingSourceRegistration = do
  root <- uniqueRoot
  let app = root FilePath.</> "app"
      sibling = root FilePath.</> "shared"
      worker = sibling FilePath.</> "Worker.elm"
      flags = CompileHelpers.Flags CompileHelpers.Dev CompileHelpers.NoOutput CompileHelpers.DebuggerNone
  Dir.createDirectoryIfMissing True app
  Dir.createDirectoryIfMissing True sibling
  writeFile worker "module Worker exposing (main)\n\nmain = 1\n"
  state <- Client.initState testUrls
  let register src = do
        writeFile (app FilePath.</> "elm.json")
          (Text.unpack (Text.replace "[\"src\"]" ("[\"" <> Text.pack src <> "\"]") (Text.pack basicElmJson)))
        (projects, errors) <- Project.discoverWithErrors app
        case projects of
          [project] -> do
            result <- ProjectState.upsert state flags app (Project._entrypoints project)
            pure (null errors && either (const False) (const True) result)
          _ -> pure False
  siblingOk <- register "../shared"
  Dir.createDirectoryLink sibling (app FilePath.</> "linked")
  symlinkOk <- register "linked"
  -- A file in a similarly named directory must still be rejected.
  let outside = root FilePath.</> "shared-other" FilePath.</> "Worker.elm"
  Dir.createDirectoryIfMissing True (FilePath.takeDirectory outside)
  writeFile outside "module Worker exposing (main)\n\nmain = 1\n"
  rejected <- ProjectState.upsert state flags app (NE.List outside [])
  pure (siblingOk && symlinkOk && case rejected of
    Left (ProjectState.EntrypointOutsideSourceDirs _ _) -> True
    _ -> False)

testDiscoveryRetry :: IO Bool
testDiscoveryRetry = do
  root <- uniqueRoot
  writeElmApp root
  state <- Client.initState testUrls
  first <- DiscoverState.discoverWithErrors state root
  Dir.createDirectoryIfMissing True (root FilePath.</> "src")
  writeFile (root FilePath.</> "src/Main.elm") "module Main exposing (main)\n\nmain = 1\n"
  second <- DiscoverState.discoverWithErrors state root
  canonical <- Dir.canonicalizePath root
  registered <- Client.getExistingProject canonical state
  pure (not (null first) && null second && either (const False) (const True) registered)

testDiscoveryErrors :: IO Bool
testDiscoveryErrors = do
  root <- uniqueRoot
  writeElmApp root
  (_, outlineErrors) <- Project.discoverWithErrors root
  Dir.createDirectoryIfMissing True (root FilePath.</> "src")
  writeFile (root FilePath.</> "src/Main.elm") "module Main exposing (main)\n\nmain =\n"
  (_, entrypointErrors) <- Project.discoverWithErrors root
  writeElmDevConfig root invalidThemeElmDevConfig
  (_, generationErrors) <- Project.discoverWithErrors root
  pure (any (List.isInfixOf "src") outlineErrors
    && any (List.isInfixOf "No application entrypoints") entrypointErrors
    && any (List.isInfixOf "Code generation failed") generationErrors)

testProjectContainsSegmentAware :: IO Bool
testProjectContainsSegmentAware = do
  let root = "/tmp/app"
      proj = Project.Project root root (NE.List (root FilePath.</> "src" FilePath.</> "Main.elm") []) [root FilePath.</> "src"] 1
      good = Project.contains "/tmp/app/src/Main.elm" proj
      exact = Project.contains "/tmp/app" proj
      sibling = Project.contains "/tmp/app2/src/Main.elm" proj
  pure (good && exact && not sibling)

testProjectAffectsCompilation :: IO Bool
testProjectAffectsCompilation = do
  let root = "/tmp/app"
      externalSrc = "/tmp/shared-src"
      proj = Project.Project root root (NE.List (root FilePath.</> "src" FilePath.</> "Main.elm") []) [root FilePath.</> "src"] 1
      externalProj = Project.Project root root (NE.List (externalSrc FilePath.</> "Main.elm") []) [externalSrc] 1
      srcElm = Project.affectsCompilation "/tmp/app/src/Main.elm" proj
      elmJson = Project.affectsCompilation "/tmp/app/elm.json" proj
      elmDevJson = Project.affectsCompilation "/tmp/app/elm.dev.json" proj
      externalSourceElm = Project.affectsCompilation "/tmp/shared-src/Main.elm" externalProj
      outsideSrc = Project.affectsCompilation "/tmp/app/tests/Main.elm" proj
      generated = Project.affectsCompilation "/tmp/app/elm-stuff/generated/Main.elm" proj
      nonElm = Project.affectsCompilation "/tmp/app/src/version.ts" proj
  pure (srcElm && elmJson && elmDevJson && externalSourceElm && not outsideSrc && not generated && not nonElm)

testApplicationEntrypointGroupsSplit :: IO Bool
testApplicationEntrypointGroupsSplit = do
  root <- uniqueRoot
  let srcDir = root FilePath.</> "src"
      mainA = srcDir FilePath.</> "Main.elm"
      mainB = srcDir FilePath.</> "Other.elm"
      proj = Project.Project root root (NE.List mainA [mainB]) [srcDir] 1
  writeElmApp root
  Dir.createDirectoryIfMissing True srcDir
  writeFile mainA "module Main exposing (main)\n\nmain = 1\n"
  writeFile mainB "module Other exposing (main)\n\nmain = 2\n"
  groups <- Project.entrypointGroupsForChangedFiles [root FilePath.</> "elm.json"] proj
  pure (map NE.toList groups == [[mainA], [mainB]])

testApplicationEntrypointGroupsTargeted :: IO Bool
testApplicationEntrypointGroupsTargeted = do
  root <- uniqueRoot
  let srcDir = root FilePath.</> "src"
      mainA = srcDir FilePath.</> "Main.elm"
      mainB = srcDir FilePath.</> "Other.elm"
      shared = srcDir FilePath.</> "Shared.elm"
      onlyA = srcDir FilePath.</> "OnlyA.elm"
      unused = srcDir FilePath.</> "Unused.elm"
      proj = Project.Project root root (NE.List mainA [mainB]) [srcDir] 1
  writeElmApp root
  Dir.createDirectoryIfMissing True srcDir
  writeFile mainA "module Main exposing (main)\n\nimport Shared\nimport OnlyA\n\nmain = 1\n"
  writeFile mainB "module Other exposing (main)\n\nimport Shared\n\nmain = 2\n"
  writeFile shared "module Shared exposing (value)\n\nvalue = 1\n"
  writeFile onlyA "module OnlyA exposing (value)\n\nvalue = 1\n"
  writeFile unused "module Unused exposing (value)\n\nvalue = 1\n"
  sharedGroups <- Project.entrypointGroupsForChangedFiles [shared] proj
  onlyAGroups <- Project.entrypointGroupsForChangedFiles [onlyA] proj
  unusedGroups <- Project.entrypointGroupsForChangedFiles [unused] proj
  pure
    ( map NE.toList sharedGroups == [[mainA], [mainB]]
        && map NE.toList onlyAGroups == [[mainA]]
        && null unusedGroups
    )

testApplicationUnusedModuleDetection :: IO Bool
testApplicationUnusedModuleDetection = do
  root <- uniqueRoot
  let srcDir = root FilePath.</> "src"
      mainA = srcDir FilePath.</> "Main.elm"
      shared = srcDir FilePath.</> "Shared.elm"
      unused = srcDir FilePath.</> "Unused.elm"
      proj = Project.Project root root (NE.List mainA []) [srcDir] 1
  writeElmApp root
  Dir.createDirectoryIfMissing True srcDir
  writeFile mainA "module Main exposing (main)\n\nimport Shared\n\nmain = Shared.value\n"
  writeFile shared "module Shared exposing (value)\n\nvalue = 1\n"
  writeFile unused "module Unused exposing (value)\n\nvalue = 1\n"
  usedResult <- Project.unusedModuleForFile shared proj
  unusedResult <- Project.unusedModuleForFile unused proj
  pure
    ( usedResult == Nothing
        && case unusedResult of
             Just unusedModule -> ModuleName.toChars (Project.unusedModuleName unusedModule) == "Unused"
             Nothing -> False
    )

testPackageChangedSourceIsTargeted :: IO Bool
testPackageChangedSourceIsTargeted = do
  root <- uniqueRoot
  let srcDir = root FilePath.</> "src"
      exposedA = srcDir FilePath.</> "One.elm"
      exposedB = srcDir FilePath.</> "Two.elm"
      internal = srcDir FilePath.</> "Internal.elm"
      proj = Project.Project root root (NE.List exposedA [exposedB]) [srcDir] 1
  writeElmPackage root
  Dir.createDirectoryIfMissing True srcDir
  writeFile exposedA "module One exposing (value)\n\nvalue = 1\n"
  writeFile exposedB "module Two exposing (value)\n\nvalue = 2\n"
  writeFile internal "module Internal exposing (value)\n\nvalue = 3\n"
  exposedGroups <- Project.entrypointGroupsForChangedFiles [exposedA] proj
  internalGroups <- Project.entrypointGroupsForChangedFiles [internal] proj
  configGroups <- Project.entrypointGroupsForChangedFiles [root FilePath.</> "elm.json"] proj
  pure
    ( map NE.toList exposedGroups == [[exposedA]]
        && map NE.toList internalGroups == [[internal]]
        && map NE.toList configGroups == [[exposedA, exposedB]]
    )

testPackageUnusedModuleDetection :: IO Bool
testPackageUnusedModuleDetection = do
  root <- uniqueRoot
  let srcDir = root FilePath.</> "src"
      exposedA = srcDir FilePath.</> "One.elm"
      exposedB = srcDir FilePath.</> "Two.elm"
      internal = srcDir FilePath.</> "Internal.elm"
      unused = srcDir FilePath.</> "Unused.elm"
      proj = Project.Project root root (NE.List exposedA [exposedB]) [srcDir] 1
  writeElmPackage root
  Dir.createDirectoryIfMissing True srcDir
  writeFile exposedA "module One exposing (value)\n\nimport Internal\n\nvalue = Internal.value\n"
  writeFile exposedB "module Two exposing (value)\n\nvalue = 2\n"
  writeFile internal "module Internal exposing (value)\n\nvalue = 3\n"
  writeFile unused "module Unused exposing (value)\n\nvalue = 4\n"
  exposedResult <- Project.unusedModuleForFile exposedA proj
  internalResult <- Project.unusedModuleForFile internal proj
  unusedResult <- Project.unusedModuleForFile unused proj
  pure
    ( exposedResult == Nothing
        && internalResult == Nothing
        && case unusedResult of
             Just unusedModule -> ModuleName.toChars (Project.unusedModuleName unusedModule) == "Unused"
             Nothing -> False
    )

testTargetResultsSuccess :: IO Bool
testTargetResultsSuccess = do
  let success = Client.Success CompileHelpers.CompiledSkippedOutput
      grouped = Client.TargetResults [("/tmp/app/src/Main.elm", success), ("/tmp/app/src/Other.elm", success)]
  pure (Client.compilationResultSucceeded grouped)

testNoOutputEntrypointsBatch :: IO Bool
testNoOutputEntrypointsBatch = do
  root <- uniqueRoot
  let srcDir = root FilePath.</> "src"
      mainPath = srcDir FilePath.</> "Main.elm"
      otherPath = srcDir FilePath.</> "Other.elm"
      sharedPath = srcDir FilePath.</> "Shared.elm"
      flags = CompileHelpers.Flags CompileHelpers.Dev CompileHelpers.NoOutput CompileHelpers.DebuggerNone
  writeElmApp root
  Dir.createDirectoryIfMissing True srcDir
  writeFile mainPath (workerElm "Main" True)
  writeFile otherPath (workerElm "Other" True)
  writeFile sharedPath "module Shared exposing (value)\n\nvalue : Int\nvalue = 1\n"
  (analysisResult, analysisFileInfo) <- MemoryCompile.compile root (NE.List mainPath [otherPath]) flags Nothing
  (state, projectCache, compileResult) <- twoEntrypointProject root flags
  result <- CompileState.compile state "test.no-output-batch" projectCache []
  stored <- STM.readTVarIO compileResult
  let hasAnalysisInfo info = Maybe.isJust (Client.docs info) && Maybe.isJust (Client.canonicalAst info)
  pure
    ( case (analysisResult, result, stored) of
        ( Right CompileHelpers.CompiledSkippedOutput
          , Right CompileHelpers.CompiledSkippedOutput
          , Client.TargetResults [(entrypoint, Client.Success CompileHelpers.CompiledSkippedOutput)]
          ) ->
              entrypoint == mainPath
                && Map.size analysisFileInfo == 3
                && all hasAnalysisInfo (Map.elems analysisFileInfo)
        _ -> False
    )

testNoOutputEntrypointErrors :: IO Bool
testNoOutputEntrypointErrors = do
  root <- uniqueRoot
  let srcDir = root FilePath.</> "src"
      mainPath = srcDir FilePath.</> "Main.elm"
      otherPath = srcDir FilePath.</> "Other.elm"
      sharedPath = srcDir FilePath.</> "Shared.elm"
      flags = CompileHelpers.Flags CompileHelpers.Dev CompileHelpers.NoOutput CompileHelpers.DebuggerNone
  writeElmApp root
  Dir.createDirectoryIfMissing True srcDir
  writeFile mainPath (brokenWorkerElm "Main" "main is broken")
  writeFile otherPath (brokenWorkerElm "Other" "other is broken")
  writeFile sharedPath "module Shared exposing (value)\n\nvalue : Int\nvalue = 1\n"
  (analysisResult, analysisFileInfo) <- MemoryCompile.compile root (NE.List mainPath [otherPath]) flags Nothing
  (state, projectCache, compileResult) <- twoEntrypointProject root flags
  result <- CompileState.compile state "test.no-output-errors" projectCache []
  stored <- STM.readTVarIO compileResult
  pure $
    case (analysisResult, result, stored) of
      ( Left (ReportingExit.ReactorBadBuild (ReportingExit.BuildBadModules _ _ analysisOtherErrors))
        , Left (Client.ReactorError (ReportingExit.ReactorBadBuild (ReportingExit.BuildBadModules _ _ otherErrors)))
        , Client.TargetResults [(entrypoint, Client.Error (Client.ReactorError (ReportingExit.ReactorBadBuild (ReportingExit.BuildBadModules _ _ storedOtherErrors))))]
        ) ->
          entrypoint == mainPath
            && length analysisOtherErrors == 1
            && length otherErrors == 1
            && length storedOtherErrors == 1
            && Map.size analysisFileInfo == 3
            && all (Maybe.isJust . Client.localizer) (Map.elems analysisFileInfo)
            && any (Maybe.isJust . Client.canonicalAst) (Map.elems analysisFileInfo)
      _ -> False

testOutputEntrypointsRemainIndependent :: IO Bool
testOutputEntrypointsRemainIndependent = do
  root <- uniqueRoot
  let srcDir = root FilePath.</> "src"
      mainPath = srcDir FilePath.</> "Main.elm"
      otherPath = srcDir FilePath.</> "Other.elm"
      flags = CompileHelpers.Flags CompileHelpers.Dev (CompileHelpers.OutputTo CompileHelpers.Js) CompileHelpers.DebuggerNone
  writeElmApp root
  Dir.createDirectoryIfMissing True srcDir
  writeFile mainPath (workerElm "Main" False)
  writeFile otherPath (workerElm "Other" False)
  (state, projectCache, compileResult) <- twoEntrypointProject root flags
  result <- CompileState.compile state "test.output-targets" projectCache []
  stored <- STM.readTVarIO compileResult
  case (result, stored) of
      ( Right (CompileHelpers.CompiledJs aggregateBuilder)
        , Client.TargetResults
            [ (mainEntrypoint, Client.Success (CompileHelpers.CompiledJs mainBuilder))
            , (otherEntrypoint, Client.Success (CompileHelpers.CompiledJs otherBuilder))
            ]
        ) -> do
          let mainJs = renderBuilder mainBuilder
              otherJs = renderBuilder otherBuilder
              aggregateJs = renderBuilder aggregateBuilder
          pure (mainEntrypoint == mainPath
               && otherEntrypoint == otherPath
               && List.isInfixOf "$author$project$Main$main" mainJs
               && not (List.isInfixOf "$author$project$Other$main" mainJs)
               && List.isInfixOf "$author$project$Other$main" otherJs
               && not (List.isInfixOf "$author$project$Main$main" otherJs)
               && aggregateJs == otherJs)
      _ -> pure False

twoEntrypointProject :: FilePath -> CompileHelpers.Flags -> IO (Client.State, Client.ProjectCache, STM.TVar Client.CompilationResult)
twoEntrypointProject root flags = do
  let srcDir = root FilePath.</> "src"
      mainPath = srcDir FilePath.</> "Main.elm"
      otherPath = srcDir FilePath.</> "Other.elm"
      proj = Project.Project root root (NE.List mainPath [otherPath]) [srcDir] 1
  state <- Client.initState testUrls
  compileResult <- STM.newTVarIO Client.NotCompiled
  testInfo <- STM.newTVarIO Nothing
  let projectCache = Client.ProjectCache proj GenConfig.defaultDocs flags compileResult testInfo
  STM.atomically $ STM.writeTVar (Client.projects state) [projectCache]
  pure (state, projectCache, compileResult)

workerElm :: String -> Bool -> String
workerElm moduleName importsShared =
  unlines
    [ "module " ++ moduleName ++ " exposing (main)"
    , ""
    , "import Platform"
    , "import Platform.Cmd as Cmd"
    , "import Platform.Sub as Sub"
    , if importsShared then "import Shared" else ""
    , ""
    , "main ="
    , "    Platform.worker"
    , "        { init = \\() -> ( " ++ (if importsShared then "Shared.value" else "0") ++ ", Cmd.none )"
    , "        , update = \\_ model -> ( model, Cmd.none )"
    , "        , subscriptions = \\_ -> Sub.none"
    , "        }"
    ]

brokenWorkerElm :: String -> String -> String
brokenWorkerElm moduleName brokenValue =
  workerElm moduleName True
    ++ "\nbroken : Int\nbroken = \"" ++ brokenValue ++ "\"\n"

testSkippedCompileReplacesStaleResult :: IO Bool
testSkippedCompileReplacesStaleResult = do
  root <- uniqueRoot
  let srcDir = root FilePath.</> "src"
      mainPath = srcDir FilePath.</> "Main.elm"
      unusedPath = srcDir FilePath.</> "Unused.elm"
      proj = Project.Project root root (NE.List mainPath []) [srcDir] 1
      flags = CompileHelpers.Flags CompileHelpers.Dev CompileHelpers.NoOutput CompileHelpers.DebuggerNone
      session = Client.LspSession
        { Client.workspaceDiagnosticsSnapshotFiles = []
        , Client.workspaceDiagnosticsSnapshotOutOfDate = False
        , Client.publishedDiagnosticFiles = Map.empty
        , Client.lspRoot = [root]
        }
  writeElmApp root
  Dir.createDirectoryIfMissing True srcDir
  writeFile mainPath "module Main exposing (main)\n\nmain = 1\n"
  writeFile unusedPath "module Unused exposing (value)\n\nvalue = 1\n"
  state <- Client.initState testUrls
  compileResult <- STM.newTVarIO Client.NotCompiled
  testInfo <- STM.newTVarIO Nothing
  let projectCache = Client.ProjectCache proj GenConfig.defaultDocs flags compileResult testInfo
  STM.atomically $ do
    STM.writeTVar (Client.projects state) [projectCache]
    STM.writeTVar (Client.workspaceDiagnosticsRequested state) (Map.singleton "test-conn" session)
  CompileState.markFilesystemChanged state [unusedPath]
  result <- CompileState.compile state "test.skipped" projectCache [unusedPath]
  stored <- STM.readTVarIO compileResult
  sessions <- STM.readTVarIO (Client.workspaceDiagnosticsRequested state)
  pure
    ( case (result, stored, Map.lookup "test-conn" sessions) of
        (Right CompileHelpers.CompiledSkippedOutput, Client.Success CompileHelpers.CompiledSkippedOutput, Just updatedSession) ->
          Client.workspaceDiagnosticsSnapshotOutOfDate updatedSession
        _ -> False
    )

testOpenFileSkipsFilesystemSync :: IO Bool
testOpenFileSkipsFilesystemSync = do
  let path = "/tmp/app/src/Main.elm"
      connId = Text.pack "test-conn"
      editorsOpen = EditorsOpen.fileMarkedOpen connId path EditorsOpen.empty
      shouldSyncOpen = ProjectState.shouldSyncFilesystemPath path editorsOpen
      shouldSyncClosed = ProjectState.shouldSyncFilesystemPath path (EditorsOpen.fileMarkedClosed connId path editorsOpen)
  pure (not shouldSyncOpen && shouldSyncClosed)

testCompileRefreshPreservesOpenFile :: IO Bool
testCompileRefreshPreservesOpenFile = do
  root <- uniqueRoot
  let srcDir = root FilePath.</> "src"
      path = srcDir FilePath.</> "Main.elm"
      diskContents = TextEncoding.encodeUtf8 "module Main exposing (value)\n\nvalue = 1\n"
      editorContents = TextEncoding.encodeUtf8 "module Main exposing (value)\n\nvalue = 2\n"
      proj = Project.Project root root (NE.List path []) [srcDir] 1
      connId = Text.pack "test-conn"
  Dir.createDirectoryIfMissing True srcDir
  BS.writeFile path diskContents
  FileCache.insert path editorContents
  state <- Client.initState testUrls
  STM.atomically $ do
    editors <- STM.readTVar (Client.projectsBeingEdited state)
    STM.writeTVar (Client.projectsBeingEdited state) (EditorsOpen.fileMarkedOpen connId path editors)

  changed <- CompileState.updateVfsFromFs state proj
  cached <- FileCache.readUtf8 path
  FileCache.delete path
  pure (not changed && cached == editorContents)

testCleanupConnectionState :: IO Bool
testCleanupConnectionState = do
  state <- Client.initState testUrls
  let connA = Text.pack "conn-a"
      connB = Text.pack "conn-b"
      pathA = "/tmp/app/src/A.elm"
      pathB = "/tmp/app/src/B.elm"
      lspSession root =
        Client.LspSession
          { Client.workspaceDiagnosticsSnapshotFiles = [Protocol.Uri (Text.pack root)]
          , Client.workspaceDiagnosticsSnapshotOutOfDate = False
          , Client.publishedDiagnosticFiles = Map.empty
          , Client.lspRoot = [root]
          }
  STM.atomically $ do
    editors <- STM.readTVar (Client.projectsBeingEdited state)
    STM.writeTVar (Client.projectsBeingEdited state)
      (EditorsOpen.fileMarkedOpen connB pathB (EditorsOpen.fileMarkedOpen connA pathA editors))
    STM.writeTVar (Client.workspaceDiagnosticsRequested state)
      (Map.fromList [(connA, lspSession "/tmp/app"), (connB, lspSession "/tmp/other")])
  Client.cleanupConnectionState state connA
  editors' <- STM.readTVarIO (Client.projectsBeingEdited state)
  requested' <- STM.readTVarIO (Client.workspaceDiagnosticsRequested state)
  pure
    ( not (EditorsOpen.isFileOpen pathA editors')
        && EditorsOpen.isFileOpen pathB editors'
        && Map.notMember connA requested'
        && Map.member connB requested'
    )

testUpdateProjectFileInfo :: IO Bool
testUpdateProjectFileInfo = do
  let root = "/tmp/project-a"
      proj = Project.Project root root (NE.List (root FilePath.</> "src" FilePath.</> "Main.elm") []) [root FilePath.</> "src"] 1
      stalePath = root FilePath.</> "src" FilePath.</> "Old.elm"
      keptPath = root FilePath.</> "src" FilePath.</> "Main.elm"
      otherPath = "/tmp/project-b/src/Other.elm"
      mkFileInfo = Client.FileInfo [] Nothing Nothing Nothing Nothing Nothing
      current = Map.fromList [(stalePath, mkFileInfo), (keptPath, mkFileInfo), (otherPath, mkFileInfo)]
      latest = Map.fromList [(keptPath, mkFileInfo)]
      updated = CompileState.updateProjectFileInfo proj True (Right ()) current latest
  pure (Map.notMember stalePath updated && Map.member keptPath updated && Map.member otherPath updated)

testDuplicateChangeEventsMarkDirtyOnce :: IO Bool
testDuplicateChangeEventsMarkDirtyOnce = do
  root <- uniqueRoot
  let srcDir = root FilePath.</> "src"
      path = srcDir FilePath.</> "Main.elm"
      oldContents = BS.pack (map (fromIntegral . fromEnum) "module Main exposing (old)\n\nold = 1\n")
      newContents = BS.pack (map (fromIntegral . fromEnum) "module Main exposing (new)\n\nnew = 1\n")
      proj = Project.Project root root (NE.List path []) [srcDir] 1
      flags = CompileHelpers.Flags CompileHelpers.Dev CompileHelpers.NoOutput CompileHelpers.DebuggerNone
  Dir.createDirectoryIfMissing True srcDir
  BS.writeFile path newContents
  FileCache.insert path oldContents
  state <- Client.initState testUrls
  compileResult <- STM.newTVarIO Client.NotCompiled
  testInfo <- STM.newTVarIO Nothing
  STM.atomically $ do
    STM.writeTVar (Client.projects state) [Client.ProjectCache proj GenConfig.defaultDocs flags compileResult testInfo]

  didChangeChanged <- FileCache.insertIfChanged path newContents
  when didChangeChanged (CompileState.markFilesystemChanged state [path])
  didSaveChanged <- FileCache.insertIfChanged path newContents
  when didSaveChanged (CompileState.markFilesystemChanged state [path])
  devFileChanged <- FileCache.insertIfChanged path newContents
  when devFileChanged (CompileState.markFilesystemChanged state [path])

  versions <- Versions.readVersions root
  pure
    ( didChangeChanged
        && not didSaveChanged
        && not devFileChanged
        && Versions.fsVersion versions == 1
        && Versions.compileVersion versions == 0
    )

testJavascriptCancellation :: IO Bool
testJavascriptCancellation = do
  results <- sequence
    [ (Exception.try (Timeout.timeout 20000 (Javascript.run "setInterval(() => {}, 1000);" "{}"))
        :: IO (Either Exception.SomeException (Maybe (Either Javascript.RunError String))))
    | _ <- [1..5 :: Int]
    ]
  pure (all wasCancelled results)
  where
    wasCancelled (Right Nothing) = True
    wasCancelled _ = False


testIsolatedTestLauncherSuccess :: IO Bool
#ifdef mingw32_HOST_OS
testIsolatedTestLauncherSuccess = pure True
#else
testIsolatedTestLauncherSuccess =
  withWorkerScript successWorkerScript $ \worker -> do
    result <- TestRunner.runIsolatedWith worker (Just 5) (Just ["Suite.tests"]) (Just 1) (Just 2) "/tmp/project"
    pure $ case result of
      Right success ->
        TestRunner.isolatedRendered success == "worker result"
          && TestRunner.isolatedTestFiles success == ["tests/Suite.elm"]
          && TestRunner.summaryTotal (TestRunner.isolatedSummary success) == 1
          && TestRunner.summaryPassed (TestRunner.isolatedSummary success) == 1
          && TestRunner.isolatedSeed success == 1
          && TestRunner.isolatedFuzz success == 2
      Left _ -> False
#endif


testIsolatedTestLauncherTimeout :: IO Bool
#ifdef mingw32_HOST_OS
testIsolatedTestLauncherTimeout = pure True
#else
testIsolatedTestLauncherTimeout =
  withWorkerScript timeoutWorkerScript $ \worker -> do
    result <- TestRunner.runIsolatedWith worker (Just 5) Nothing Nothing Nothing "/tmp/project"
    pure $ case result of
      Left (TestRunner.TimedOut 5) -> True
      _ -> False
#endif


testIsolatedTestLauncherCancellation :: IO Bool
#ifdef mingw32_HOST_OS
testIsolatedTestLauncherCancellation = pure True
#else
testIsolatedTestLauncherCancellation =
  Temp.withSystemTempDirectory "elm-dev-worker-cancellation" $ \root -> do
    let worker = root FilePath.</> "worker.sh"
        childPidPath = root FilePath.</> "child.pid"
        script = unlines
          [ "#!/bin/sh"
          , "sleep 30 &"
          , "child=$!"
          , "printf '%s' \"$child\" > \"" ++ childPidPath ++ "\""
          , "wait"
          ]
    writeExecutable worker script
    result <- Timeout.timeout 500000 (TestRunner.runIsolatedWith worker Nothing Nothing Nothing Nothing "/tmp/project")
    pidExists <- Dir.doesFileExist childPidPath
    if not pidExists
      then pure False
      else do
        pidText <- readFile childPidPath
        case Read.readMaybe pidText of
          Nothing -> pure False
          Just pid -> do
            stopped <- waitForProcessToStop 40 pid
            pure (case result of { Nothing -> stopped; Just _ -> False })
#endif


testIsolatedTestLauncherRepeatedRuns :: IO Bool
#ifdef mingw32_HOST_OS
testIsolatedTestLauncherRepeatedRuns = pure True
#else
testIsolatedTestLauncherRepeatedRuns =
  withWorkerScript successWorkerScript $ \worker -> do
    results <- sequence
      [ TestRunner.runIsolatedWith worker Nothing Nothing (Just runNumber) (Just 1) "/tmp/project"
      | runNumber <- [1..10]
      ]
    pure (all isSuccess results)
  where
    isSuccess (Right success) = TestRunner.isolatedRendered success == "worker result"
    isSuccess (Left _) = False
#endif


successWorkerResponse :: BS.ByteString
successWorkerResponse = "{\"status\":\"ok\",\"rendered\":\"worker result\",\"testFiles\":[\"tests/Suite.elm\"],\"total\":1,\"passed\":1,\"failed\":0,\"failures\":[],\"seed\":1,\"fuzz\":2,\"durationMs\":3}"


#ifndef mingw32_HOST_OS
withWorkerScript :: String -> (FilePath -> IO a) -> IO a
withWorkerScript script action =
  Temp.withSystemTempDirectory "elm-dev-worker-script" $ \root -> do
    let worker = root FilePath.</> "worker.sh"
    writeExecutable worker script
    action worker


writeExecutable :: FilePath -> String -> IO ()
writeExecutable path contents = do
  BS.writeFile path (BSChar8.pack contents)
  permissions <- Dir.getPermissions path
  Dir.setPermissions path permissions { Dir.executable = True }


successWorkerScript :: String
successWorkerScript = unlines
  [ "#!/bin/sh"
  , "[ \"$1\" = \"__elm-dev-test-worker\" ] || exit 40"
  , "case \"$GHCRTS\" in *-M1024m*) ;; *) exit 41 ;; esac"
  , "case \"$NODE_OPTIONS\" in *--max-old-space-size=768*) ;; *) exit 42 ;; esac"
  , "printf '%s' '" ++ BSChar8.unpack successWorkerResponse ++ "' > \"$3\""
  ]


timeoutWorkerScript :: String
timeoutWorkerScript = unlines
  [ "#!/bin/sh"
  , "printf '%s' '{\"status\":\"error\",\"message\":\"Tests timed out after 5000ms\",\"timeoutSeconds\":5}' > \"$3\""
  ]


processExists :: Int -> IO Bool
processExists pid = do
  (exitCode, processState, _) <- Process.readProcessWithExitCode "ps" ["-o", "stat=", "-p", show pid] ""
  pure $ case (exitCode, dropWhile (== ' ') processState) of
    (Exit.ExitSuccess, state : _) -> state /= 'Z'
    _ -> False


waitForProcessToStop :: Int -> Int -> IO Bool
waitForProcessToStop attempts pid = do
  exists <- processExists pid
  if not exists
    then pure True
    else if attempts <= 0
      then pure False
      else Concurrent.threadDelay 50000 >> waitForProcessToStop (attempts - 1) pid
#endif

testGenerateBasicConfig :: IO Bool
testGenerateBasicConfig = do
  root <- uniqueRoot
  Dir.createDirectoryIfMissing True root
  writeElmDevConfig root basicElmDevConfig
  result <- Generate.run root
  let mainPath = root FilePath.</> "elm-stuff" FilePath.</> "generated" FilePath.</> "Main.elm"
  generatedMainExists <- Dir.doesFileExist mainPath
  pure $ case result of
    Right () -> generatedMainExists
    Left _ -> False

testGenerateRepairsOutputs :: IO Bool
testGenerateRepairsOutputs = do
  root <- uniqueRoot
  Dir.createDirectoryIfMissing True root
  writeElmDevConfig root basicElmDevConfig
  first <- Generate.run root
  let generated = root FilePath.</> "elm-stuff/generated"
      mainPath = generated FilePath.</> "Main.elm"
      templatePath = generated FilePath.</> "Effect.elm"
      hashPath = generated FilePath.</> "generated.hash"
  before <- Dir.getModificationTime hashPath
  unchanged <- Generate.run root
  after <- Dir.getModificationTime hashPath
  Dir.removeFile mainPath
  repairedMain <- Generate.run root
  mainExists <- Dir.doesFileExist mainPath
  Dir.removeFile templatePath
  repairedTemplate <- Generate.run root
  templateExists <- Dir.doesFileExist templatePath
  Dir.removeFile (generated FilePath.</> "generated-files.json")
  repairedManifest <- Generate.run root
  manifestExists <- Dir.doesFileExist (generated FilePath.</> "generated-files.json")
  pure (all (== Right ()) [first, unchanged, repairedMain, repairedTemplate, repairedManifest]
    && before == after && mainExists && templateExists && manifestExists)

testGenerateThemeDecodeError :: IO Bool
testGenerateThemeDecodeError = do
  root <- uniqueRoot
  Dir.createDirectoryIfMissing True root
  writeElmDevConfig root invalidThemeElmDevConfig
  result <- Generate.run root
  pure $ case result of
    Left err ->
      (List.isInfixOf "Generation failed:" err || List.isInfixOf "Error decoding flags" err)
        && not (List.isInfixOf "key \"generated\" not found" err)
    Right _ -> False

testScaffoldInitStopsOnElmJsonFailure :: IO Bool
testScaffoldInitStopsOnElmJsonFailure = do
  root <- uniqueRoot
  Dir.createDirectoryIfMissing True root
  generationCalled <- IORef.newIORef False
  result <-
    InitCommand.runWith
      (\_ -> pure (Left ReportingExit.InitAlreadyExists))
      (\_ -> IORef.writeIORef generationCalled True >> pure (Right ()))
      root
  readmeExists <- Dir.doesFileExist (root FilePath.</> "README.md")
  elmDevExists <- Dir.doesFileExist (root FilePath.</> "elm.dev.json")
  called <- IORef.readIORef generationCalled
  pure $ case result of
    Left err ->
      Text.isInfixOf "EXISTING PROJECT" err
        && not called
        && not readmeExists
        && not elmDevExists
    Right () -> False

testScaffoldInitSurfacesGenerationFailure :: IO Bool
testScaffoldInitSurfacesGenerationFailure = do
  root <- uniqueRoot
  Dir.createDirectoryIfMissing True root
  result <-
    InitCommand.runWith
      (\_ -> pure (Right ()))
      (\_ -> pure (Left "Generation blew up"))
      root
  pure $ case result of
    Left err -> Text.isInfixOf "Generation blew up" err
    Right () -> False

writeElmDevConfig :: FilePath -> String -> IO ()
writeElmDevConfig root configContents =
  writeFile (root FilePath.</> "elm.dev.json") configContents

writeElmApp :: FilePath -> IO ()
writeElmApp root = do
  Dir.createDirectoryIfMissing True root
  writeFile (root FilePath.</> "elm.json") basicElmJson

writeElmPackage :: FilePath -> IO ()
writeElmPackage root = do
  Dir.createDirectoryIfMissing True root
  writeFile (root FilePath.</> "elm.json") basicPackageElmJson

testUrls :: Client.Urls
testUrls =
  Client.Urls
    { Client.urlsLsp = Nothing
    , Client.urlsMcp = Nothing
    , Client.urlsDevHttp = "http://localhost"
    , Client.urlsDevWebsocket = "ws://localhost"
    }

basicElmDevConfig :: String
basicElmDevConfig =
  unlines
    [ "{"
    , "  \"pages\": {"
    , "    \"Home\": \"/\""
    , "  },"
    , "  \"assets\": {"
    , "    \"./public\": \"assets\""
    , "  }"
    , "}"
    ]

basicElmJson :: String
basicElmJson =
  unlines
    [ "{"
    , "  \"type\": \"application\","
    , "  \"source-directories\": [\"src\"],"
    , "  \"elm-version\": \"0.19.1\","
    , "  \"dependencies\": {"
    , "    \"direct\": { \"elm/core\": \"1.0.5\" },"
    , "    \"indirect\": { \"elm/json\": \"1.1.3\" }"
    , "  },"
    , "  \"test-dependencies\": {"
    , "    \"direct\": {},"
    , "    \"indirect\": {}"
    , "  }"
    , "}"
    ]

basicPackageElmJson :: String
basicPackageElmJson =
  unlines
    [ "{"
    , "  \"type\": \"package\","
    , "  \"name\": \"author/project\","
    , "  \"summary\": \"Test package\","
    , "  \"license\": \"BSD-3-Clause\","
    , "  \"version\": \"1.0.0\","
    , "  \"exposed-modules\": [\"One\", \"Two\"],"
    , "  \"elm-version\": \"0.19.0 <= v < 0.20.0\","
    , "  \"dependencies\": { \"elm/core\": \"1.0.0 <= v < 2.0.0\" },"
    , "  \"test-dependencies\": {}"
    , "}"
    ]

addThemeElmDevConfig :: String
addThemeElmDevConfig =
  unlines
    [ "{"
    , "  \"assets\": {"
    , "    \"./public\": \"assets\""
    , "  },"
    , "  \"pages\": {"
    , "    \"Game\": \"/game/:gameId\","
    , "    \"Home\": \"/\","
    , "    \"Lobby\": \"/lobby\""
    , "  },"
    , "  \"theme\": {"
    , "    \"borders\": {"
    , "      \"radius\": { \"md\": 8, \"sm\": 4 },"
    , "      \"width\": { \"md\": 2, \"sm\": 1 }"
    , "    },"
    , "    \"colorRoles\": {"
    , "      \"background\": {"
    , "        \"@dark\": { \"canvas\": \"neutral10\", \"primary\": \"brand80\", \"surface\": \"neutral20\" },"
    , "        \"canvas\": \"white50\", \"primary\": \"brand40\", \"surface\": \"neutral95\""
    , "      },"
    , "      \"border\": {"
    , "        \"@dark\": { \"default\": \"neutral30\", \"focus\": \"brand80\" },"
    , "        \"default\": \"neutral80\", \"focus\": \"brand50\""
    , "      },"
    , "      \"text\": {"
    , "        \"@dark\": { \"default\": \"neutral90\", \"muted\": \"neutral70\", \"onBrand\": \"white95\" },"
    , "        \"default\": \"neutral20\", \"muted\": \"neutral40\", \"onBrand\": \"white50\""
    , "      }"
    , "    },"
    , "    \"colors\": {"
    , "      \"black\": \"#111111\","
    , "      \"brand\": { \"swatchFrom\": \"#2563EB\" },"
    , "      \"neutral\": { \"swatchFrom\": \"#6B7280\" },"
    , "      \"white\": \"#FFFFFF\""
    , "    },"
    , "    \"scale\": 4,"
    , "    \"target\": \"elm-ui\","
    , "    \"typography\": {"
    , "      \"families\": {"
    , "        \"serifDisplay\": [\"EB Garamond\", \"serif\"],"
    , "        \"uiSans\": [\"Inter\", \"sans-serif\"]"
    , "      },"
    , "      \"instances\": {"
    , "        \"body\": { \"family\": \"uiSans\", \"lineHeight\": 1.5, \"size\": 16, \"weight\": 400 },"
    , "        \"title\": { \"family\": \"serifDisplay\", \"lineHeight\": 1.2, \"size\": 32, \"weight\": 700 }"
    , "      }"
    , "    }"
    , "  }"
    , "}"
    ]

invalidThemeElmDevConfig :: String
invalidThemeElmDevConfig =
  unlines
    [ "{"
    , "  \"assets\": {"
    , "    \"./public\": \"assets\""
    , "  },"
    , "  \"pages\": {"
    , "    \"Home\": \"/\""
    , "  },"
    , "  \"theme\": {"
    , "    \"target\": \"not-a-real-target\""
    , "  }"
    , "}"
    ]
