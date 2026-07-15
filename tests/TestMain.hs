{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (when)
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.STM as STM
import qualified Control.Exception as Exception
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy.Char8 as LazyChar8
import qualified Data.Foldable as Foldable
import qualified Ext.FileCache as FileCache
import qualified Ext.Filewatch as Filewatch
import qualified Ext.Test.Result as TestResult
import qualified Ext.Test.Result.Report as TestReport
import qualified Data.Map.Strict as Map
import qualified Data.NonEmptyList as NE
import qualified Ext.CompileHelpers.Disk as DiskCompile
import qualified Ext.Optimization.Level as Optimization
import qualified Gen.Generate as Generate
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
import qualified Watchtower.State.Project as ProjectState
import qualified Watchtower.State.TestJobs as TestJobs
import qualified Watchtower.Server.LSP.EditorsOpen as EditorsOpen
import qualified Watchtower.Server.MCP as MCP
import qualified System.Exit as Exit
import qualified System.Directory as Dir
import qualified System.FilePath as FilePath
import qualified System.Process as Process
import qualified Data.List as List
import qualified Data.IORef as IORef
import qualified Data.Text as Text
import qualified Data.Time.Clock.POSIX as POSIX
import qualified Elm.ModuleName as ModuleName
import qualified Gen.Commands.Init as InitCommand
import qualified System.Timeout as Timeout

main :: IO ()
main = do
  fileCacheResults <- sequence fileCacheTests
  versionResults <- sequence versionTests
  generationResults <- sequence generationTests
  optimizationResults <- sequence optimizationTests
  mcpResults <- sequence mcpTests
  let results = fileCacheResults ++ versionResults ++ generationResults ++ optimizationResults ++ mcpResults
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

versionTests :: [NamedTest]
versionTests =
  [ runTest "versions initialize to zero" testVersionsInit
  , runTest "versions bump and set compile" testVersionsBumpAndSet
  , runTest "watch path filter keeps elm sources/config" testWatchPathFilterRelevant
  , runTest "watch path filter excludes elm-stuff artifacts" testWatchPathFilterElmStuff
  , runTest "project contains is segment-aware" testProjectContainsSegmentAware
  , runTest "project affectsCompilation is source-aware" testProjectAffectsCompilation
  , runTest "application entrypoints compile as singleton groups" testApplicationEntrypointGroupsSplit
  , runTest "application entrypoints are targeted by imports" testApplicationEntrypointGroupsTargeted
  , runTest "application unused modules are detected" testApplicationUnusedModuleDetection
  , runTest "package changed source is targeted directly" testPackageChangedSourceIsTargeted
  , runTest "package unused modules are detected from exposed roots" testPackageUnusedModuleDetection
  , runTest "target results preserve per-entrypoint success" testTargetResultsSuccess
  , runTest "open file keeps in-memory precedence over fs watcher" testOpenFileSkipsFilesystemSync
  , runTest "disconnect cleanup removes editor and diagnostics state" testCleanupConnectionState
  , runTest "successful project fileInfo update replaces stale entries" testUpdateProjectFileInfo
  , runTest "duplicate didChange/save/fileChanged content only marks dirty once" testDuplicateChangeEventsMarkDirtyOnce
  ]

generationTests :: [NamedTest]
generationTests =
  [ runTest "code generation succeeds with basic config" testGenerateBasicConfig
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
  , runTest "MCP hides retired single-purpose tools" testMcpRetiredToolsHidden
  , runTest "MCP tool surface is exact" testMcpToolSurfaceExact
  , runTest "MCP composite schemas advertise tagged unions" testMcpCompositeSchemas
  , runTest "MCP stale test summaries can be cleared" testMcpClearTestResults
  , runTest "MCP XML blocks preserve tag attributes and body" testMcpXmlBlockShape
  , runTest "MCP XML attributes are escaped" testMcpXmlAttributeEscaping
  , runTest "MCP test jobs complete and retain results" testMcpTestJobCompletion
  , runTest "MCP test jobs cancel running actions" testMcpTestJobCancellation
  , runTest "MCP test jobs cancel while queued" testMcpQueuedTestJobCancellation
  , runTest "MCP test jobs retain only recent terminal results" testMcpTestJobRetentionBound
  , runTest "test reports decode and render exported-value durations" testReportDuration
  ]

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
  pure (docsOk && addOk && checkOk)

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


testMcpTestJobRetentionBound :: IO Bool
testMcpTestJobRetentionBound = do
  registry <- TestJobs.newRegistry
  submitted <- mapM
    (\_ -> TestJobs.submitWith registry "/tmp/project" $ \_ ->
      pure (TestJobs.ActionCompleted TestJobs.Completion
        { TestJobs.completionOutcome = TestJobs.TestsPassed
        , TestJobs.completionBody = "report"
        , TestJobs.completionPassed = 1
        , TestJobs.completionFailed = 0
        , TestJobs.completionTotal = 1
        }))
    [(1 :: Int)..22]
  mapM_ (\job -> TestJobs.waitForWith registry (TestJobs.jobId job) 1) submitted
  retained <- TestJobs.listJobsForRootWith registry "/tmp/project"
  let retainedIds = map TestJobs.jobId retained
  pure (length retained == 20 && "test-1" `notElem` retainedIds && "test-2" `notElem` retainedIds)


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
  pure (Versions.fsVersion versions == 0 && Versions.compileVersion versions == 0)

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

testOpenFileSkipsFilesystemSync :: IO Bool
testOpenFileSkipsFilesystemSync = do
  let path = "/tmp/app/src/Main.elm"
      connId = Text.pack "test-conn"
      editorsOpen = EditorsOpen.fileMarkedOpen connId path EditorsOpen.empty
      shouldSyncOpen = ProjectState.shouldSyncFilesystemPath path editorsOpen
      shouldSyncClosed = ProjectState.shouldSyncFilesystemPath path (EditorsOpen.fileMarkedClosed connId path editorsOpen)
  pure (not shouldSyncOpen && shouldSyncClosed)

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
