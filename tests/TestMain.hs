module Main where

import Control.Monad (when)
import qualified Control.Concurrent.STM as STM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy.Char8 as LazyChar8
import qualified Ext.FileCache as FileCache
import qualified Ext.Filewatch as Filewatch
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
import qualified Watchtower.Server.LSP.EditorsOpen as EditorsOpen
import qualified System.Exit as Exit
import qualified System.Directory as Dir
import qualified System.FilePath as FilePath
import qualified System.Process as Process
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Time.Clock.POSIX as POSIX

main :: IO ()
main = do
  fileCacheResults <- sequence fileCacheTests
  versionResults <- sequence versionTests
  generationResults <- sequence generationTests
  optimizationResults <- sequence optimizationTests
  let results = fileCacheResults ++ versionResults ++ generationResults ++ optimizationResults
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
  , runTest "package entrypoints stay grouped" testPackageEntrypointGroupsStayGrouped
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
  ]

optimizationTests :: [NamedTest]
optimizationTests =
  [ runTest "O2 function helpers use arity-specific raw function fields" testO2FunctionHelpers
  , runTest "O0 function helpers preserve default wrapper fields" testO0FunctionHelpers
  , runTest "O2 function helpers preserve runtime call behavior" testO2FunctionHelpersRuntime
  , runTest "make mode maps -O2 and -O3 to optimization levels" testMakeOptimizationModes
  , runTest "make flags parse -O2 shorthand" testMakeO2FlagParsing
  , runTest "compiled O0/O2/O3 JS preserves worker runtime output" testCompiledOptimizationRuntime
  ]

testO2FunctionHelpers :: IO Bool
testO2FunctionHelpers = do
  let js = renderBuilder (JsFunctions.functions (Mode.Prod Optimization.O2 Map.empty Map.empty Map.empty))
  pure
    ( List.isInfixOf "curried.a2 = fun;" js
        && List.isInfixOf "return fun.a2 ? fun.a2(a, b) : fun(a)(b);" js
        && List.isInfixOf "curried.a9 = fun;" js
        && List.isInfixOf "return fun.a9 ? fun.a9(a, b, c, d, e, f, g, h, i)" js
        && not (List.isInfixOf "wrapper.a = arity;" js)
    )

testO0FunctionHelpers :: IO Bool
testO0FunctionHelpers = do
  let js = renderBuilder (JsFunctions.functions (Mode.Prod Optimization.O0 Map.empty Map.empty Map.empty))
  pure
    ( List.isInfixOf "wrapper.a = arity;" js
        && List.isInfixOf "wrapper.f = fun;" js
        && List.isInfixOf "return fun.a === 2 ? fun.f(a, b) : fun(a)(b);" js
        && not (List.isInfixOf "curried.a2 = fun;" js)
    )

testO2FunctionHelpersRuntime :: IO Bool
testO2FunctionHelpersRuntime = do
  let helpers = renderBuilder (JsFunctions.functions (Mode.Prod Optimization.O2 Map.empty Map.empty Map.empty))
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
      proj = Project.Project root root (NE.List (root FilePath.</> "src" FilePath.</> "Main.elm") []) [root FilePath.</> "src"] 1
      srcElm = Project.affectsCompilation "/tmp/app/src/Main.elm" proj
      elmJson = Project.affectsCompilation "/tmp/app/elm.json" proj
      elmDevJson = Project.affectsCompilation "/tmp/app/elm.dev.json" proj
      outsideSrc = Project.affectsCompilation "/tmp/app/tests/Main.elm" proj
      generated = Project.affectsCompilation "/tmp/app/elm-stuff/generated/Main.elm" proj
      nonElm = Project.affectsCompilation "/tmp/app/src/version.ts" proj
  pure (srcElm && elmJson && elmDevJson && not outsideSrc && not generated && not nonElm)

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

testPackageEntrypointGroupsStayGrouped :: IO Bool
testPackageEntrypointGroupsStayGrouped = do
  root <- uniqueRoot
  let srcDir = root FilePath.</> "src"
      exposedA = srcDir FilePath.</> "One.elm"
      exposedB = srcDir FilePath.</> "Two.elm"
      proj = Project.Project root root (NE.List exposedA [exposedB]) [srcDir] 1
  writeElmPackage root
  Dir.createDirectoryIfMissing True srcDir
  writeFile exposedA "module One exposing (value)\n\nvalue = 1\n"
  writeFile exposedB "module Two exposing (value)\n\nvalue = 2\n"
  groups <- Project.entrypointGroupsForChangedFiles [exposedA] proj
  pure (map NE.toList groups == [[exposedA, exposedB]])

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
      updated = CompileState.updateProjectFileInfo proj (Right ()) current latest
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
