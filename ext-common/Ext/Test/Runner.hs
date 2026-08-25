{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Ext.Test.Runner
  ( RunError(..)
  , TestSummary(..)
  , TestInfo(..)
  , RunSuccess(..)
  , IsolatedSuccess(..)
  , matchesAnyGlob
  , run
  , runIsolated
  , runIsolatedWith
  , workerMain
  ) where

import qualified Control.Concurrent as Concurrent
import qualified Control.Exception as Exception
import qualified Data.Aeson as JSON
import Data.Aeson ((.:), (.:?), (.=))
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as List
import qualified Data.NonEmptyList as NE
import qualified Data.Name as Name
import qualified BackgroundWriter
import qualified Elm.ModuleName as ModuleName
import qualified Ext.CompileMode
import qualified Ext.FileCache as FileCache
import qualified Ext.Test.Discover as Discover
import qualified Ext.Test.Parse as Parse
import qualified Ext.Test.Generate as Generate
import qualified Ext.Test.Compile as TestCompile
import qualified Ext.Test.Result.Report as TestReport
import qualified System.Directory as Dir
import qualified System.Environment as Env
import qualified System.Exit as Exit
import qualified System.FilePath as FP
import qualified System.IO as IO
import qualified System.IO.Temp as Temp
import qualified System.Process as Process
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS.Char8
import qualified Data.ByteString.UTF8 as UTF8
import qualified Ext.Test.Templates.Loader as Templates
import qualified Ext.Log
import qualified Gen.Javascript as Javascript
import qualified Ext.Test.Result
import qualified System.Timeout as Timeout
import qualified Data.Time.Clock as Time
import System.Random (getStdRandom, random)
#ifndef mingw32_HOST_OS
import qualified System.Posix.Signals as Signals
#endif
-- NOTE: Keep this module independent of Watchtower to avoid cycles.


-- | Failure cases for running tests
data RunError
  = RunFailed String
  | TimedOut Int -- ^ seconds waited

data TestSummary = TestSummary
  { summaryTotal :: Int
  , summaryPassed :: Int
  , summaryFailed :: Int
  , summaryFailures :: [String]
  }

data TestInfo = TestInfo
  { tiSuites :: [(ModuleName.Raw, Name.Name)]
  , tiFiles :: [FilePath]
  , tiSummary :: TestSummary
  }

data RunSuccess = RunSuccess
  { info :: TestInfo
  , reports :: [Ext.Test.Result.Report]
  , seed :: Int
  , fuzz :: Int
  }


data IsolatedSuccess = IsolatedSuccess
  { isolatedRendered :: String
  , isolatedTestFiles :: [FilePath]
  , isolatedSummary :: TestSummary
  , isolatedSeed :: Int
  , isolatedFuzz :: Int
  , isolatedDurationMs :: Int
  }


data WorkerRequest = WorkerRequest
  { requestTimeoutSeconds :: Maybe Int
  , requestGlobs :: Maybe [String]
  , requestSeed :: Maybe Int
  , requestFuzz :: Maybe Int
  , requestRoot :: FilePath
  }


data WorkerResponse
  = WorkerSucceeded IsolatedSuccess
  | WorkerFailed String (Maybe Int)


instance JSON.ToJSON WorkerRequest where
  toJSON request = JSON.object
    [ "timeoutSeconds" .= requestTimeoutSeconds request
    , "globs" .= requestGlobs request
    , "seed" .= requestSeed request
    , "fuzz" .= requestFuzz request
    , "root" .= requestRoot request
    ]


instance JSON.FromJSON WorkerRequest where
  parseJSON = JSON.withObject "WorkerRequest" $ \obj ->
    WorkerRequest
      <$> obj .:? "timeoutSeconds"
      <*> obj .:? "globs"
      <*> obj .:? "seed"
      <*> obj .:? "fuzz"
      <*> obj .: "root"


instance JSON.ToJSON WorkerResponse where
  toJSON response =
    case response of
      WorkerSucceeded success -> JSON.object
        [ "status" .= ("ok" :: String)
        , "rendered" .= isolatedRendered success
        , "testFiles" .= isolatedTestFiles success
        , "total" .= summaryTotal (isolatedSummary success)
        , "passed" .= summaryPassed (isolatedSummary success)
        , "failed" .= summaryFailed (isolatedSummary success)
        , "failures" .= summaryFailures (isolatedSummary success)
        , "seed" .= isolatedSeed success
        , "fuzz" .= isolatedFuzz success
        , "durationMs" .= isolatedDurationMs success
        ]
      WorkerFailed message timeoutSeconds -> JSON.object
        [ "status" .= ("error" :: String)
        , "message" .= message
        , "timeoutSeconds" .= timeoutSeconds
        ]


instance JSON.FromJSON WorkerResponse where
  parseJSON = JSON.withObject "WorkerResponse" $ \obj -> do
    status <- obj .: "status"
    case (status :: String) of
      "ok" -> do
        summary <- TestSummary
          <$> obj .: "total"
          <*> obj .: "passed"
          <*> obj .: "failed"
          <*> obj .: "failures"
        WorkerSucceeded <$> (IsolatedSuccess
          <$> obj .: "rendered"
          <*> obj .: "testFiles"
          <*> pure summary
          <*> obj .: "seed"
          <*> obj .: "fuzz"
          <*> obj .: "durationMs")
      "error" -> WorkerFailed <$> obj .: "message" <*> obj .:? "timeoutSeconds"
      _ -> fail ("Unknown worker response status: " ++ status)

-- | Run tests. If a timeout (in seconds) is provided, enforce it; otherwise run without a timeout.
-- Optionally filter which tests run using glob patterns matched against
-- fully-qualified test IDs like \"Module.Submodule.testName\".
-- Optionally provide seed and fuzz (runs) values; if not provided, seed is randomly generated and fuzz defaults to 100.
run :: Maybe Int -> Maybe [String] -> Maybe Int -> Maybe Int -> FilePath -> IO (Either RunError RunSuccess)
run mSeconds mGlobs mSeed mFuzz root = do
  -- Generate seed if not provided
  actualSeed <- case mSeed of
    Just s -> pure s
    Nothing -> do
      s <- getStdRandom random
      pure (abs s)
  -- Use fuzz value or default to 100
  let actualFuzz = maybe 100 id mFuzz
  -- Optionally apply timeout to runNode
  let runWithTimeout root' = case mSeconds of
        Nothing -> fmap Just (runNode mGlobs actualSeed actualFuzz root')
        Just seconds -> Timeout.timeout (seconds * 1000000) (runNode mGlobs actualSeed actualFuzz root')
  let timeoutSeconds = maybe 0 id mSeconds
  r <- runWithTimeout root
  case r of
    Nothing ->
      pure (Left (TimedOut timeoutSeconds))
    Just (Left e) -> case e of
      Javascript.ThreadKilled ->
        pure (Left (TimedOut timeoutSeconds))
      Javascript.Other msg ->
        pure (Left (RunFailed msg))
    Just (Right (out, tests, testFiles)) ->
      case Ext.Test.Result.decodeReportsStrict out of
        Left e -> pure (Left (RunFailed e))
        Right reports -> do
          let allResults = concatMap (concatMap Ext.Test.Result.testRunResult . Ext.Test.Result.reportRuns) reports
          let total = length allResults
          let passed = length [ () | res <- allResults, case res of { Ext.Test.Result.Passed -> True; _ -> False } ]
          let failed = length [ () | res <- allResults, case res of { Ext.Test.Result.Failed{} -> True; _ -> False } ]
          -- Check for empty test results
          if total == 0 && not (null tests)
            then pure (Left (RunFailed "No test results returned (tests may have timed out or failed to execute)"))
            else do
              let failures =
                    take 100 $ map (take 4096) $
                    concatMap
                      (\rep ->
                         concatMap
                           (\run ->
                              let labelPath = List.intercalate " › " (Ext.Test.Result.testRunLabel run)
                              in [ labelPath ++ " - " ++ msg
                                 | res <- Ext.Test.Result.testRunResult run
                                 , case res of { Ext.Test.Result.Failed{} -> True; _ -> False }
                                 , let msg = case res of { Ext.Test.Result.Failed{ Ext.Test.Result.failureMessage = m } -> m; _ -> "" }
                                 ]
                           )
                           (Ext.Test.Result.reportRuns rep)
                      )
                      reports
              let summary = TestSummary total passed failed failures
              let info = TestInfo
                           { tiSuites = tests
                           , tiFiles = testFiles
                           , tiSummary = summary
                           }
              pure (Right (RunSuccess info reports actualSeed actualFuzz))


runIsolated :: Maybe Int -> Maybe [String] -> Maybe Int -> Maybe Int -> FilePath -> IO (Either RunError IsolatedSuccess)
runIsolated mSeconds mGlobs mSeed mFuzz root = do
  executable <- Env.getExecutablePath
  runIsolatedWith executable mSeconds mGlobs mSeed mFuzz root


runIsolatedWith :: FilePath -> Maybe Int -> Maybe [String] -> Maybe Int -> Maybe Int -> FilePath -> IO (Either RunError IsolatedSuccess)
runIsolatedWith executable mSeconds mGlobs mSeed mFuzz root =
  Temp.withSystemTempDirectory "elm-dev-test-worker" $ \tempDir -> do
    environment <- Env.getEnvironment
    let requestPath = tempDir FP.</> "request.json"
        responsePath = tempDir FP.</> "response.json"
        stdoutPath = tempDir FP.</> "stdout"
        stderrPath = tempDir FP.</> "stderr"
        request = WorkerRequest mSeconds mGlobs mSeed mFuzz root
        workerEnvironment =
          setEnvironmentVariable "GHCRTS" "-M1024m -N2" $
            setEnvironmentVariable "NODE_OPTIONS" "--max-old-space-size=768" environment
    LBS.writeFile requestPath (JSON.encode request)
    exitCode <-
      IO.withBinaryFile stdoutPath IO.WriteMode $ \stdoutHandle ->
        IO.withBinaryFile stderrPath IO.WriteMode $ \stderrHandle ->
          Process.withCreateProcess
            ((Process.proc executable ["__elm-dev-test-worker", requestPath, responsePath])
              { Process.std_in = Process.NoStream
              , Process.std_out = Process.UseHandle stdoutHandle
              , Process.std_err = Process.UseHandle stderrHandle
              , Process.create_group = True
              , Process.env = Just workerEnvironment
              })
            (\_ _ _ processHandle -> waitForProcessGroup processHandle)
    case exitCode of
      Exit.ExitSuccess -> decodeWorkerResponse responsePath
      Exit.ExitFailure code -> do
        stdout <- readIfPresent stdoutPath
        stderr <- readIfPresent stderrPath
        pure (Left (RunFailed ("Test worker exited with code " ++ show code ++ processDetails stdout stderr)))


workerMain :: FilePath -> FilePath -> IO ()
workerMain requestPath responsePath = do
  requestBytes <- LBS.readFile requestPath
  response <-
    case JSON.eitherDecode requestBytes of
      Left message -> pure (WorkerFailed ("Invalid test worker request: " ++ message) Nothing)
      Right request -> runWorkerRequest request
  LBS.writeFile responsePath (JSON.encode response)


runWorkerRequest :: WorkerRequest -> IO WorkerResponse
runWorkerRequest request = do
  startedAt <- Time.getCurrentTime
  result <- run
    (requestTimeoutSeconds request)
    (requestGlobs request)
    (requestSeed request)
    (requestFuzz request)
    (requestRoot request)
  finishedAt <- Time.getCurrentTime
  let durationMs = round (realToFrac (Time.diffUTCTime finishedAt startedAt) * 1000 :: Double)
  pure $ case result of
    Left (TimedOut seconds) -> WorkerFailed ("Tests timed out after " ++ show (seconds * 1000) ++ "ms") (Just seconds)
    Left (RunFailed message) -> WorkerFailed (boundWorkerText message) Nothing
    Right success ->
      let rendered = TestReport.renderReportsWithDuration False (Just durationMs) (Just (seed success)) (Just (fuzz success)) (reports success)
      in WorkerSucceeded IsolatedSuccess
        { isolatedRendered = boundWorkerText rendered
        , isolatedTestFiles = tiFiles (info success)
        , isolatedSummary = tiSummary (info success)
        , isolatedSeed = seed success
        , isolatedFuzz = fuzz success
        , isolatedDurationMs = durationMs
        }


boundWorkerText :: String -> String
boundWorkerText value
  | length value <= maxWorkerOutputChars = value
  | otherwise = take maxWorkerOutputChars value ++ "\n\n[Output truncated by elm-dev test worker.]"


maxWorkerOutputChars :: Int
maxWorkerOutputChars = 256 * 1024


decodeWorkerResponse :: FilePath -> IO (Either RunError IsolatedSuccess)
decodeWorkerResponse responsePath = do
  exists <- Dir.doesFileExist responsePath
  if not exists
    then pure (Left (RunFailed "Test worker exited without producing a result."))
    else do
      bytes <- LBS.readFile responsePath
      pure $ case JSON.eitherDecode bytes of
        Left message -> Left (RunFailed ("Invalid test worker response: " ++ message))
        Right (WorkerSucceeded success) -> Right success
        Right (WorkerFailed message maybeTimeout) ->
          case maybeTimeout of
            Just seconds -> Left (TimedOut seconds)
            Nothing -> Left (RunFailed message)


waitForProcessGroup :: Process.ProcessHandle -> IO Exit.ExitCode
waitForProcessGroup processHandle =
  wait `Exception.onException` stopProcessGroup processHandle
  where
    wait = do
      maybeExit <- Process.getProcessExitCode processHandle
      case maybeExit of
        Just exitCode -> pure exitCode
        Nothing -> Concurrent.threadDelay 10000 >> wait


stopProcessGroup :: Process.ProcessHandle -> IO ()
stopProcessGroup processHandle = do
#ifndef mingw32_HOST_OS
  maybePid <- Process.getPid processHandle
#endif
  _ <- Exception.try (Process.interruptProcessGroupOf processHandle) :: IO (Either Exception.SomeException ())
  Concurrent.threadDelay 100000
#ifdef mingw32_HOST_OS
  maybeExit <- Process.getProcessExitCode processHandle
  case maybeExit of
    Just _ -> pure ()
    Nothing -> Process.terminateProcess processHandle
#else
  case maybePid of
    Nothing -> do
      maybeExit <- Process.getProcessExitCode processHandle
      case maybeExit of
        Just _ -> pure ()
        Nothing -> Process.terminateProcess processHandle
    Just pid -> do
      _ <- Exception.try (Signals.signalProcessGroup Signals.sigKILL (fromIntegral pid)) :: IO (Either Exception.SomeException ())
      pure ()
#endif
  _ <- Exception.try (Process.waitForProcess processHandle) :: IO (Either Exception.SomeException Exit.ExitCode)
  pure ()


setEnvironmentVariable :: String -> String -> [(String, String)] -> [(String, String)]
setEnvironmentVariable name value environment =
  (name, value) : filter ((/= name) . fst) environment


readIfPresent :: FilePath -> IO String
readIfPresent path = do
  exists <- Dir.doesFileExist path
  if exists
    then IO.withBinaryFile path IO.ReadMode $ \handle ->
      BS.Char8.unpack <$> BS.hGet handle maxWorkerOutputChars
    else pure ""


processDetails :: String -> String -> String
processDetails stdout stderr
  | not (null stderr) = ":\n" ++ stderr
  | not (null stdout) = ":\n" ++ stdout
  | otherwise = ""


-- Returns (JSON test result bytes, discovered tests, discovered test files).
runNode :: Maybe [String] -> Int -> Int -> FilePath -> IO (Either Javascript.RunError (BS.ByteString, [(ModuleName.Raw, Name.Name)], [FilePath]))
runNode mGlobs seed runs root = do
  BackgroundWriter.withScope $ \scope -> do
    Ext.CompileMode.setModeMemory
    testFiles <- Discover.discoverTestFiles root
    case testFiles of
      [] -> pure (Left $ Javascript.Other $ "No .elm files found in " ++ (root FP.</> "tests"))
      _ -> do
        -- Parse all test files to find potential test values
        parsedModules <- mapM (\filePath -> do
          content <- BS.readFile filePath
          case Parse.parseModule filePath content of
            Left err -> pure (Left err)
            Right parsed -> pure (Right (filePath, parsed))
          ) testFiles
        
        -- Collect all parsing errors
        let parseErrors = [err | Left err <- parsedModules]
        if not (null parseErrors)
          then pure (Left $ Javascript.Other $ "Failed to parse test files: " ++ List.intercalate "; " parseErrors)
          else do
            let parsed = [(fp, pm) | Right (fp, pm) <- parsedModules]
            
            -- Extract all potential test values from parsed modules
            let allPotentialTests = concatMap (Parse.potentialTestValues . snd) parsed
            
            let toId (modName, valName) = ModuleName.toChars modName ++ "." ++ Name.toChars valName
            
            -- Filter by globs if provided
            let filteredTests = case mGlobs of
                  Nothing -> allPotentialTests
                  Just globs -> filter (\t -> matchesAnyGlob globs (toId t)) allPotentialTests
            
            -- Check if no tests found
            case filteredTests of
              [] -> do
                let errorMsg = case mGlobs of
                      Just [single] -> "No tests matching glob pattern: " ++ single
                      Just globs -> "No tests matching glob patterns: " ++ List.intercalate ", " globs
                      Nothing -> "No tests found"
                pure (Left $ Javascript.Other errorMsg)
              _ -> do
                Ext.Log.log Ext.Log.Test ("Found " <> show filteredTests)
                _ <- Generate.writeAggregator root filteredTests
                let genDir = Generate.generatedDir root
                Dir.createDirectoryIfMissing True genDir
                let runnerPath = genDir `FP.combine` "Runner.elm"
                FileCache.writeUtf8AllTheWayToDisk runnerPath Templates.runnerElm
                compiledR <- TestCompile.compileRunner root (NE.List runnerPath [])
                case compiledR of
                  Left err -> pure (Left (Javascript.Other err))
                  Right compiledJs -> do
                    -- Build list of test IDs
                    let testIds = fmap toId filteredTests
                    let finalJs = replaceOnceBytes "/* {{COMPILED_ELM}} */" compiledJs Templates.nodeRunnerJs
                        inputJson = makeInputJson seed runs testIds

                    execResult <- Javascript.runBytes finalJs (UTF8.fromString inputJson)
                    case execResult of
                      Left e -> pure (Left e)
                      Right out -> pure (Right (out, filteredTests, testFiles))

-- | Check whether a target string matches any of the provided glob patterns.
--
-- Patterns support:
--   - '*' matching zero or more characters
--   - '?' matching exactly one character
--
-- Examples:
--
-- >>> matchesAnyGlob [\"My.Module.*\"] \"My.Module.tests\"
-- True
--
-- >>> matchesAnyGlob [\"My.Module.*\"] \"Other.Module.tests\"
-- False
--
-- >>> matchesAnyGlob [\"Tests.*.foo\", \"*Bar\"] \"Tests.One.foo\"
-- True
--
-- >>> matchesAnyGlob [\"*\"] \"Anything.Goes\"
-- True
--
-- >>> matchesAnyGlob [] \"Anything.Goes\"
-- False
matchesAnyGlob :: [String] -> String -> Bool
matchesAnyGlob globs target = any (\g -> globMatch g target) globs

-- Internal: glob matching for '*' and '?'
globMatch :: String -> String -> Bool
globMatch [] [] = True
globMatch [] (_:_) = False
globMatch ('*':ps) s = globMatch ps s || (not (null s) && globMatch ('*':ps) (tail s))
globMatch ('?':ps) (_:s) = globMatch ps s
globMatch (p:ps) (c:cs) = p == c && globMatch ps cs
globMatch _ _ = False


replaceOnceBytes :: BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString
replaceOnceBytes needle replacement haystack =
  let (before, rest) = BS.breakSubstring needle haystack
  in if BS.null rest
       then haystack
       else before <> replacement <> BS.drop (BS.length needle) rest

-- | Build minimal JSON for the node runner
makeInputJson :: Int -> Int -> [String] -> String
makeInputJson seed runs ids =
  let quote s = '"' : escape s ++ "\""
      escape = concatMap esc
      esc c = case c of
        '"' -> "\\\""
        '\\' -> "\\\\"
        '\n' -> "\\n"
        '\r' -> "\\r"
        '\t' -> "\\t"
        _ -> [c]
      idsJson = "[" ++ List.intercalate "," (map quote ids) ++ "]"
  in
  "{\"flags\":{\"seed\":" ++ show seed ++ ",\"runs\":" ++ show runs ++ "},\"tests\":" ++ idsJson ++ "}"


 
 
 


 
