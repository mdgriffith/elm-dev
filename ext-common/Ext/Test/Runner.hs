{-# LANGUAGE OverloadedStrings #-}
module Ext.Test.Runner
  ( RunError(..)
  , TestSummary(..)
  , TestInfo(..)
  , RunSuccess(..)
  , matchesAnyGlob
  , run
  ) where

import qualified Control.Concurrent.MVar as MVar
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.NonEmptyList as NE
import qualified Data.Name as Name
import qualified BackgroundWriter
import qualified Elm.ModuleName as ModuleName
import qualified Ext.CompileMode
import qualified Ext.Test.Discover as Discover
import qualified Ext.Test.Introspect as Introspect
import qualified Ext.Test.Generate as Generate
import qualified Ext.Test.Compile as TestCompile
import qualified Reporting
import qualified Reporting.Exit as Exit
import qualified System.Directory as Dir
import qualified System.FilePath as FP
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.UTF8 as UTF8
import qualified Ext.Test.Templates.Loader as Templates
import qualified Ext.Log
import qualified Gen.Javascript as Javascript
import qualified Ext.CompileHelpers.Generic as CompileHelpers
import qualified System.CPUTime as CPUTime
import qualified Ext.Test.Result
import qualified System.Timeout as Timeout
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
  }

-- | Run tests. If a timeout (in seconds) is provided, enforce it; otherwise run without a timeout.
-- Optionally filter which tests run using glob patterns matched against
-- fully-qualified test IDs like \"Module.Submodule.testName\".
run :: Maybe Int -> Maybe [String] -> FilePath -> IO (Either RunError RunSuccess)
run mSeconds mGlobs root = do
  case mSeconds of
    Nothing -> do
      r <- runNode mGlobs root
      case r of
        Left e -> pure (Left (RunFailed e))
        Right (out, tests, testFiles) ->
          case Ext.Test.Result.decodeReportsString out of
            Left e -> pure (Left (RunFailed e))
            Right reports -> do
              let allResults = concatMap (concatMap Ext.Test.Result.testRunResult . Ext.Test.Result.reportRuns) reports
              let total = length allResults
              let passed = length [ () | res <- allResults, case res of { Ext.Test.Result.Passed -> True; _ -> False } ]
              let failed = length [ () | res <- allResults, case res of { Ext.Test.Result.Failed{} -> True; _ -> False } ]
              let failures' =
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
              let summary = TestSummary total passed failed failures'
              let info = TestInfo
                           { tiSuites = tests
                           , tiFiles = testFiles
                           , tiSummary = summary
                           }
              pure (Right (RunSuccess info reports))
    Just seconds -> do
      timed <- Timeout.timeout (seconds * 1000000) (runNode mGlobs root)
      case timed of
        Nothing ->
          pure (Left (TimedOut seconds))
        Just (Left e) ->
          pure (Left (RunFailed e))
        Just (Right (out, tests, testFiles)) ->
          case Ext.Test.Result.decodeReportsString out of
            Left e -> pure (Left (RunFailed e))
            Right reports -> do
              let allResults = concatMap (concatMap Ext.Test.Result.testRunResult . Ext.Test.Result.reportRuns) reports
              let total = length allResults
              let passed = length [ () | res <- allResults, case res of { Ext.Test.Result.Passed -> True; _ -> False } ]
              let failed = length [ () | res <- allResults, case res of { Ext.Test.Result.Failed{} -> True; _ -> False } ]
              let failures =
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
              pure (Right (RunSuccess info reports))


-- Returns (JSON test result string, discovered tests, discovered test files).
runNode :: Maybe [String] -> FilePath -> IO (Either String (String, [(ModuleName.Raw, Name.Name)], [FilePath]))
runNode mGlobs root = do
  BackgroundWriter.withScope $ \scope -> do
    Ext.CompileMode.setModeMemory
    testFiles <- Discover.discoverTestFiles root
    case testFiles of
      [] -> pure (Left $ "No .elm files found in " ++ (root FP.</> "tests"))
      (top:rest) -> do
        discoveryR <- TestCompile.compileForDiscovery root (NE.List top rest)
        case discoveryR of
          Left err -> pure (Left (Exit.toString (Exit.reactorToReport err)))
          Right ifaces -> do
            testsAll <- Introspect.findTests ifaces
            let toId (modName, valName) = ModuleName.toChars modName ++ "." ++ Name.toChars valName
            let tests =
                  case mGlobs of
                    Nothing -> testsAll
                    Just globs -> filter (\t -> matchesAnyGlob globs (toId t)) testsAll
            _ <- Generate.writeAggregator root tests
            let genDir = Generate.generatedDir root
            Dir.createDirectoryIfMissing True genDir
            let runnerPath = genDir `FP.combine` "Runner.elm"
            BS.writeFile runnerPath Templates.runnerElm
            compiledR <- TestCompile.compileRunner root (NE.List runnerPath [])
            case compiledR of
              Left err -> pure (Left (Exit.toString (Exit.reactorToReport err)))
              Right compiled -> do
                -- Build list of test IDs
                let testIds = fmap toId tests
                -- Extract JS bytes from compilation result
                compiledJs <- case compiled of
                  CompileHelpers.CompiledJs builder -> pure (BL.toStrict (BB.toLazyByteString builder))
                  CompileHelpers.CompiledHtml _ -> pure ""
                  CompileHelpers.CompiledSkippedOutput -> pure ""
                -- Embed compiled JS into node runner template
                let templateStr = UTF8.toString Templates.nodeRunnerJs
                let compiledStr = UTF8.toString compiledJs
                let finalJsStr = replaceOnce "/* {{COMPILED_ELM}} */" compiledStr templateStr
                let inputJson = makeInputJson 0 100 testIds
                
                execResult <- Javascript.run (UTF8.fromString finalJsStr) (UTF8.fromString inputJson)
                case execResult of
                  Left e -> pure (Left e)
                  Right out -> pure (Right (out, tests, testFiles))

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


-- | Replace first occurrence of a substring with a replacement
replaceOnce :: String -> String -> String -> String
replaceOnce needle replacement haystack =
  case breakOn needle haystack of
    Nothing -> haystack
    Just (before, after) -> before ++ replacement ++ drop (length needle) after

breakOn :: String -> String -> Maybe (String, String)
breakOn needle haystack = go "" haystack
  where
    go _acc [] = Nothing
    go acc rest =
      if needle `isPrefixOf` rest
        then Just (reverse acc, rest)
        else go (head rest : acc) (tail rest)

isPrefixOf :: String -> String -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

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


 
 
 


 



