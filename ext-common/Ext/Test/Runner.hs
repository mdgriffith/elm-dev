{-# LANGUAGE OverloadedStrings #-}
module Ext.Test.Runner
  ( run
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


-- | Same as 'run' but decodes the raw JSON stdout into Elm-mirrored Haskell types.
run :: FilePath -> IO (Either String [Ext.Test.Result.Report])
run root = do
  r <- runNode root
  case r of
    Left e -> pure (Left e)
    Right s -> pure (Ext.Test.Result.decodeReportsString s)


-- | Orchestrates discovery, aggregator generation, compilation. Returns JSON test result as a String.
runNode :: FilePath -> IO (Either String String)
runNode root = do
  BackgroundWriter.withScope $ \scope -> do
    Ext.CompileMode.setModeMemory
    testFiles <- Discover.discoverTestFiles root
    Ext.Log.log Ext.Log.Test ("Found " <> show testFiles)
    case testFiles of
      [] -> pure (Left "Need at least one file")
      (top:rest) -> do
        discoveryR <- TestCompile.compileForDiscovery root (NE.List top rest)
        case discoveryR of
          Left err -> pure (Left (Exit.toString (Exit.reactorToReport err)))
          Right ifaces -> do
            tests <- Introspect.findTests ifaces
            Ext.Log.log Ext.Log.Test ("Tests " <> show tests)
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
                let testIds = fmap (\(modName, valName) -> ModuleName.toChars modName ++ "." ++ Name.toChars valName) tests
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
                  Right out -> pure (Right out)




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


 



