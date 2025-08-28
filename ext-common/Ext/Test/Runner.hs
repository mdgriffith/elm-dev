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
import qualified Ext.Test.Templates.Loader as Templates
import qualified Ext.Log


-- | Orchestrates discovery, aggregator generation, compilation. Returns JSON test result as a String.
run :: FilePath -> IO (Either String String)
run root = do
  BackgroundWriter.withScope $ \scope -> do
    Ext.CompileMode.setModeMemory
    testFiles <- Discover.discoverTestFiles root
    Ext.Log.log Ext.Log.Test ("Found " <> show testFiles)
    discoveryR <- TestCompile.compileForDiscovery root testFiles
    case discoveryR of
      Left err -> pure (Left (show err))
      Right ifaces -> do
        let tests = Introspect.findTests ifaces
        _ <- Generate.writeAggregator root tests
        let genDir = Generate.generatedDir root
        Dir.createDirectoryIfMissing True genDir
        let runnerPath = genDir `FP.combine` "Runner.elm"
        BS.writeFile runnerPath Templates.runnerElm
        compiledR <- TestCompile.compileRunner root [runnerPath]
        case compiledR of
          Left err -> pure (Left (show err))
          Right _ -> pure (Right "{\"total\":0,\"passed\":0,\"failed\":0,\"failures\":[]}")


toNonEmpty :: [a] -> NE.List a
toNonEmpty xs = case xs of
  [] -> error "toNonEmpty called with empty list"
  (y:ys) -> NE.List y ys



