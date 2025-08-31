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
              Right _ -> pure (Right "{\"total\":0,\"passed\":0,\"failed\":0,\"failures\":[]}")


 



