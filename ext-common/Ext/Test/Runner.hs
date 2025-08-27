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
import qualified Ext.MemoryCached.Details
import qualified Ext.MemoryCached.Build
import qualified Ext.CompileHelpers.Generic as CompileHelpers
import qualified Reporting
import qualified Reporting.Exit as Exit
import qualified System.Directory as Dir



-- | Orchestrates discovery, aggregator generation, compilation. Returns JSON test result as a String.
run :: FilePath -> IO (Either String String)
run root = do
  BackgroundWriter.withScope $ \scope -> do
    Ext.CompileMode.setModeMemory
    testFiles <- Discover.discoverTestFiles root
    -- Load details and build artifacts for tests to extract interfaces
    detailsR <- Ext.MemoryCached.Details.load Reporting.silent scope root
    case detailsR of
      Left err -> pure (Left (show err))
      Right details -> do
        artifactsR <- Ext.MemoryCached.Build.fromPathsMemoryCached (CompileHelpers.compilationModsFromFlags CompileHelpers.Dev) Reporting.silent root details (toNonEmpty testFiles)
        case artifactsR of
          Left buildErr -> pure (Left (show buildErr))
          Right (artifacts, _fileInfo) -> do
            -- Extract public interfaces for local modules
            deps <- Ext.MemoryCached.Details.loadInterfaces root details
            depsM <- MVar.readMVar deps
            case depsM of
              Nothing -> pure (Left "No interfaces available")
              Just depIfaces -> do
                let ifaces = CompileHelpers.toInterfaces depIfaces
                let tests = Introspect.findTests ifaces
                _ <- Generate.writeAggregator root tests
                -- TODO: compile Runner.elm and execute with JS harness, parse JSON
                pure (Right "{\"total\":0,\"passed\":0,\"failed\":0,\"failures\":[]}")


toNonEmpty :: [a] -> NE.List a
toNonEmpty xs = case xs of
  [] -> error "toNonEmpty called with empty list"
  (y:ys) -> NE.List y ys



