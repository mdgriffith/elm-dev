module Ext.Test.Compile
  ( compileTests
  , CompileOutput(..)
  ) where

import qualified BackgroundWriter
import qualified Control.Concurrent.MVar as MVar
import qualified Data.ByteString.Builder as BB
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.NonEmptyList as NE
import qualified Elm.Details as Details
import qualified Elm.Interface as I
import qualified Elm.ModuleName as ModuleName
import qualified Ext.CompileHelpers.Generic as CompileHelpers
import qualified Ext.MemoryCached.Details
import qualified Ext.MemoryCached.Build
import qualified Reporting
import qualified Reporting.Task
import qualified Reporting.Exit as Exit
import qualified System.Directory as Dir


data CompileOutput = CompileOutput
  { interfaces :: Map.Map ModuleName.Raw I.Interface
  , jsBundle :: BB.Builder
  }


compileTests :: FilePath -> [FilePath] -> IO (Either Exit.Reactor CompileOutput)
compileTests root testPaths = do
    BackgroundWriter.withScope $ \scope -> do
      Dir.withCurrentDirectory root $ do
        scopeResult <- Ext.MemoryCached.Details.load Reporting.silent scope root
        case scopeResult of
          Left err -> pure (Left (Exit.ReactorBadDetails err))
          Right details -> do
            let flags = CompileHelpers.compilationModsFromFlags CompileHelpers.Dev
            -- Build artifacts for provided test files
            artifactsResult <- Ext.MemoryCached.Build.fromPathsMemoryCached flags Reporting.silent root details (toNonEmpty testPaths)
            case artifactsResult of
              Left buildErr -> pure (Left (Exit.ReactorBadBuild buildErr))
              Right (artifacts, _fileInfo) -> do
                rawIfaces <- getInterfaces root details
                let ifaces = CompileHelpers.toInterfaces rawIfaces
                -- Compile a JS bundle from an entrypoint (the Runner will be provided by caller)
                compiled <- Reporting.Task.run $ CompileHelpers.generate root details CompileHelpers.Dev artifacts (CompileHelpers.OutputTo CompileHelpers.Js)
                case compiled of
                  Right (CompileHelpers.CompiledJs js) -> pure (Right (CompileOutput ifaces js))
                  Right _ -> pure (Right (CompileOutput ifaces mempty))
                  Left exit -> pure (Left exit)


toNonEmpty :: [a] -> NE.List a
toNonEmpty xs = case xs of
  [] -> error "toNonEmpty called with empty list"
  (y:ys) -> NE.List y ys


getInterfaces :: FilePath -> Details.Details -> IO (Map.Map ModuleName.Canonical I.DependencyInterface)
getInterfaces root details = do
  imvar <- Ext.MemoryCached.Details.loadInterfaces root details
  maybeInterfaces <- MVar.readMVar imvar
  case maybeInterfaces of
    Nothing -> pure Map.empty
    Just deps -> pure deps



