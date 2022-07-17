module Ext.CompileHelpers.Memory where


import Control.Concurrent.MVar
import Control.Monad (liftM2, unless)
import Ext.Common
import qualified BackgroundWriter as BW
import qualified Ext.CompileHelpers.Generic as CompileHelpers
import qualified Ext.MemoryCached.Details as Details
import qualified Reporting
import System.IO.Unsafe (unsafePerformIO)


{-# NOINLINE artifactsCache #-}
artifactsCache :: MVar CompileHelpers.Artifacts
artifactsCache = unsafePerformIO $ newEmptyMVar


allDepArtifacts :: IO CompileHelpers.Artifacts
allDepArtifacts = do
  artifactsCacheM <- tryReadMVar artifactsCache
  case artifactsCacheM of
    Just artifacts -> do
      debug $ "🎯 allDepArtifacts cache hit"
      pure artifacts
    Nothing -> do
      debug $ "❌ allDepArtifacts cache miss"
      artifacts <- allDepArtifacts_
      placed <- tryPutMVar artifactsCache artifacts
      if placed
        then pure ()
        else modifyMVar_ artifactsCache $ (\_ -> pure artifacts)
      pure artifacts



{- Appropriated from worker/src/Artifacts.hs
   WARNING: does not load any user code!!!
-}
allDepArtifacts_ :: IO CompileHelpers.Artifacts
allDepArtifacts_ =
  BW.withScope $ \scope ->
  do  --debug "Loading allDeps"
      let style = Reporting.silent
      root <- getProjectRoot
      result <- Details.load style scope root
      case result of
        Left _ ->
          error $ "Ran into some problem loading elm.json\nTry running `elm make` in: " ++ root

        Right details ->
          do  omvar <- Details.loadObjects root details
              imvar <- Details.loadInterfaces root details
              mdeps <- readMVar imvar
              mobjs <- readMVar omvar
              case liftM2 (,) mdeps mobjs of
                Nothing ->
                  error $ "Ran into some weird problem loading elm.json\nTry running `elm make` in: " ++ root

                Just (deps, objs) ->
                  return $ CompileHelpers.Artifacts (CompileHelpers.toInterfaces deps) objs
