module Ext.CompileHelpers.Memory
  ( compileToJson
  , allDepArtifacts
  )
where

import Control.Concurrent.MVar
import Control.Monad (liftM2, unless)
import Ext.Common
import Json.Encode ((==>))
import qualified BackgroundWriter as BW
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BSL
import qualified Data.NonEmptyList as NE
import qualified Ext.CompileHelpers.Generic as CompileHelpers
import qualified Ext.FileCache
import qualified Ext.MemoryCached.Build
import qualified Ext.MemoryCached.Details
import qualified Json.Encode as Encode
import qualified Reporting
import qualified Reporting.Exit as Exit
import qualified Reporting.Task as Task
import qualified System.Directory as Dir
import System.IO.Unsafe (unsafePerformIO)


compileToJson :: FilePath -> NE.List FilePath -> IO (Either Encode.Value Encode.Value)
compileToJson root paths = do
  let toBS = BSL.toStrict . B.toLazyByteString
  result <- compileWithoutJsGen root paths
  pure $
    case result of
      Right builder ->
        Right $ Encode.object [ "compiled" ==> Encode.bool True ]
      Left exit -> do
        Left $ Exit.toJson $ Exit.reactorToReport exit


compileWithoutJsGen :: FilePath -> NE.List FilePath -> IO (Either Exit.Reactor ())
compileWithoutJsGen root paths = do
  Ext.FileCache.handleIfChanged (NE.toList paths) (compile root)


{-# NOINLINE compileCache #-}
compileCache :: MVar (Maybe (Either Exit.Reactor ()))
compileCache = unsafePerformIO $ newMVar Nothing


compile :: FilePath -> [FilePath] -> IO (Either Exit.Reactor ())
compile root paths_ = do
  case paths_ of
    [] -> do
      compileCacheM <- readMVar compileCache
      case compileCacheM of
        Just compile -> do
          debug $ "🎯 compile cache hit"
          pure compile
        Nothing -> do
          debug $ "❌ compile cache miss"
          modifyMVar compileCache (\_ -> do
              compileR <- compile_ root paths_
              pure (Just compileR, compileR)
            )

    x:xs -> do
      debug $ "❌ compile cache rebuild"
      modifyMVar compileCache (\_ -> do
          compileR <- compile_ root paths_
          pure (Just compileR, compileR)
        )


compile_ :: FilePath -> [FilePath] -> IO (Either Exit.Reactor ())
compile_ root paths_ = do
  case paths_ of
    [] -> do
      atomicPutStrLn "🙈 compile avoided - no paths given"
      pure $ Right ()
    x:xs -> do
      let paths = NE.List x xs
      Ext.Common.debug $ "🚀 compiling " ++ show root ++ " -> " ++ show paths
      Dir.withCurrentDirectory root $
        BW.withScope $ \scope ->
          -- @TODO root lock shouldn't be needed unless we're falling through to disk compile
          -- Stuff.withRootLock root $ do
          Task.run $ do
            -- Task.io $ debug $ "🌳🌳🌳🌳🌳🌳🌳🌳🌳🌳" <> show paths_

            Task.io $ Ext.MemoryCached.Details.bustDetailsCache
            Task.io $ Ext.MemoryCached.Build.bustArtifactsCache

            details <- Task.eio Exit.ReactorBadDetails $ Ext.MemoryCached.Details.load Reporting.silent scope root
            artifacts <- Task.eio Exit.ReactorBadBuild $ Ext.MemoryCached.Build.fromPathsMemoryCached Reporting.silent root details paths

            -- Task.io $ debug $ "🟠🟠🟠🟠🟠🟠🟠"
            -- javascript <- Task.mapError Exit.ReactorBadGenerate $ Generate.dev root details artifacts
            -- let (NE.List name _) = Ext.MemoryCached.Build.getRootNames artifacts
            -- return $ Html.sandwich name javascript
            pure ()



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
      result <- Ext.MemoryCached.Details.load style scope root
      case result of
        Left _ ->
          error $ "Ran into some problem loading elm.json\nTry running `elm make` in: " ++ root

        Right details ->
          do  omvar <- Ext.MemoryCached.Details.loadObjects root details
              imvar <- Ext.MemoryCached.Details.loadInterfaces root details
              mdeps <- readMVar imvar
              mobjs <- readMVar omvar
              case liftM2 (,) mdeps mobjs of
                Nothing ->
                  error $ "Ran into some weird problem loading elm.json\nTry running `elm make` in: " ++ root

                Just (deps, objs) ->
                  return $ CompileHelpers.Artifacts (CompileHelpers.toInterfaces deps) objs
