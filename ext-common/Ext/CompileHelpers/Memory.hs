module Ext.CompileHelpers.Memory
  ( compile
  , compileToJson
  , allPackageArtifacts
  )
where

import Control.Concurrent.MVar
import Control.Monad (liftM2, unless)
import Ext.Common
import qualified Ext.Log
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
import qualified Make


debug =
  Ext.Log.log Ext.Log.MemoryCache


{-|
This should be replaced with a compile call which takes the proper flags like `output`, which would skip generating stuff.

-}
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
  Ext.FileCache.handleIfChanged (NE.toList paths) (compileWithoutJsGen_ root)

compileWithoutJsGen_ :: FilePath -> [FilePath] -> IO (Either Exit.Reactor ())
compileWithoutJsGen_ root paths_ = do
  case paths_ of
    [] -> do
      atomicPutStrLn "🙈 compile avoided - no paths given"
      pure $ Right ()
    x:xs -> do
      let paths = NE.List x xs
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


            return ()
            



-- {-# NOINLINE compileCache #-}
-- compileCache :: MVar (Maybe (Either Exit.Reactor ()))
-- compileCache = unsafePerformIO $ newMVar Nothing


-- compile :: FilePath -> NE.List FilePath -> Make.Flags -> IO (Either Exit.Reactor B.Builder)
-- compile root paths_ flags = do
--     -- BUGBUG:Note, we aren't using the above cache here
--     -- Though we could.  We're recompiling every time in this case.
--     -- But we'd need to key the above cache by the root + entrypoint
--     modifyMVar compileCache (\_ -> do
--         compileR <- compile_ root paths_ flags
--         pure (Just compileR, compileR)
--       )


compile :: FilePath -> NE.List FilePath -> Make.Flags -> IO (Either Exit.Reactor B.Builder)
compile root paths (Make.Flags debug optimize maybeOutput _ maybeDocs) = do
    let desiredMode = CompileHelpers.getMode debug optimize
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

          -- let (NE.List name _) = Ext.MemoryCached.Build.getRootNames artifacts
          CompileHelpers.generate root details desiredMode artifacts
          

{-# NOINLINE artifactsCache #-}
artifactsCache :: MVar CompileHelpers.Artifacts
artifactsCache = unsafePerformIO $ newEmptyMVar


allPackageArtifacts :: FilePath -> IO CompileHelpers.Artifacts
allPackageArtifacts root = do
  artifactsCacheM <- tryReadMVar artifactsCache
  case artifactsCacheM of
    Just artifacts -> do
      debug $ "🎯 allPackageArtifacts cache hit"
      pure artifacts
    Nothing -> do
      debug $ "❌ allPackageArtifacts cache miss"
      artifacts <- allPackageArtifacts_ root
      placed <- tryPutMVar artifactsCache artifacts
      if placed
        then pure ()
        else modifyMVar_ artifactsCache $ (\_ -> pure artifacts)
      pure artifacts



{- Appropriated from worker/src/Artifacts.hs
   WARNING: does not load any user code!!!
-}
allPackageArtifacts_ :: FilePath -> IO CompileHelpers.Artifacts
allPackageArtifacts_ root =
  BW.withScope $ \scope ->
  do  --debug "Loading allDeps"
      let style = Reporting.silent
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
