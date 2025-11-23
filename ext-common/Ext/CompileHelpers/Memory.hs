module Ext.CompileHelpers.Memory
  ( compile
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
import qualified Ext.MemoryCached.Build
import qualified Ext.MemoryCached.Details
import qualified Json.Encode as Encode
import qualified Reporting
import qualified Reporting.Exit as Exit
import qualified Reporting.Task as Task
import qualified System.Directory as Dir
import System.IO.Unsafe (unsafePerformIO)
import qualified Make
import qualified Data.Map as Map
import qualified Reporting.Warning as Warning
import qualified Elm.Docs
import qualified Reporting.Render.Type.Localizer
import qualified Watchtower.Live.Client
import qualified Control.Concurrent.STM as STM
import qualified Elm.Package


debug :: String -> IO ()
debug =
  Ext.Log.log Ext.Log.MemoryCache


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


-- Returns (either compile result or reactor error, and the per-file info map).
compile :: FilePath -> NE.List FilePath -> CompileHelpers.Flags -> Maybe (STM.TVar (Map.Map Elm.Package.Name Watchtower.Live.Client.PackageInfo)) -> IO (Either Exit.Reactor CompileHelpers.CompilationResult, Map.Map FilePath Watchtower.Live.Client.FileInfo)
compile root paths flags@(CompileHelpers.Flags mode output) packagesVar = do
    Dir.withCurrentDirectory root $
      BW.withScope $ \scope -> do
        -- Bust caches explicitly
        Ext.MemoryCached.Details.bustDetailsCache
        Ext.MemoryCached.Build.bustArtifactsCache
        let compilationFlags = CompileHelpers.compilationModsFromFlags mode
        -- Load details (IO Either)
        detailsEither <- Ext.MemoryCached.Details.load Reporting.silent scope root packagesVar
        case detailsEither of
          Left detailsErr ->
            pure (Left (Exit.ReactorBadDetails detailsErr), Map.empty)
          Right details -> do
            -- Build artifacts while collecting per-file info, even on failure
            (eitherArtifacts, fileInfoByPath) <- Ext.MemoryCached.Build.fromPathsMemoryCached packagesVar compilationFlags Reporting.silent root details paths
            case eitherArtifacts of
              Left buildProblem ->
                pure (Left (Exit.ReactorBadBuild buildProblem), fileInfoByPath)
              Right artifacts -> do
                compiledEither <- Task.run $ CompileHelpers.generate root details mode artifacts output
                case compiledEither of
                  Left reactorErr -> pure (Left reactorErr, fileInfoByPath)
                  Right compiled -> pure (Right compiled, fileInfoByPath)


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
  do  let style = Reporting.silent
      result <- Ext.MemoryCached.Details.load style scope root Nothing
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
