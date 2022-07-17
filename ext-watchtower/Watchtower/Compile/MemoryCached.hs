{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Watchtower.Compile.MemoryCached (compileToJson) where

import Control.Concurrent.MVar
import System.IO.Unsafe (unsafePerformIO)
import Ext.Common
import Json.Encode ((==>))
import qualified BackgroundWriter as BW
import qualified Build
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BSL
import qualified Data.NonEmptyList as NE
import qualified Elm.Details as Details
import qualified Ext.Common
import qualified Ext.FileCache
import qualified Ext.MemoryCached.Build
import qualified Ext.MemoryCached.Details
import qualified Generate
import qualified Generate.Html as Html
import qualified Json.Encode as Encode
import qualified Reporting
import qualified Reporting.Exit as Exit
import qualified Reporting.Task as Task
import qualified Stuff
import qualified System.Directory as Dir
import StandaloneInstances


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
        -- @TODO root lock shouldn't be needed unless we're falling through to disk compile
        BW.withScope $ \scope -> Stuff.withRootLock root $ do
          Task.run $ do
            -- Task.io $ debug $ "🌳🌳🌳🌳🌳🌳🌳🌳🌳🌳" <> show paths_

            !_ <- Task.io $ Ext.MemoryCached.Details.bustDetailsCache
            !_ <- Task.io $ Ext.MemoryCached.Build.bustArtifactsCache

            details <- Task.eio Exit.ReactorBadDetails $ Ext.MemoryCached.Details.load Reporting.silent scope root
            artifacts <- Task.eio Exit.ReactorBadBuild $ Ext.MemoryCached.Build.fromPathsMemoryCached Reporting.silent root details paths

            -- Task.io $ debug $ "🟠🟠🟠🟠🟠🟠🟠"
            -- javascript <- Task.mapError Exit.ReactorBadGenerate $ Generate.dev root details artifacts
            -- let (NE.List name _) = Ext.MemoryCached.Build.getRootNames artifacts
            -- return $ Html.sandwich name javascript
            pure ()
