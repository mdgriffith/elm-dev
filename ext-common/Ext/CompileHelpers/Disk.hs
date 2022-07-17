module Ext.CompileHelpers.Disk
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
import qualified Build
import qualified Elm.Details as Details
import qualified Json.Encode as Encode
import qualified Reporting
import qualified Reporting.Exit as Exit
import qualified Reporting.Task as Task
import qualified System.Directory as Dir
import System.IO.Unsafe (unsafePerformIO)
import qualified Stuff
import qualified Generate.Html as Html
import qualified Generate
import qualified Data.ByteString as BS


compileToJson :: FilePath -> NE.List FilePath -> IO (Either Encode.Value Encode.Value)
compileToJson root paths = do
  let toBS = BSL.toStrict . B.toLazyByteString
  result <- compileWithoutJsGen root paths
  pure $
    case result of
      Right () ->
        Right $ Encode.object [ "compiled" ==> Encode.bool True ]
      Left exit -> do
        Left $ Exit.toJson $ Exit.reactorToReport exit


compileToBuilder :: FilePath -> NE.List FilePath -> IO (Either BS.ByteString BS.ByteString)
compileToBuilder root paths = do
  let toBS = BSL.toStrict . B.toLazyByteString
  result <- compile root paths
  pure $
    case result of
      Right builder ->
        Right $ toBS builder
      Left exit -> do
        Left $
          toBS $
            Encode.encode $
              Exit.toJson $ Exit.reactorToReport exit


compile :: FilePath -> NE.List FilePath -> IO (Either Exit.Reactor B.Builder)
compile root paths =
  do
    -- Ext.Common.debug $ " compiling " ++ show root ++ " -> " ++ show paths
    Dir.withCurrentDirectory root $
      BW.withScope $ \scope -> Stuff.withRootLock root $
        Task.run $
          do
            details <- Task.eio Exit.ReactorBadDetails $ Details.load Reporting.silent scope root
            artifacts <- Task.eio Exit.ReactorBadBuild $ Build.fromPaths Reporting.silent root details paths
            javascript <- Task.mapError Exit.ReactorBadGenerate $ Generate.dev root details artifacts
            let (NE.List name _) = Build.getRootNames artifacts
            return $ Html.sandwich name javascript


compileWithoutJsGen :: FilePath -> NE.List FilePath -> IO (Either Exit.Reactor ())
compileWithoutJsGen root paths =
  do
    Dir.withCurrentDirectory root $
      BW.withScope $ \scope -> Stuff.withRootLock root $
        Task.run $
          do
            details <- Task.eio Exit.ReactorBadDetails $ Details.load Reporting.silent scope root
            artifacts <- Task.eio Exit.ReactorBadBuild $ Build.fromPaths Reporting.silent root details paths

            return ()



{- Appropriated from worker/src/Artifacts.hs
   WARNING: does not load any user code!!!
-}
allDepArtifacts :: IO CompileHelpers.Artifacts
allDepArtifacts =
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



