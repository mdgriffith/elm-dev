
-- This compilation mode uses an in-memory file cache to avoid disk reads

{-# LANGUAGE OverloadedStrings #-}

module Watchtower.Compile.FileCache (compileToJson) where

-- @TODO cleanup imports

import qualified BackgroundWriter as BW
import qualified Build
import qualified Ext.FileCached.Build
import Control.Applicative ((<|>))
import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, writeTVar)
import Control.Monad (guard)
import Control.Monad.Trans (MonadIO (liftIO))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HashMap
import Data.Monoid ((<>))
import qualified Data.NonEmptyList as NE
import qualified Develop.Generate.Help as Help
import qualified Develop.Generate.Index as Index
import qualified Develop.StaticFiles as StaticFiles
import qualified Elm.Details as Details
import Ext.Common (getProjectRootFor, trackedForkIO)
import qualified Ext.Common
import qualified Ext.Filewatch as Filewatch
import qualified Ext.Sentry as Sentry
import qualified Generate
import qualified Generate.Html as Html
import Json.Encode ((==>))
import qualified Json.Encode as Encode
import qualified Reporting
import qualified Reporting.Exit as Exit
import qualified Reporting.Task as Task
import Snap.Core hiding (path)
import Snap.Http.Server
import Snap.Util.FileServe
import StandaloneInstances
import qualified Stuff
import qualified System.Directory as Dir
import System.FilePath as FP


compileToJson :: FilePath -> NE.List FilePath -> IO (Either Encode.Value Encode.Value)
compileToJson root paths =
  do
    let toBS = BSL.toStrict . B.toLazyByteString
    result <- compile root paths

    -- hindentPrintValue "Exit.Reactor" result

    pure $
      case result of
        Right builder ->
          Right $
            Encode.object
              [ "compiled" ==> Encode.bool True
              ]
        Left exit -> do
          -- @LAMDERA because we do AST injection, sometimes we might get
          -- an error that actually cannot be displayed, i.e, the reactorToReport
          -- function itself throws an exception, mainly due to use of unsafe
          -- functions like Prelude.last and invariants that for some reason haven't
          -- held with our generated code (usually related to subsequent type inference)
          -- We print out a less-processed version here in debug mode to aid with
          -- debugging in these scenarios, as the browser will just get zero bytes
          -- debugPass "serveElm error" (Exit.reactorToReport exit) (pure ())
          -- Help.makePageHtml "Errors" $ Just $
          Left $ Exit.toJson $ Exit.reactorToReport exit


compile :: FilePath -> NE.List FilePath -> IO (Either Exit.Reactor B.Builder)
compile root paths =
  do
    Ext.Common.debug $ " compiling " ++ show root ++ " -> " ++ show paths
    Dir.withCurrentDirectory root $
      BW.withScope $ \scope -> Stuff.withRootLock root $
        Task.run $
          do
            details <- Task.eio Exit.ReactorBadDetails $ Details.load Reporting.silent scope root
            artifacts <- Task.eio Exit.ReactorBadBuild $ Ext.FileCached.Build.fromPaths Reporting.silent root details paths
            javascript <- Task.mapError Exit.ReactorBadGenerate $ Generate.dev root details artifacts
            let (NE.List name _) = Ext.FileCached.Build.getRootNames artifacts
            return $ Html.sandwich name javascript
