{-# LANGUAGE OverloadedStrings #-}

module Watchtower.Compile where

-- @TODO cleanup imports

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Control.Monad.Trans (MonadIO(liftIO))
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HashMap
import Data.Monoid ((<>))
import Control.Concurrent.STM (atomically, newTVarIO, readTVar, writeTVar, TVar)
import qualified System.Directory as Dir
import System.FilePath as FP

import Snap.Core hiding (path)
import Snap.Http.Server
import Snap.Util.FileServe

import qualified Data.NonEmptyList as NE
import qualified Json.Encode as Encode
import Json.Encode ((==>))
import qualified BackgroundWriter as BW
import qualified Build
import qualified Elm.Details as Details
import qualified Develop.Generate.Help as Help
import qualified Develop.Generate.Index as Index
import qualified Develop.StaticFiles as StaticFiles
import qualified Generate.Html as Html
import qualified Generate
import qualified Reporting
import qualified Reporting.Exit as Exit
import qualified Reporting.Task as Task
import qualified Stuff

import Ext.Common (trackedForkIO, getProjectRootFor)
import qualified Ext.Filewatch as Filewatch
import qualified Ext.Sentry as Sentry

import StandaloneInstances



compileToJson :: FilePath -> IO (Either Encode.Value Encode.Value)
compileToJson path =
  do
      let toBS = BSL.toStrict . B.toLazyByteString
      result <- compile path

      -- hindentPrintValue "Exit.Reactor" result

      pure $
        case result of
            Right builder ->
              Right $
                Encode.object
                  [ "compiled" ==> (Encode.bool True)
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


compileToBuilder :: FilePath -> FilePath -> IO (Either BS.ByteString BS.ByteString)
compileToBuilder root path =
  do
      let toBS = BSL.toStrict . B.toLazyByteString
      result <- Dir.withCurrentDirectory root $ compile path

      pure $
        case result of
            Right builder ->
              Right $ toBS builder

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
              Left $ toBS $ Encode.encode $
                Exit.toJson $ Exit.reactorToReport exit


compile :: FilePath -> IO (Either Exit.Reactor B.Builder)
compile path =
  do  maybeRoot <- getProjectRootFor path
      case maybeRoot of
        Nothing ->
          return $ Left $ Exit.ReactorNoOutline

        Just root ->
          BW.withScope $ \scope -> Stuff.withRootLock root $ Task.run $
            do  details <- Task.eio Exit.ReactorBadDetails $ Details.load Reporting.silent scope root
                artifacts <- Task.eio Exit.ReactorBadBuild $ Build.fromPaths Reporting.silent root details (NE.List path [])
                javascript <- Task.mapError Exit.ReactorBadGenerate $ Generate.dev root details artifacts
                let (NE.List name _) = Build.getRootNames artifacts
                return $ Html.sandwich name javascript
