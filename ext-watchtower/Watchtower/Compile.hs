{-# LANGUAGE OverloadedStrings #-}

module Watchtower.Compile (compileToJson, compileToBuilder, warnings) where

-- @TODO cleanup imports

import qualified BackgroundWriter as BW
import qualified Build
import Control.Applicative ((<|>))
import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, writeTVar)
import Control.Monad (guard)
import Control.Monad.Trans (MonadIO (liftIO))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import qualified Data.Name as Name
import Data.Monoid ((<>))
import qualified Data.NonEmptyList as NE
import qualified Develop.Generate.Help as Help
import qualified Develop.Generate.Index as Index
import qualified Develop.StaticFiles as StaticFiles
import qualified Elm.Details as Details
import qualified Elm.Package as Pkg
import Ext.Common (getProjectRootFor, trackedForkIO)
import qualified Ext.Common
import qualified Ext.Filewatch as Filewatch
import qualified Ext.Sentry as Sentry
import qualified Generate
import qualified Generate.Html as Html
import Json.Encode ((==>))
import qualified Json.Encode as Encode

import qualified System.IO.Unsafe


import qualified AST.Source as Src
import qualified AST.Canonical as Can
import qualified AST.Optimized as Opt
import qualified File
import qualified Llamadera
import qualified Parse.Module as Parse
import qualified Canonicalize.Module as Canonicalize
import qualified Reporting.Result
-- Reporting
import qualified Reporting
import qualified Reporting.Exit as Exit
import qualified Reporting.Task as Task
import qualified Reporting.Warning as Warning
import qualified Type.Constrain.Module as Type
import qualified Type.Solve as Type
import qualified Optimize.Module as Optimize

-- server stuff
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
    result <- compileToDevNull root paths

    -- hindentPrintValue "Exit.Reactor" result

    pure $
      case result of
        Right () ->
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

compileToBuilder :: FilePath -> NE.List FilePath -> IO (Either BS.ByteString BS.ByteString)
compileToBuilder root paths =
  do
    let toBS = BSL.toStrict . B.toLazyByteString
    result <- compile root paths

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
          Left $
            toBS $
              Encode.encode $
                Exit.toJson $ Exit.reactorToReport exit

compile :: FilePath -> NE.List FilePath -> IO (Either Exit.Reactor B.Builder)
compile root paths =
  do
    Ext.Common.debug $ " compiling " ++ show root ++ " -> " ++ show paths
    Dir.withCurrentDirectory root $
      BW.withScope $ \scope -> Stuff.withRootLock root $
        Task.run $
          do
            details <- Task.eio Exit.ReactorBadDetails $ Details.load Reporting.silent scope root
            artifacts <- Task.eio Exit.ReactorBadBuild $ Build.fromPaths Reporting.silent root details paths
            javascript <- Task.mapError Exit.ReactorBadGenerate $ Generate.dev root details artifacts
            let (NE.List name _) = Build.getRootNames artifacts
            return $ Html.sandwich name javascript



compileToDevNull :: FilePath -> NE.List FilePath -> IO (Either Exit.Reactor ())
compileToDevNull root paths =
  do
    Dir.withCurrentDirectory root $
      BW.withScope $ \scope -> Stuff.withRootLock root $
        Task.run $
          do
            details <- Task.eio Exit.ReactorBadDetails $ Details.load Reporting.silent scope root
            artifacts <- Task.eio Exit.ReactorBadBuild $ Build.fromPaths Reporting.silent root details paths
           
            return ()



warnings :: FilePath -> FilePath -> IO (Either () (Src.Module, [ Warning.Warning ]))
warnings root path =
  Dir.withCurrentDirectory root $ do
    ifaces <- Llamadera.allInterfaces [path]
    source <- File.readUtf8 path
    case Parse.fromByteString Parse.Application source of
      Right srcModule ->
        do 
          let (canWarnings, eitherCanned) = Reporting.Result.run $ Canonicalize.canonicalize Pkg.dummyName ifaces srcModule
          case eitherCanned of
            Left errs ->
              pure (Right (srcModule, canWarnings))

            Right canModule ->
                case typeCheck srcModule canModule of
                  Left typeErrors ->
                      pure (Right (srcModule, canWarnings))
                  
                  Right annotations ->
                    do
                      let (optWarnings, _) = Reporting.Result.run $ Optimize.optimize annotations canModule
                      pure (Right (srcModule, canWarnings <> optWarnings))

      Left err ->
        pure (Left ())

  --  case snd $ R.run $ Optimize.optimize annotations canonical of
  --   Right localGraph ->
  --     Right localGraph

  --   Left errors ->
  --     Left (E.BadMains (Localizer.fromModule modul) errors)




typeCheck :: Src.Module -> Can.Module -> Either () (Map.Map Name.Name Can.Annotation)
typeCheck modul canonical =
  case System.IO.Unsafe.unsafePerformIO (Type.run =<< Type.constrain canonical) of
    Right annotations ->
      Right annotations

    Left errors ->
      Left ()
