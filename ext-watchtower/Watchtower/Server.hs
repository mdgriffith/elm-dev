{-# LANGUAGE OverloadedStrings #-}

module Watchtower.Server (Flags (..), serve) where

import Control.Applicative ((<|>))
import Control.Monad.Trans (MonadIO (liftIO))
import qualified Develop.Generate.Help
import qualified Ext.Common
import qualified Ext.Filewatch
import qualified Json.Encode
import qualified Reporting.Annotation as Ann
import Snap.Core hiding (path)
import qualified Snap.Http.Server
import Snap.Util.FileServe
import qualified System.Directory as Dir
import qualified Watchtower.Live
import qualified Watchtower.Live.Compile
import qualified Watchtower.Questions
import qualified Watchtower.StaticAssets
import Data.Maybe as Maybe

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import qualified Ext.FileCache as FileCache

import qualified Ext.CompileMode
import qualified Ext.Log

newtype Flags = Flags
  { _port :: Maybe Int
  }

serve :: Maybe FilePath -> Flags -> IO ()
serve maybeRoot (Flags maybePort) =
  do
    let port = Ext.Common.withDefault 51213 maybePort
    Ext.Common.atomicPutStrLn $ "elm-dev is now running at http://localhost:" ++ show port

    liveState <- Watchtower.Live.init

    debug <- Ext.Log.isActive Ext.Log.VerboseServer
    Ext.Log.log Ext.Log.VerboseServer ("Running in " <> (show Ext.CompileMode.getMode) <> " Mode")
    -- VSCode is telling us when files change
    -- Start file watcher for the memory mode
    -- Ext.Filewatch.watch root (Watchtower.Live.recompile liveState)


    Snap.Http.Server.httpServe (config port debug) $
      serveAssets
        <|> Watchtower.Live.websocket liveState
        <|> Watchtower.Questions.serve liveState
        <|> error404

config :: Int -> Bool -> Snap.Http.Server.Config Snap a
config port isDebug =
  Snap.Http.Server.setVerbose isDebug $
    Snap.Http.Server.setPort port $
      Snap.Http.Server.setAccessLog (Snap.Http.Server.ConfigIoLog logger) $
        Snap.Http.Server.setErrorLog
          (Snap.Http.Server.ConfigIoLog logger)
          Snap.Http.Server.defaultConfig


logger =
  (\bs ->
      Ext.Log.log Ext.Log.VerboseServer  $ T.unpack $ T.decodeUtf8 bs
  )


-- SERVE STATIC ASSETS

serveAssets :: Snap ()
serveAssets =
  do
    path <- getSafePath
    case Watchtower.StaticAssets.lookup path of
      Nothing ->
        pass
      Just (content, mimeType) ->
        do
          modifyResponse (setContentType (mimeType <> ";charset=utf-8"))
          writeBS content

error404 :: Snap ()
error404 =
  do
    modifyResponse $ setResponseStatus 404 "Not Found"
    modifyResponse $ setContentType "text/html; charset=utf-8"
    writeBuilder $ "404 page not found!"
