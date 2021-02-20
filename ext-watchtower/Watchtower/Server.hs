{-# LANGUAGE OverloadedStrings #-}

module Watchtower.Server (Flags(..),serve) where

import Control.Applicative ((<|>))
import Control.Monad.Trans (MonadIO(liftIO))
import System.IO (hFlush, hPutStr, hPutStrLn, stderr, stdout)
import Snap.Core hiding (path)
import Snap.Http.Server
import Snap.Util.FileServe


import qualified Develop.Generate.Help
import qualified Json.Encode
import qualified Reporting.Annotation as Ann
import qualified Watchtower.StaticAssets
import qualified Watchtower.Live
import qualified Watchtower.Details
import qualified Watchtower.Questions

data Flags =
  Flags
    { _port :: Maybe Int
    }

serve :: Flags -> IO ()
serve (Flags maybePort) =
  do  let port = maybe 8000 id maybePort
      putStrLn $ "Go to http://localhost:" ++ show port ++ " to see your project dashboard."
      liveState <- liftIO $ Watchtower.Live.init
      httpServe (config port) $
        serveAssets
            <|> Watchtower.Live.websocket liveState
            <|> Watchtower.Questions.serve
            <|> error404




config :: Int -> Config Snap a
config port =
  setVerbose False $ setPort port $
    setAccessLog ConfigNoLog $ setErrorLog ConfigNoLog $ defaultConfig


-- SERVE STATIC ASSETS


serveAssets :: Snap ()
serveAssets =
  do  path <- getSafePath
      case Watchtower.StaticAssets.lookup path of
        Nothing ->
          pass

        Just (content, mimeType) ->
          do  modifyResponse (setContentType (mimeType <> ";charset=utf-8"))
              writeBS content


-- NOT FOUND


error404 :: Snap ()
error404 =
  do  modifyResponse $ setResponseStatus 404 "Not Found"
      modifyResponse $ setContentType "text/html;charset=utf-8"
      writeBuilder $ Develop.Generate.Help.makePageHtml "NotFound" Nothing
