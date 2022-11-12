{-# LANGUAGE OverloadedStrings #-}

module Watchtower.Live where

import AST.Canonical (Port (Outgoing))
import AST.Source (Type_ (TVar))
import Control.Applicative ((<$>), (<*>), (<|>))
import qualified Control.Concurrent.STM as STM
import Control.Monad as Monad (foldM, guard)
import Control.Monad.Trans (liftIO)

import qualified Data.Name as Name
import qualified Data.ByteString.Builder
import qualified Data.ByteString.Lazy
import qualified Data.List as List
import qualified Data.Either as Either
import qualified Data.Maybe as Maybe
import qualified Data.Map as Map
import qualified Data.NonEmptyList as NonEmpty
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy
import qualified Develop.Generate.Help
import Ext.Common
import qualified Ext.Sentry
import qualified Json.Decode
import Json.Encode ((==>))
import qualified Json.Encode
import qualified Json.String
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Snap as WS
import qualified Reporting.Annotation as Ann
import Snap.Core hiding (path)
import Snap.Http.Server
import Snap.Util.FileServe
import System.IO (hFlush, hPutStr, hPutStrLn, stderr, stdout)
import qualified Ext.CompileProxy
import qualified Ext.Dev.Project
import qualified Watchtower.StaticAssets
import qualified Watchtower.Websocket
import qualified Ext.FileProxy
import qualified Ext.CompileMode

import qualified Reporting.Doc
import qualified Reporting.Render.Type
import qualified Reporting.Warning as Warning
import qualified Reporting.Render.Type.Localizer
import qualified Watchtower.Live.Compile
import qualified Watchtower.Live.Client as Client

type State = Client.State
type ProjectCache = Client.ProjectCache

encodeWarning =
  Client.encodeWarning

getRoot =
  Client.getRoot


init :: IO Client.State
init =
    Client.State
      <$> Watchtower.Websocket.clientsInit
      <*> STM.newTVarIO []

initWith :: FilePath -> IO Client.State
initWith root =
  do
    projectList <- discoverProjects root
    Client.State
      <$> Watchtower.Websocket.clientsInit
      <*> STM.newTVarIO projectList

discoverProjects :: FilePath -> IO [Client.ProjectCache]
discoverProjects root = do
  projects <- Ext.Dev.Project.discover root
  let projectTails = fmap (getProjectShorthand root) projects
  Ext.Common.logList ("DISCOVER 👁️  found projects\n" ++ root) projectTails
  Monad.foldM initializeProject [] projects


getProjectShorthand :: FilePath -> Ext.Dev.Project.Project -> FilePath
getProjectShorthand root proj =
    case (List.stripPrefix root (Ext.Dev.Project.getRoot proj)) of
      Nothing -> "."
      Just "" -> "."
      Just str ->
        str

initializeProject :: [Client.ProjectCache] -> Ext.Dev.Project.Project -> IO [Client.ProjectCache]
initializeProject accum project =
  do
    cache <- Ext.Sentry.init
    pure (Client.ProjectCache project cache : accum)


websocket :: Client.State -> Snap ()
websocket state =
  route
    [ ("/ws", websocket_ state)
    ]

websocket_ :: Client.State -> Snap ()
websocket_ state@(Client.State mClients projects) = do
  mKey <- getHeader "sec-websocket-key" <$> getRequest
  case mKey of
    Just key -> do
      let onJoined clientId totalClients = do
            -- statuses <-
            --   Monad.foldM
            --     ( \gathered (ProjectCache proj cache) ->
            --         do
            --           jsonStatus <- Ext.Sentry.getCompileResult cache
            --           pure $ addProjectStatusIfErr proj jsonStatus gathered
            --     )
            --     []
            --     projects
            debug "💪  Joined"
            -- pure $ Just $ builderToString $ encodeOutgoing (ElmStatus statuses)
            pure Nothing

      Watchtower.Websocket.runWebSocketsSnap $
        Watchtower.Websocket.socketHandler
          mClients
          onJoined
          (receive state)
          (T.decodeUtf8 key)
          Client.emptyWatch
    Nothing ->
      error404


error404 :: Snap ()
error404 =
  do
    modifyResponse $ setResponseStatus 404 "Not Found"
    modifyResponse $ setContentType "text/html; charset=utf-8"
    writeBuilder $ Develop.Generate.Help.makePageHtml "NotFound" Nothing


receive state clientId text = do
  case Json.Decode.fromByteString Client.decodeIncoming (T.encodeUtf8 text) of
    Left err -> do
      debug $ (T.unpack "Error decoding!" <> T.unpack text)
      pure ()

    Right action -> do
      receiveAction state clientId action


receiveAction :: Client.State -> Client.ClientId -> Client.Incoming -> IO ()
receiveAction state@(Client.State mClients mProjects) clientId incoming =
  case incoming of
    Client.Changed fileChanged ->
      do
        Ext.Common.log "👀 file changed" fileChanged
        Watchtower.Live.Compile.recompile state [fileChanged]

    Client.Watched watching ->
      do
        Ext.Common.log "👀 watch changed" ""
        STM.atomically $ do
          STM.modifyTVar
            mClients
            ( fmap
                ( Watchtower.Websocket.updateClientData
                    clientId
                    (Client.watchTheseFilesOnly watching)
                )
            )
        Watchtower.Live.Compile.recompile state (Map.keys watching)

    Client.Discover root ->
      do
        Ext.Common.log "👀 discover requested" root

        discovered <- discoverProjects root

        STM.atomically $
            do
              STM.modifyTVar mProjects
                  (\projects ->
                      List.foldl
                        (\existing new ->
                          if List.any (Client.matchingProject new) existing then
                              existing
                          else
                              new : existing

                        )
                        projects
                        discovered
                  )

              STM.modifyTVar
                mClients
                ( fmap
                    ( Watchtower.Websocket.updateClientData
                        clientId
                        ( Client.watchProjects (List.map Client.getProjectRoot discovered))
                        
                    )
                )

        statuses <- Client.getAllStatuses state

        Client.broadcastTo mClients clientId (Client.ElmStatus statuses)



