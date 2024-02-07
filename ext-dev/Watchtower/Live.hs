{-# LANGUAGE OverloadedStrings #-}

module Watchtower.Live where

import AST.Canonical (Port (Outgoing))
import AST.Source (Type_ (TVar))
import Control.Applicative ((<$>), (<*>), (<|>))
import qualified Control.Concurrent.STM as STM
import Control.Monad as Monad (foldM, guard)
import Control.Monad.Trans (liftIO)
import Data.Function ((&))

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
import qualified System.FilePath as FilePath
import qualified Ext.CompileProxy
import qualified Ext.Dev.Project
import qualified Watchtower.StaticAssets
import qualified Watchtower.Websocket
import qualified Ext.FileProxy
import qualified Ext.CompileMode
import qualified Ext.Log

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
  Ext.Log.log Ext.Log.Live (("👁️  found projects\n" ++ root) <> (formatList projectTails))
  Monad.foldM initializeProject [] projects


indent :: Int -> String -> String
indent i str =
    List.replicate i ' ' ++ str

formatList :: [String] -> String
formatList strs =
  List.foldr (\tail gathered ->  gathered ++ indent 4 tail ++ "\n") "\n" strs



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
            Ext.Log.log Ext.Log.Live "💪  Joined"
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
      Ext.Log.log Ext.Log.Live  $ (T.unpack "Error decoding!" <> T.unpack text)
      pure ()

    Right action -> do
      receiveAction state clientId action


receiveAction :: Client.State -> Client.ClientId -> Client.Incoming -> IO ()
receiveAction state@(Client.State mClients mProjects) senderClientId incoming =
  case incoming of
    Client.Changed fileChanged ->
      do
        Ext.Log.log Ext.Log.Live ("👀 file changed: " <> (FilePath.takeFileName fileChanged))
        Watchtower.Live.Compile.recompile state [fileChanged]

    Client.Watched watching ->
      do
        Ext.Log.log Ext.Log.Live 
          ("👀 watch changed" <> ("\n    " ++ List.intercalate "\n    " (fmap FilePath.takeFileName ((Map.keys watching)))))
        
        maybePreviouslyWatching <- Client.getClientData senderClientId state
        
        STM.atomically $ do
          STM.modifyTVar
            mClients
            ( fmap
                ( Watchtower.Websocket.updateClientData
                    senderClientId
                    (Client.watchTheseFilesOnly watching)
                )
            )

        -- Only recompile files that were not being watched before.
        case maybePreviouslyWatching of
          Nothing ->
            pure ()
          
          Just previouslyWatching -> do
            let previouslyWatchingFiles = Client.watchedFiles previouslyWatching
            -- Map.difference, values in watching, not in previouslyWatching
            let addedKeys = Map.keys (Map.difference watching previouslyWatchingFiles)
            case addedKeys of
              [] -> pure ()

              _ -> Watchtower.Live.Compile.recompile state addedKeys
         
        

    Client.Discover root watching ->  do
      Ext.Log.log Ext.Log.Live ("👀 discover requested: " <> root)
      discovered <- discoverProjects root
      STM.atomically $ do
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
                  senderClientId
                  (\clientWatching ->
                      clientWatching
                        & Client.watchProjects (List.map Client.getProjectRoot discovered)
                        & Client.watchTheseFilesOnly watching
                  )       
              )
          )

      Watchtower.Live.Compile.recompile state (Map.keys watching)

    Client.EditorViewingUpdated viewingList -> do
      Ext.Log.log Ext.Log.Live ("👀 editor viewing updated: " ++ (show viewingList))
      broadCastToEveryoneNotMe state senderClientId
        (Client.EditorViewing viewingList)

    Client.EditorJumpToRequested file position -> do
      Ext.Log.log Ext.Log.Live ("👀 editor jump to requested: " ++ (show file) ++ " " ++ (show position))
      broadCastToEveryoneNotMe state senderClientId
        (Client.EditorJumpTo file position)
      



broadCastToEveryoneNotMe :: Client.State -> Client.ClientId -> Client.Outgoing -> IO ()
broadCastToEveryoneNotMe (Client.State mClients _) myClientId outgoing =
  Client.broadcastToMany mClients
    (\client -> 
      not (Watchtower.Websocket.matchId myClientId client)
    )
    outgoing