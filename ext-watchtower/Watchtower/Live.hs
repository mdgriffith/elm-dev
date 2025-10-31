{-# LANGUAGE OverloadedStrings #-}

module Watchtower.Live where

import AST.Canonical (Port (Outgoing))
import AST.Source (Type_ (TVar))
import Control.Applicative ((<$>), (<*>), (<|>))
import qualified Control.Concurrent.STM as STM
import Control.Monad as Monad (foldM, guard)
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Builder
import qualified Data.ByteString.Lazy
import qualified Data.Either as Either
import Data.Function ((&))
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Name as Name
import qualified Data.NonEmptyList as NonEmpty
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy
import qualified Develop.Generate.Help
import qualified Ext.CompileMode
import qualified Ext.CompileProxy
import qualified Ext.Dev.Project
import qualified Ext.Log
import qualified Ext.Sentry
import qualified Json.Decode
import Json.Encode ((==>))
import qualified Json.Encode
import qualified Json.String
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Snap as WS
import qualified Reporting.Annotation as Ann
import qualified Reporting.Doc
import qualified Reporting.Render.Type
import qualified Reporting.Render.Type.Localizer
import qualified Reporting.Warning as Warning
import Snap.Core hiding (path)
import Snap.Http.Server
import Snap.Util.FileServe
import qualified System.FilePath as FilePath
import System.IO (hFlush, hPutStr, hPutStrLn, stderr, stdout)
import qualified Watchtower.Live.Client as Client
import qualified Watchtower.Live.Compile
import qualified Watchtower.State.Discover
import qualified Watchtower.StaticAssets
import qualified Watchtower.Websocket

type State = Client.State

type ProjectCache = Client.ProjectCache

encodeWarning :: Reporting.Render.Type.Localizer.Localizer -> Warning.Warning -> Json.Encode.Value
encodeWarning =
  Client.encodeWarning


websocket :: Client.State -> Snap ()
websocket state =
  route
    [ ("/ws", websocket_ state)
    ]

isSuccess :: Either Json.Encode.Value Json.Encode.Value -> Bool
isSuccess (Left _) = True
isSuccess (Right _) = False

flattenJsonStatus :: Either Json.Encode.Value Json.Encode.Value -> Json.Encode.Value
flattenJsonStatus (Left json) = json
flattenJsonStatus (Right json) = json

websocket_ :: Client.State -> Snap ()
websocket_ state@(Client.State mClients mProjects _ _ _ _ _) = do
  mKey <- getHeader "sec-websocket-key" <$> getRequest
  case mKey of
    Just key -> do
      let onJoined clientId totalClients = do
            projects <- STM.readTVarIO mProjects
            statuses <-
              Monad.foldM
                ( \gathered (Client.ProjectCache proj docsInfo _ mCompileResult _) -> do
                    result <- STM.readTVarIO mCompileResult
                    let projectStatus =
                          case result of
                            Client.Success _ -> Client.ProjectStatus proj True (Client.toOldJSON result) docsInfo
                            _ -> Client.ProjectStatus proj False (Client.toOldJSON result) docsInfo
                    pure $ projectStatus : gathered
                )
                []
                projects

            Ext.Log.log Ext.Log.Live ("💪  Joined, reporting project statuses: " <> show (length projects))
            pure $ Just $ Client.builderToString $ Client.encodeOutgoing (Client.ElmStatus statuses)

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

receive :: Client.State -> Client.ClientId -> T.Text -> IO ()
receive state clientId text = do
  case Json.Decode.fromByteString Client.decodeIncoming (T.encodeUtf8 text) of
    Left err -> do
      Ext.Log.log Ext.Log.Live $ T.unpack "Error decoding!" <> T.unpack text
      pure ()
    Right action -> do
      receiveAction state clientId action

receiveAction :: Client.State -> Client.ClientId -> Client.Incoming -> IO ()
receiveAction state@(Client.State mClients mProjects _ _ _ _ _) senderClientId incoming =
  case incoming of
    Client.Changed fileChanged ->
      do
        Ext.Log.log Ext.Log.Live ("👀 file changed: " <> FilePath.takeFileName fileChanged)
        Watchtower.Live.Compile.recompile state [fileChanged]
    Client.Watched watching ->
      do
        Ext.Log.log
          Ext.Log.Live
          ("👀 watch changed" <> ("\n    " ++ List.intercalate "\n    " (fmap FilePath.takeFileName (Map.keys watching))))

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
    Client.Discover root watching -> do
      Watchtower.State.Discover.discover state root
    Client.EditorViewingUpdated viewingList -> do
      Ext.Log.log Ext.Log.Live ("👀 editor viewing updated: " ++ show viewingList)
      broadCastToEveryoneNotMe
        state
        senderClientId
        (Client.EditorViewing viewingList)
    Client.EditorJumpToRequested file position -> do
      Ext.Log.log Ext.Log.Live ("👀 editor jump to requested: " ++ show file ++ " " ++ show position)
      broadCastToEveryoneNotMe
        state
        senderClientId
        (Client.EditorJumpTo file position)

-- helper broadcast (typo retained to preserve call sites)
broadCastToEveryoneNotMe :: Client.State -> Client.ClientId -> Client.Outgoing -> IO ()
broadCastToEveryoneNotMe (Client.State mClients _ _ _ _ _ _) myClientId =
  Client.broadcastToMany
    mClients
    ( not . Watchtower.Websocket.matchId myClientId
    )
