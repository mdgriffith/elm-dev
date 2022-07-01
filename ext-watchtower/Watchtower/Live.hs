{-# LANGUAGE OverloadedStrings #-}

module Watchtower.Live where

import AST.Canonical (Port (Outgoing))
import AST.Source (Type_ (TVar))
import Control.Applicative ((<$>), (<*>), (<|>))
import Control.Concurrent.STM
import Control.Monad as Monad (foldM, guard)
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Builder
import qualified Data.ByteString.Lazy
import qualified Data.List as List
import qualified Data.Maybe as Maybe
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
import qualified Watchtower.Compile
import qualified Watchtower.Details
import qualified Watchtower.Project
import qualified Watchtower.StaticAssets
import qualified Watchtower.Websocket

data State = State
  { clients :: TVar [Client],
    projects ::
      [ProjectCache]
  }

data ProjectCache = ProjectCache
  { project :: Watchtower.Project.Project,
    sentry :: Ext.Sentry.Cache
  }

type ClientId = T.Text

type Client = Watchtower.Websocket.Client (Set.Set ProjectRoot)

type ProjectRoot = FilePath

init :: FilePath -> IO State
init root =
  State
    <$> Watchtower.Websocket.clientsInit
    <*> discoverProjects root

getProjectShorthand :: FilePath -> Watchtower.Project.Project -> FilePath
getProjectShorthand root proj =
    case (List.stripPrefix root (Watchtower.Project.getRoot proj)) of 
      Nothing -> "."
      Just "" -> "."
      Just str ->
        str

discoverProjects :: FilePath -> IO [ProjectCache]
discoverProjects root = do
  projects <- Watchtower.Project.discover root
  let projectTails = fmap (getProjectShorthand root) projects
  Ext.Common.logList ("DISCOVER 👁️ found projects:" ++ root) projectTails
    
  Monad.foldM initializeProject [] projects

initializeProject :: [ProjectCache] -> Watchtower.Project.Project -> IO [ProjectCache]
initializeProject accum project =
  do
    cache <- Ext.Sentry.init
    pure (ProjectCache project cache : accum)

getRoot :: FilePath -> State -> Maybe FilePath
getRoot path (State mClients projects) =
  getRootHelp path projects Nothing

getRootHelp path projects found =
  case projects of
    [] -> found
    (ProjectCache project _) : remain ->
      if Watchtower.Project.contains path project
        then case found of
          Nothing ->
            getRootHelp path remain (Just (Watchtower.Project._root project))
          Just root ->
            if List.length (Watchtower.Project._root project) > List.length root
              then getRootHelp path remain (Just (Watchtower.Project._root project))
              else getRootHelp path remain found
        else getRootHelp path remain found


recompileAllProjects :: Watchtower.Live.State -> IO ()
recompileAllProjects (Watchtower.Live.State mClients projects) = do
  
  Ext.Common.log "🛫" "Recompile everything"
  trackedForkIO $
    track "recompile" $ do
      
      Monad.foldM
        (\files proj -> recompileProject mClients proj)
        []
        projects

      Ext.Common.log "🛬"  "Recompile everything finished"
  



recompile :: Watchtower.Live.State -> [String] -> IO ()
recompile (Watchtower.Live.State mClients projects) allChangedFiles = do
  let changedElmFiles = List.filter (\filepath -> ".elm" `List.isSuffixOf` filepath ) allChangedFiles
  if (changedElmFiles /= []) then do
    debug $ "🛫  recompile starting: " ++ show changedElmFiles
    trackedForkIO $
      track "recompile" $ do
        
        Monad.foldM
          -- (recompileProjectIfSubFile mClients)
          (recompileChangedFile mClients)
          changedElmFiles
          projects

        debug $ "🛬  recompile finished: " ++ show changedElmFiles
    else
        pure ()



recompileProject :: TVar [Client] -> ProjectCache -> IO [FilePath]
recompileProject mClients proj@(ProjectCache (Watchtower.Project.Project projectRoot entrypoints) cache) =
  case entrypoints of
    [] ->
      do
        Ext.Common.log "Skipping compile, no entrypoint" projectRoot
        pure []

    _ ->
        recompileChangedFile mClients entrypoints proj


recompileChangedFile :: TVar [Client] -> [String] -> ProjectCache -> IO [FilePath]
recompileChangedFile mClients changedFiles projCache@(ProjectCache proj@(Watchtower.Project.Project projectRoot entrypoints) cache) =
    do
      case changedFiles of
        [] ->
          do
            pure []
        (top : remain) ->
            if List.any (\f -> Watchtower.Project.contains f proj) changedFiles then 
              do
                  debug $ "🧟 affected project"

                  let entry = (NonEmpty.List top remain)
                  -- Can compileToJson take multiple entrypoints like elm make?
                  eitherStatusJson <-
                    Watchtower.Compile.compileToJson
                      projectRoot
                      entry

                  Ext.Sentry.updateCompileResult cache $
                    pure eitherStatusJson

                  case eitherStatusJson of
                    Right statusJson ->
                      do
                        -- If this file compiled successfully, compile the entire project
                        recompileProjectIfSubFile mClients changedFiles projCache
                        pure ()

                    Left errJson ->
                      -- send the errors to any client that's listening
                      broadcastToMany
                        mClients
                        ( \client ->
                            let clientData = Watchtower.Websocket.clientData client
                              in Set.member (Watchtower.Project._root proj) clientData
                        )
                        (ElmStatus [ ProjectStatus proj False errJson ])

                  pure []

            else 
                pure []
           
          


{-
This function will recompile a project using the projects entrypoint if the changed file is within a projects root dir.

This does mean that if the detected entrypoint doesn't ultimately import the changed file, then you won't get errors

-}
recompileProjectIfSubFile :: TVar [Client] -> [String] -> ProjectCache -> IO [FilePath]
recompileProjectIfSubFile mClients remainingFiles (ProjectCache proj@(Watchtower.Project.Project projectRoot entrypoints) cache) =
    do
      let remaining =
            List.filter
              (\file -> not (Watchtower.Project.contains file proj))
              remainingFiles

      let maybeEntry =
            case entrypoints of
              [] ->
                let filesWithinProject =
                      List.filter
                        (\file -> Watchtower.Project.contains file proj)
                        remainingFiles
                  in case filesWithinProject of
                      [] -> Nothing
                      top : remain ->
                        Just (NonEmpty.List top remain)
              -- Nothing
              top : remainingEntrypoints ->
                Just (NonEmpty.List top remainingEntrypoints)

      case maybeEntry of
        Nothing ->
          do
            debug $ ("☹️ No detected affected project")
            pure remaining
        Just entry ->
          do
            debug $ "🧟 affected project"
            -- Can compileToJson take multiple entrypoints like elm make?
            eitherStatusJson <-
              Watchtower.Compile.compileToJson
                projectRoot
                entry

            Ext.Sentry.updateCompileResult cache $
              pure eitherStatusJson

            case eitherStatusJson of
              Right statusJson ->
                broadcastToMany
                  mClients
                  ( \client ->
                      let clientData = Watchtower.Websocket.clientData client
                        in Set.member (Watchtower.Project._root proj) clientData
                  )
                  (ElmStatus [ ProjectStatus proj True statusJson ])
              Left errJson ->
                -- send the errors to any client that's listening
                broadcastToMany
                  mClients
                  ( \client ->
                      let clientData = Watchtower.Websocket.clientData client
                        in Set.member (Watchtower.Project._root proj) clientData
                  )
                  (ElmStatus [ ProjectStatus proj False errJson ])


            pure remaining
          


websocket :: State -> Snap ()
websocket state =
  route
    [ ("/ws", websocket_ state)
    ]

websocket_ :: State -> Snap ()
websocket_ state@(State mClients projects) = do
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
          Set.empty
    Nothing ->
      error404

builderToString =
  T.decodeUtf8 . Data.ByteString.Lazy.toStrict . Data.ByteString.Builder.toLazyByteString

receive state clientId text = do
  debug $ (T.unpack "RECVD" <> T.unpack text)
  case Json.Decode.fromByteString decodeIncoming (T.encodeUtf8 text) of
    Left err -> do
      debug $ (T.unpack "Error decoding!" <> T.unpack text)
      pure ()
    Right action -> do
      debug $ (T.unpack "Action!" <> T.unpack text)
      receiveAction state clientId action

receiveAction :: State -> ClientId -> Incoming -> IO ()
receiveAction state@(State mClients projects) clientId incoming =
  case incoming of
    Watch roots ->
      do
        let newRootSet = Set.fromList roots
        debug $ "watching " <> List.concat roots
        statuses <-
          Monad.foldM
            ( \gathered (ProjectCache proj cache) ->
                if Set.member (Watchtower.Project._root proj) newRootSet
                  then do
                    jsonStatus <- Ext.Sentry.getCompileResult cache
                    pure $
                      addProjectStatusIfErr proj jsonStatus gathered
                  else pure gathered
            )
            []
            projects

        -- debug $
        --   "👀  watch, init status: " ++ show statuses
        atomically $
          do
            modifyTVar
              mClients
              ( fmap
                  ( Watchtower.Websocket.updateClientData
                      clientId
                      ( Set.union newRootSet
                      )
                  )
              )
        broadcastTo mClients clientId (ElmStatus statuses)
    Visible visible -> do
      debug $ "forwarding visibility"
      broadcastAll mClients (FwdVisible visible)
    JumpTo location -> do
      debug $ "forwarding jump"
      broadcastAll mClients (FwdJumpTo location)
    InsertMissingTypeSignatures path -> do
      debug $ "forwarding insert-missing"
      broadcastAll mClients (FwdInsertMissingTypeSignatures path)

addProjectStatusIfErr ::
      Watchtower.Project.Project 
        -> Either Json.Encode.Value Json.Encode.Value 
        -> [ProjectStatus] 
        -> [ProjectStatus]
addProjectStatusIfErr proj either ls =
  case either of
    Right json ->
      ls
    Left json ->
      (ProjectStatus proj False json) : ls

reduceStatus :: Either Json.Encode.Value Json.Encode.Value -> Json.Encode.Value
reduceStatus either =
  case either of
    Right json ->
      json
    Left json ->
      json

decodeIncoming :: Json.Decode.Decoder T.Text Incoming
decodeIncoming =
  Json.Decode.field "msg" Json.Decode.string
    >>= ( \msg ->
            case msg of
              "Watch" ->
                Watch <$> Json.Decode.field "details" (Json.Decode.list decodeWatch)
              "Visible" ->
                Visible <$> Json.Decode.field "details" Watchtower.Details.decodeVisible
              "Jump" ->
                JumpTo <$> Json.Decode.field "details" Watchtower.Details.decodeLocation
              "InsertMissingTypeSignatures" ->
                InsertMissingTypeSignatures
                  <$> ( Json.Decode.field
                          "details"
                          (Json.Decode.field "path" (Json.String.toChars <$> Json.Decode.string))
                      )
              _ ->
                Json.Decode.failure "Unknown msg"
        )

decodeWatch =
  Json.Decode.string
    & fmap Json.String.toChars

error404 :: Snap ()
error404 =
  do
    modifyResponse $ setResponseStatus 404 "Not Found"
    modifyResponse $ setContentType "text/html; charset=utf-8"
    writeBuilder $ Develop.Generate.Help.makePageHtml "NotFound" Nothing

{- Messages!

So, we have two different kinds of messages.

If there's a better way to square this, I'd love to hear it.

1. Messages being forwarded from one client to another.
    I.e. the editor saying a file is visible or focused.

2. An idea of a status subscription which you can ask for.

There are also one-off questions that can be asked via normal GET requests handeld directly by `Watchtower`.

-}
data Incoming
  = -- forwarding information from a source to somewhere else
    Visible Watchtower.Details.Visible
  | JumpTo Watchtower.Details.Location
  | InsertMissingTypeSignatures FilePath
  | -- watch the provided filepath, which must match a project root
    Watch [FilePath]

data Outgoing
  = -- forwarding information
    FwdVisible Watchtower.Details.Visible
  | FwdJumpTo Watchtower.Details.Location
  | FwdInsertMissingTypeSignatures FilePath
  | ElmStatus [ ProjectStatus ]


data ProjectStatus = ProjectStatus 
  { _project :: Watchtower.Project.Project
  , _success :: Bool
  , _json :: Json.Encode.Value
  }


encodeOutgoing :: Outgoing -> Data.ByteString.Builder.Builder
encodeOutgoing out =
  Json.Encode.encodeUgly $
    case out of
      FwdVisible visible ->
        Json.Encode.object
          [ "msg" ==> Json.Encode.string (Json.String.fromChars "Visible"),
            "details" ==> Watchtower.Details.encodeVisible visible
          ]
      FwdJumpTo loc ->
        Json.Encode.object
          [ "msg" ==> Json.Encode.string (Json.String.fromChars "Jump"),
            "details" ==> Watchtower.Details.encodeLocation loc
          ]
      FwdInsertMissingTypeSignatures path ->
        Json.Encode.object
          [ "msg" ==> Json.Encode.string (Json.String.fromChars "InsertMissingTypeSignatures"),
            "details"
              ==> Json.Encode.object
                [ "path" ==> Json.Encode.string (Json.String.fromChars path)
                ]
          ]
      ElmStatus statuses ->
        Json.Encode.object
          [ "msg" ==> Json.Encode.string (Json.String.fromChars "Status"),
            "details"
              ==> Json.Encode.list
                ( \(ProjectStatus project success status) ->
                    Json.Encode.object
                      [ "root"
                          ==> Json.Encode.string
                            ( Json.String.fromChars
                                (Watchtower.Project._root project)
                            ),
                        "status" ==> status
                      ]
                )
                statuses
          ]

encodeStatus (Watchtower.Project.Project root entrypoints, js) =
  Json.Encode.object
    [ "root" ==> Json.Encode.string (Json.String.fromChars root),
      "entrypoints" ==> Json.Encode.list (Json.Encode.string . Json.String.fromChars) entrypoints,
      "status" ==> js
    ]

{- WEBSOCKET STUFF -}

broadcastAll :: TVar [Client] -> Outgoing -> IO ()
broadcastAll allClients outgoing =
  Watchtower.Websocket.broadcastWith
    allClients
    (\c -> True)
    ( builderToString $
        encodeOutgoing outgoing
    )


broadcastTo :: TVar [Client] -> ClientId -> Outgoing -> IO ()
broadcastTo allClients id outgoing =
  do 
    Ext.Common.log "◀️" (outgoingToLog outgoing)
    Watchtower.Websocket.broadcastWith
      allClients
      ( Watchtower.Websocket.matchId id
      )
      ( builderToString $
          encodeOutgoing outgoing
      )


broadcastToMany :: TVar [Client] -> (Client -> Bool) -> Outgoing -> IO ()
broadcastToMany allClients shouldBroadcast outgoing =
  do 
    Ext.Common.log "◀️" (outgoingToLog outgoing)
    Watchtower.Websocket.broadcastWith
      allClients
      shouldBroadcast
      ( builderToString $
          encodeOutgoing outgoing
      )


outgoingToLog :: Outgoing -> String
outgoingToLog outgoing =
  case outgoing of
    ElmStatus projectStatusList ->
      "Status: " ++ Ext.Common.formatList (fmap projectStatusToString projectStatusList)
    FwdVisible vis ->
      "fwd:visibility"
    FwdJumpTo jump ->
      "fwd:jump"
    FwdInsertMissingTypeSignatures path ->
      "fwd:missing signatures"


projectStatusToString :: ProjectStatus -> String
projectStatusToString (ProjectStatus proj success json) =
    if success then 
        "Success @" ++ Watchtower.Project.getRoot proj
    else
        "Failing @" ++ Watchtower.Project.getRoot proj
