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
import qualified Data.List as List
import qualified Data.Either as Either
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
import System.IO (hFlush, hPutStr, hPutStrLn, stderr, stdout)
import qualified Watchtower.Compile.Classic
import qualified Watchtower.Details
import qualified Watchtower.Project
import qualified Watchtower.StaticAssets
import qualified Watchtower.Websocket

data State = State
  { clients :: STM.TVar [Client],
    projects ::
      STM.TVar
        [ProjectCache]
  }

data ProjectCache = ProjectCache
  { project :: Watchtower.Project.Project,
    sentry :: Ext.Sentry.Cache
  }

type ClientId = T.Text

type Client = Watchtower.Websocket.Client (Set.Set ProjectRoot)

type ProjectRoot = FilePath


cacheRoot :: ProjectCache -> FilePath
cacheRoot (ProjectCache proj _) =
    Watchtower.Project.getRoot proj

matchingCache ::  ProjectCache -> ProjectCache -> Bool
matchingCache (ProjectCache one _) (ProjectCache two _) =
  Watchtower.Project.equal one two

init :: FilePath -> IO State
init root =
  do
    projectList <- discoverProjects root
    State
      <$> Watchtower.Websocket.clientsInit
      <*> STM.newTVarIO projectList

cacheToString (ProjectCache project _) =
  Watchtower.Project._root project

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
  Ext.Common.logList ("DISCOVER 👁️  found projects\n" ++ root) projectTails
  Monad.foldM initializeProject [] projects

initializeProject :: [ProjectCache] -> Watchtower.Project.Project -> IO [ProjectCache]
initializeProject accum project =
  do
    cache <- Ext.Sentry.init
    pure (ProjectCache project cache : accum)


projectIsCompiling :: FilePath -> State -> IO Bool
projectIsCompiling target (State mClients mProjects) =
  do
      projects <- STM.readTVarIO mProjects
      maybeBool <-
        Monad.foldM
            (\found (Watchtower.Live.ProjectCache proj sentry) ->
              case found of
                Nothing ->
                    if Watchtower.Project.contains target proj then
                      do
                        result <- Ext.Sentry.getCompileResult sentry
                        pure
                          (case result of
                            Right _ ->
                              Just True
                            Left _ ->
                              Just False
                          )
                    else
                      pure Nothing

                _ ->
                    pure found

            )
            Nothing
            projects
      pure (Maybe.fromMaybe False maybeBool)

getRoot :: FilePath -> State -> IO (Maybe FilePath)
getRoot path (State mClients mProjects) =
  do
    projects <- STM.readTVarIO mProjects
    pure (getRootHelp path projects Nothing)

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
recompileAllProjects (Watchtower.Live.State mClients mProjects) = do

  Ext.Common.log "🛫" "Recompile everything"
  trackedForkIO $
    track "recompile" $ do
      projects <- STM.readTVarIO mProjects
      Monad.foldM
        (\files proj -> recompileProject mClients proj)
        []
        projects

      Ext.Common.log "🛬"  "Recompile everything finished"



getAllStatuses :: Watchtower.Live.State -> IO [ProjectStatus]
getAllStatuses state@(State mClients mProjects) =
  do
    projects <- STM.readTVarIO mProjects

    Monad.foldM
      (\statuses proj ->
        do
          status <- getStatus proj
          pure (status : statuses)
      )
      []
      projects


getStatus :: ProjectCache -> IO ProjectStatus
getStatus (ProjectCache proj cache) =
    do
        compileResult <- Ext.Sentry.getCompileResult cache
        let successful = Either.isRight compileResult
        let json = (case compileResult of
                      Left j -> j
                      Right j -> j
                    )
        pure (ProjectStatus proj successful json)

recompile :: Watchtower.Live.State -> [String] -> IO ()
recompile (Watchtower.Live.State mClients mProjects) allChangedFiles = do
  let changedElmFiles = List.filter (\filepath -> ".elm" `List.isSuffixOf` filepath ) allChangedFiles
  if (changedElmFiles /= []) then do
    debug $ "🛫  recompile starting: " ++ show changedElmFiles
    projects <- STM.readTVarIO mProjects
    trackedForkIO $
      track "recompile" $ do
        Monad.foldM
          (recompileChangedFile mClients)
          changedElmFiles
          projects


recompile :: Watchtower.Live.State -> [String] -> IO ()
recompile (Watchtower.Live.State mClients projects) changedFiles = do
  debug $ "🛫  recompile starting: " ++ show changedFiles
  trackedForkIO $
    track "recompile" $ do

      Monad.foldM
        -- (recompileProjectIfSubFile mClients)
        (recompileChangedFile mClients)
        changedFiles
        projects

      debug $ "🛬  recompile finished: " ++ show changedFiles

recompileProject :: STM.TVar [Client] -> ProjectCache -> IO [FilePath]
recompileProject mClients proj@(ProjectCache (Watchtower.Project.Project projectRoot entrypoints) cache) =
  case entrypoints of
    [] ->
      do
        Ext.Common.log "Skipping compile, no entrypoint" projectRoot
        pure []

    _ ->
        recompileChangedFile mClients entrypoints proj


recompileChangedFile :: STM.TVar [Client] -> [String] -> ProjectCache -> IO [FilePath]
recompileChangedFile mClients changedFiles projCache@(ProjectCache proj@(Watchtower.Project.Project projectRoot entrypoints) cache) =
    do
      case changedFiles of
        [] ->
          do
            pure []
        (top : remain) ->
            if List.any (\f -> Watchtower.Project.contains f proj) changedFiles then
              do

                  let entry = (NonEmpty.List top remain)
                  -- Can compileToJson take multiple entrypoints like elm make?
                  eitherStatusJson <-
                    Watchtower.Compile.Classic.compileToJson
                      projectRoot
                      entry

                  Ext.Sentry.updateCompileResult cache $
                    pure eitherStatusJson

                  case eitherStatusJson of
                    Right statusJson ->
                      do
                        Ext.Common.log "Changed file successful, recompiling project" "!"
                        -- If this file compiled successfully, compile the entire project
                        recompileProjectIfSubFile mClients changedFiles projCache
                        pure ()

                    Left errJson ->
                      do
                        Ext.Common.log "Changed file failed" "!"
                        broadcastToSubscribedProject mClients proj
                          (ElmStatus [ ProjectStatus proj False errJson ])

                        -- still recompile the entire project
                        recompileProjectIfSubFile mClients changedFiles projCache
                        pure ()

                  pure []

            else
                pure []




{-
This function will recompile a project using the projects entrypoint if the changed file is within a projects root dir.

This does mean that if the detected entrypoint doesn't ultimately import the changed file, then you won't get errors

-}
recompileProjectIfSubFile :: STM.TVar [Client] -> [String] -> ProjectCache -> IO [FilePath]
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
            -- Can compileToJson take multiple entrypoints like elm make?
            eitherStatusJson <-
              Watchtower.Compile.Classic.compileToJson
                projectRoot
                entry

            Ext.Sentry.updateCompileResult cache $
              pure eitherStatusJson

            case eitherStatusJson of
              Right statusJson ->
                do
                  Ext.Common.log "Affected project success" "--"
                  broadcastToSubscribedProject mClients proj
                    (ElmStatus [ ProjectStatus proj True statusJson ])

              Left errJson ->
                -- send the errors to any client that's listening
                do
                  Ext.Common.log "Affected project failure" "--"
                  broadcastToSubscribedProject mClients proj
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
      receiveAction state clientId action


receiveAction :: State -> ClientId -> Incoming -> IO ()
receiveAction state@(State mClients mProjects) clientId incoming =
  case incoming of
    Changed fileChanged ->
      do
        Ext.Common.log "👀 file changed" fileChanged
        recompile state [fileChanged]

    Discover root ->
      do
        Ext.Common.log "👀 discover requested" root

        discovered <- discoverProjects root


        STM.atomically $
            do
              STM.modifyTVar mProjects
                  (\projects ->
                      List.foldl
                        (\existing new ->
                          if List.any (matchingCache new) existing then
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
                        ( Set.union (Set.fromList (List.map cacheRoot discovered))
                        )
                    )
                )

        statuses <- getAllStatuses state

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
              "Changed" ->
                Changed
                  <$> (Json.Decode.field "details"
                          (Json.Decode.field "path" (Json.String.toChars <$> Json.Decode.string))
                      )
              "Discover" ->
                Discover <$> Json.Decode.field "details" decodeWatch
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
  -- watch the provided filepath, which must match a project root
  | Discover FilePath
  | Changed FilePath

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

broadcastAll :: STM.TVar [Client] -> Outgoing -> IO ()
broadcastAll allClients outgoing =
  Watchtower.Websocket.broadcastWith
    allClients
    (\c -> True)
    ( builderToString $
        encodeOutgoing outgoing
    )


broadcastTo :: STM.TVar [Client] -> ClientId -> Outgoing -> IO ()
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


broadcastToMany :: STM.TVar [Client] -> (Client -> Bool) -> Outgoing -> IO ()
broadcastToMany allClients shouldBroadcast outgoing =
  do
    Ext.Common.log "◀️" (outgoingToLog outgoing)
    Watchtower.Websocket.broadcastWith
      allClients
      shouldBroadcast
      ( builderToString $
          encodeOutgoing outgoing
      )


broadcastToSubscribedProject :: STM.TVar [Client] -> Watchtower.Project.Project -> Outgoing -> IO ()
broadcastToSubscribedProject mClients proj msg =
 broadcastToMany
      mClients
      ( \client ->
          let clientData = Watchtower.Websocket.clientData client
            in Set.member (Watchtower.Project._root proj) clientData
      )
      msg


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
