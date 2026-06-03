{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
SQLite-backed tracing for local elm-dev performance diagnostics.

Tracing is opt-in. If @ELM_DEV_TRACE_DB@ is not set, 'setupFromEnv' returns a
no-op 'Trace', so call sites do not need to branch on whether tracing is enabled.

Common setup:

@
traceHandle <- Ext.Trace.setupFromEnv
@ 

In Watchtower, this is owned by @Client.State@ and accessed with
@Client.trace state@ so tracing does not have to be threaded through every
function by hand.

Common point-in-time event:

@
Ext.Trace.event (Client.trace state) "lsp.didChange"
  [ Ext.Trace.text "path" filePath
  , Ext.Trace.int "edit_count" (length changes)
  ]
@ 

Common timed span:

@
Ext.Trace.span (Client.trace state) "compile.project"
  [ Ext.Trace.text "project_root" projectRoot
  , Ext.Trace.int "changed_file_count" (length files)
  ]
  $ do
    compileProject
@ 

To record traces for a process:

@
ELM_DEV_TRACE_DB=/tmp/elm-dev-trace.sqlite elm-dev server
@ 
-}
module Ext.Trace
  ( Trace
  , Attribute
  , RootOrigin(..)
  , setupFromEnv
  , shutdown
  , workspaceRoot
  , project
  , event
  , span
  , text
  , int
  , bool
  , json
  ) where

import Prelude hiding (span)

import Control.Applicative ((<|>))
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Exception as Exception
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Int as Int
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Time.Clock.POSIX as POSIX
import qualified Database.SQLite.Simple as SQLite
import qualified Ext.Trace.Schema as Schema
import qualified System.Clock as Clock
import qualified System.Directory as Directory
import qualified System.Environment as Environment
import qualified System.Process as Process

data Trace
  = NoTrace
  | SQLiteTrace SQLiteState

data SQLiteState = SQLiteState
  { sqliteConnection :: MVar.MVar SQLite.Connection
  , sqliteSessionId :: Int.Int64
  , sqliteActiveSpans :: MVar.MVar (Map.Map Concurrent.ThreadId [ActiveSpan])
  }

data ActiveSpan = ActiveSpan
  { activeSpanId :: Int.Int64
  , activeTraceId :: Text.Text
  }

newtype Attribute = Attribute (Text.Text, Aeson.Value)

data RootOrigin
  = LspInitializeWorkspaceFolder
      { workspaceFolderIndex :: Int
      , workspaceFolderName :: Text.Text
      , workspaceFolderUri :: Text.Text
      }
  | LspInitializeRootPath
  | LspInitializeRootUri
  | DevServerRootArgument

setupFromEnv :: IO Trace
setupFromEnv = do
  maybePath <- Environment.lookupEnv "ELM_DEV_TRACE_DB"
  case maybePath of
    Nothing ->
      pure NoTrace

    Just path ->
      setupSQLite path

shutdown :: Trace -> IO ()
shutdown trace =
  case trace of
    NoTrace ->
      pure ()

    SQLiteTrace sqlite ->
      MVar.withMVar (sqliteConnection sqlite) $ \conn -> do
        now <- unixMillis
        SQLite.execute conn "UPDATE sessions SET ended_at_unix_ms = ? WHERE id = ?" (now, sqliteSessionId sqlite)
        SQLite.close conn

workspaceRoot :: Trace -> RootOrigin -> FilePath -> [Attribute] -> IO ()
workspaceRoot trace origin root attrs =
  case trace of
    NoTrace ->
      pure ()

    SQLiteTrace sqlite ->
      MVar.withMVar (sqliteConnection sqlite) $ \conn -> do
        now <- unixMillis
        SQLite.execute
          conn
          "INSERT INTO workspace_roots(session_id, root, root_origin_kind, lsp_workspace_folder_index, lsp_workspace_folder_name, lsp_workspace_folder_uri, discovered_at_unix_ms, metadata_json) VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
          ( sqliteSessionId sqlite
          , root
          , rootOriginKind origin
          , rootOriginWorkspaceFolderIndex origin
          , rootOriginWorkspaceFolderName origin
          , rootOriginWorkspaceFolderUri origin
          , now
          , attributesJson attrs
          )

project :: Trace -> FilePath -> FilePath -> [FilePath] -> [FilePath] -> Int -> [Attribute] -> IO ()
project trace root projectRoot entrypoints srcDirs shortId attrs =
  case trace of
    NoTrace ->
      pure ()

    SQLiteTrace sqlite ->
      MVar.withMVar (sqliteConnection sqlite) $ \conn -> do
        now <- unixMillis
        workspaceRootId <- lookupWorkspaceRootId conn (sqliteSessionId sqlite) projectRoot
        SQLite.execute
          conn
          "INSERT INTO projects(session_id, workspace_root_id, short_id, root, project_root, entrypoints_json, src_dirs_json, discovered_at_unix_ms, metadata_json) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?) ON CONFLICT(session_id, root) DO UPDATE SET workspace_root_id = excluded.workspace_root_id, short_id = excluded.short_id, project_root = excluded.project_root, entrypoints_json = excluded.entrypoints_json, src_dirs_json = excluded.src_dirs_json, metadata_json = excluded.metadata_json"
          ( sqliteSessionId sqlite
          , workspaceRootId
          , shortId
          , root
          , projectRoot
          , jsonText entrypoints
          , jsonText srcDirs
          , now
          , attributesJson attrs
          )

event :: Trace -> Text.Text -> [Attribute] -> IO ()
event trace name attrs =
  case trace of
    NoTrace ->
      pure ()

    SQLiteTrace sqlite ->
      MVar.withMVar (sqliteConnection sqlite) $ \conn -> do
        now <- monotonicNanos
        active <- currentActiveSpan sqlite
        let traceId = traceIdFromAttributes attrs <|> fmap activeTraceId active
            spanId = fmap activeSpanId active
        projectId <- lookupProjectIdForAttributes conn (sqliteSessionId sqlite) attrs
        SQLite.execute
          conn
          "INSERT INTO events(session_id, trace_id, span_id, project_id, timestamp_monotonic_ns, name, category, metadata_json) VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
          ( sqliteSessionId sqlite
          , traceId
          , spanId
          , projectId
          , now
          , name
          , categoryFromName name
          , attributesJson attrs
          )

span :: Trace -> Text.Text -> [Attribute] -> IO a -> IO a
span trace name attrs action =
  case trace of
    NoTrace ->
      action

    SQLiteTrace sqlite ->
      Exception.mask $ \restore -> do
        started <- monotonicNanos
        (spanId, traceId) <- MVar.withMVar (sqliteConnection sqlite) $ \conn -> do
          projectId <- lookupProjectIdForAttributes conn (sqliteSessionId sqlite) attrs
          let traceId = Maybe.fromMaybe name (traceIdFromAttributes attrs)
          activeParentSpanId <- currentParentSpanId sqlite traceId
          openParentSpanId <- lookupOpenParentSpanId conn (sqliteSessionId sqlite) traceId
          let parentSpanId = activeParentSpanId <|> openParentSpanId
          SQLite.execute
            conn
            "INSERT INTO spans(session_id, trace_id, parent_span_id, project_id, name, category, started_at_monotonic_ns, metadata_json) VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
            ( sqliteSessionId sqlite
            , traceId
            , parentSpanId
            , projectId
            , name
            , categoryFromName name
            , started
            , attributesJson attrs
            )
          insertedId <- SQLite.lastInsertRowId conn
          pure (insertedId, traceId)
        pushActiveSpan sqlite (ActiveSpan spanId traceId)
        result <- Exception.try (restore action)
        popActiveSpan sqlite
        ended <- monotonicNanos
        let status =
              case result of
                Right _ -> "ok" :: Text.Text
                Left (_ :: Exception.SomeException) -> "error"
            duration = ended - started
        MVar.withMVar (sqliteConnection sqlite) $ \conn ->
          SQLite.execute
            conn
            "UPDATE spans SET ended_at_monotonic_ns = ?, duration_ns = ?, status = ? WHERE id = ?"
            (ended, duration, status, spanId)
        case result of
          Right value ->
            pure value

          Left err ->
            Exception.throwIO err

text :: Text.Text -> String -> Attribute
text key value =
  Attribute (key, Aeson.String (Text.pack value))

int :: Text.Text -> Int -> Attribute
int key value =
  Attribute (key, Aeson.Number (fromIntegral value))

bool :: Text.Text -> Bool -> Attribute
bool key value =
  Attribute (key, Aeson.Bool value)

json :: Text.Text -> Aeson.Value -> Attribute
json key value =
  Attribute (key, value)

rootOriginKind :: RootOrigin -> Text.Text
rootOriginKind origin =
  case origin of
    LspInitializeWorkspaceFolder _ _ _ ->
      "lsp.initialize.workspaceFolder"

    LspInitializeRootPath ->
      "lsp.initialize.rootPath"

    LspInitializeRootUri ->
      "lsp.initialize.rootUri"

    DevServerRootArgument ->
      "dev-server.rootArgument"

rootOriginWorkspaceFolderIndex :: RootOrigin -> Maybe Int
rootOriginWorkspaceFolderIndex origin =
  case origin of
    LspInitializeWorkspaceFolder ix _ _ ->
      Just ix

    _ ->
      Nothing

rootOriginWorkspaceFolderName :: RootOrigin -> Maybe Text.Text
rootOriginWorkspaceFolderName origin =
  case origin of
    LspInitializeWorkspaceFolder _ name _ ->
      Just name

    _ ->
      Nothing

rootOriginWorkspaceFolderUri :: RootOrigin -> Maybe Text.Text
rootOriginWorkspaceFolderUri origin =
  case origin of
    LspInitializeWorkspaceFolder _ _ uri ->
      Just uri

    _ ->
      Nothing

setupSQLite :: FilePath -> IO Trace
setupSQLite path = do
  conn <- SQLite.open path
  mapM_ (SQLite.execute_ conn . SQLite.Query . Text.pack) Schema.setupSql
  now <- unixMillis
  pid <- fromIntegral <$> Process.getCurrentPid
  cwd <- Directory.getCurrentDirectory
  command <- unwords <$> Environment.getArgs
  SQLite.execute
    conn
    "INSERT INTO sessions(started_at_unix_ms, process_id, command, cwd) VALUES (?, ?, ?, ?)"
    ( now
    , pid :: Int
    , command
    , cwd
    )
  sessionId <- SQLite.lastInsertRowId conn
  SQLiteTrace <$> (SQLiteState <$> MVar.newMVar conn <*> pure sessionId <*> MVar.newMVar Map.empty)

attributesJson :: [Attribute] -> Text.Text
attributesJson attrs =
  Text.Encoding.decodeUtf8 (LBS.toStrict (Aeson.encode (Aeson.object (map toPair attrs))))
  where
    toPair (Attribute (key, value)) =
      AesonKey.fromText key Aeson..= value

jsonText :: [FilePath] -> Text.Text
jsonText values =
  Text.Encoding.decodeUtf8 (LBS.toStrict (Aeson.encode values))

lookupTextAttribute :: Text.Text -> [Attribute] -> Maybe Text.Text
lookupTextAttribute key attrs =
  Maybe.listToMaybe
    [ value
    | Attribute (attrKey, Aeson.String value) <- attrs
    , attrKey == key
      ]

traceIdFromAttributes :: [Attribute] -> Maybe Text.Text
traceIdFromAttributes attrs =
  lookupTextAttribute "trace_id" attrs
    <|> lspRequestTraceId attrs
    <|> lspNotificationTraceId attrs
  where
    lspRequestTraceId values = do
      method <- lookupTextAttribute "method" values
      requestId <- lookupTextAttribute "request_id" values
      pure ("lsp.request:" <> method <> "#" <> requestId)

    lspNotificationTraceId values = do
      method <- lookupTextAttribute "method" values
      pure ("lsp.notification:" <> method)

currentActiveSpan :: SQLiteState -> IO (Maybe ActiveSpan)
currentActiveSpan sqlite = do
  threadId <- Concurrent.myThreadId
  activeByThread <- MVar.readMVar (sqliteActiveSpans sqlite)
  pure (Map.lookup threadId activeByThread >>= Maybe.listToMaybe)

currentParentSpanId :: SQLiteState -> Text.Text -> IO (Maybe Int.Int64)
currentParentSpanId sqlite traceId = do
  active <- currentActiveSpan sqlite
  pure $ case active of
    Just parent | activeTraceId parent == traceId ->
      Just (activeSpanId parent)

    _ ->
      Nothing

pushActiveSpan :: SQLiteState -> ActiveSpan -> IO ()
pushActiveSpan sqlite active = do
  threadId <- Concurrent.myThreadId
  MVar.modifyMVar_ (sqliteActiveSpans sqlite) $ \activeByThread ->
    pure (Map.insertWith (++) threadId [active] activeByThread)

popActiveSpan :: SQLiteState -> IO ()
popActiveSpan sqlite = do
  threadId <- Concurrent.myThreadId
  MVar.modifyMVar_ (sqliteActiveSpans sqlite) $ \activeByThread ->
    pure $ case Map.lookup threadId activeByThread of
      Just (_:rest) | not (null rest) ->
        Map.insert threadId rest activeByThread

      Just _ ->
        Map.delete threadId activeByThread

      Nothing ->
        activeByThread

lookupProjectIdForAttributes :: SQLite.Connection -> Int.Int64 -> [Attribute] -> IO (Maybe Int.Int64)
lookupProjectIdForAttributes conn sessionId attrs =
  case lookupTextAttribute "elm_json_root" attrs <|> lookupTextAttribute "project_root" attrs of
    Nothing ->
      pure Nothing

    Just root ->
      lookupProjectId conn sessionId root

lookupProjectId :: SQLite.Connection -> Int.Int64 -> Text.Text -> IO (Maybe Int.Int64)
lookupProjectId conn sessionId root = do
  rows <- SQLite.query conn "SELECT id FROM projects WHERE session_id = ? AND (root = ? OR project_root = ?) LIMIT 1" (sessionId, root, root) :: IO [SQLite.Only Int.Int64]
  case rows of
    SQLite.Only projectId : _ ->
      pure (Just projectId)

    [] ->
      pure Nothing

lookupOpenParentSpanId :: SQLite.Connection -> Int.Int64 -> Text.Text -> IO (Maybe Int.Int64)
lookupOpenParentSpanId conn sessionId traceId = do
  rows <- SQLite.query conn "SELECT id FROM spans WHERE session_id = ? AND trace_id = ? AND ended_at_monotonic_ns IS NULL ORDER BY started_at_monotonic_ns DESC LIMIT 1" (sessionId, traceId) :: IO [SQLite.Only Int.Int64]
  case rows of
    SQLite.Only spanId : _ ->
      pure (Just spanId)

    [] ->
      pure Nothing

lookupWorkspaceRootId :: SQLite.Connection -> Int.Int64 -> FilePath -> IO (Maybe Int.Int64)
lookupWorkspaceRootId conn sessionId projectRoot = do
  rows <- SQLite.query conn "SELECT id, root FROM workspace_roots WHERE session_id = ? ORDER BY length(root) DESC" (SQLite.Only sessionId) :: IO [(Int.Int64, FilePath)]
  pure (fmap fst (Maybe.listToMaybe (filter (\(_, root) -> root `isPathPrefixOf` projectRoot) rows)))

isPathPrefixOf :: FilePath -> FilePath -> Bool
isPathPrefixOf prefix path =
  prefix == path || Text.pack (prefix <> "/") `Text.isPrefixOf` Text.pack path

categoryFromName :: Text.Text -> Text.Text
categoryFromName name =
  case Text.breakOn "." name of
    ("", _) ->
      "event"

    (category, _) ->
      category

unixMillis :: IO Int.Int64
unixMillis = do
  seconds <- POSIX.getPOSIXTime
  pure (round (seconds * 1000))

monotonicNanos :: IO Int.Int64
monotonicNanos = do
  time <- Clock.getTime Clock.Monotonic
  pure (fromIntegral (Clock.toNanoSecs time))
