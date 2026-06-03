{-# LANGUAGE OverloadedStrings #-}

module Ext.Trace.Schema
  ( schemaVersion
  , setupSql
  , pragmaSql
  , tableSql
  , migrationSql
  , indexSql
  ) where

-- | Increment when changing the SQLite trace schema in an incompatible way.
schemaVersion :: Int
schemaVersion = 1

-- | Statements to run when opening a trace database.
--
-- Keep these as individual statements because SQLite pragmas such as
-- journal_mode are connection setup, not ordinary migration body SQL.
setupSql :: [String]
setupSql =
  pragmaSql ++ tableSql ++ migrationSql ++ indexSql

pragmaSql :: [String]
pragmaSql =
  [ "PRAGMA journal_mode = WAL;"
  , "PRAGMA synchronous = NORMAL;"
  , "PRAGMA temp_store = MEMORY;"
  , "PRAGMA busy_timeout = 5000;"
  , "PRAGMA foreign_keys = ON;"
  , "PRAGMA wal_autocheckpoint = 1000;"
  ]

tableSql :: [String]
tableSql =
  [ unlines
      [ "CREATE TABLE IF NOT EXISTS trace_schema ("
      , "  version INTEGER PRIMARY KEY,"
      , "  installed_at_unix_ms INTEGER NOT NULL"
      , ");"
      ]
  , unlines
      [ "CREATE TABLE IF NOT EXISTS sessions ("
      , "  id INTEGER PRIMARY KEY,"
      , "  started_at_unix_ms INTEGER NOT NULL,"
      , "  ended_at_unix_ms INTEGER,"
      , "  process_id INTEGER NOT NULL,"
      , "  command TEXT NOT NULL,"
      , "  cwd TEXT NOT NULL,"
      , "  elm_dev_version TEXT,"
      , "  git_commit TEXT,"
      , "  metadata_json TEXT NOT NULL DEFAULT '{}' CHECK (json_valid(metadata_json))"
      , ");"
      ]
  , unlines
      [ "CREATE TABLE IF NOT EXISTS workspace_roots ("
      , "  id INTEGER PRIMARY KEY,"
      , "  session_id INTEGER NOT NULL REFERENCES sessions(id) ON DELETE CASCADE,"
      , "  root TEXT NOT NULL,"
      , "  root_origin_kind TEXT NOT NULL CHECK (root_origin_kind IN ("
      , "    'lsp.initialize.workspaceFolder',"
      , "    'lsp.initialize.rootPath',"
      , "    'lsp.initialize.rootUri',"
      , "    'dev-server.rootArgument'"
      , "  )),"
      , "  lsp_connection_id TEXT,"
      , "  lsp_workspace_folder_index INTEGER,"
      , "  lsp_workspace_folder_name TEXT,"
      , "  lsp_workspace_folder_uri TEXT,"
      , "  discovered_at_unix_ms INTEGER NOT NULL,"
      , "  metadata_json TEXT NOT NULL DEFAULT '{}' CHECK (json_valid(metadata_json))"
      , ");"
      ]
  , unlines
      [ "CREATE TABLE IF NOT EXISTS projects ("
      , "  id INTEGER PRIMARY KEY,"
      , "  session_id INTEGER NOT NULL REFERENCES sessions(id) ON DELETE CASCADE,"
      , "  workspace_root_id INTEGER REFERENCES workspace_roots(id) ON DELETE SET NULL,"
      , "  short_id INTEGER,"
      , "  root TEXT NOT NULL,"
      , "  project_root TEXT NOT NULL,"
      , "  outline_kind TEXT CHECK (outline_kind IN ('application', 'package')),"
      , "  entrypoints_json TEXT NOT NULL CHECK (json_valid(entrypoints_json)),"
      , "  src_dirs_json TEXT NOT NULL CHECK (json_valid(src_dirs_json)),"
      , "  discovered_at_unix_ms INTEGER NOT NULL,"
      , "  metadata_json TEXT NOT NULL DEFAULT '{}' CHECK (json_valid(metadata_json)),"
      , "  UNIQUE(session_id, root)"
      , ");"
      ]
  , unlines
      [ "CREATE TABLE IF NOT EXISTS spans ("
      , "  id INTEGER PRIMARY KEY,"
      , "  session_id INTEGER NOT NULL REFERENCES sessions(id) ON DELETE CASCADE,"
      , "  trace_id TEXT NOT NULL,"
      , "  parent_span_id INTEGER REFERENCES spans(id) ON DELETE SET NULL,"
      , "  workspace_root_id INTEGER REFERENCES workspace_roots(id) ON DELETE SET NULL,"
      , "  project_id INTEGER REFERENCES projects(id) ON DELETE SET NULL,"
      , "  name TEXT NOT NULL,"
      , "  category TEXT NOT NULL,"
      , "  started_at_monotonic_ns INTEGER NOT NULL,"
      , "  ended_at_monotonic_ns INTEGER,"
      , "  duration_ns INTEGER,"
      , "  status TEXT CHECK (status IN ('ok', 'error', 'cancelled', 'skipped')),"
      , "  metadata_json TEXT NOT NULL DEFAULT '{}' CHECK (json_valid(metadata_json)),"
      , "  CHECK (ended_at_monotonic_ns IS NULL OR ended_at_monotonic_ns >= started_at_monotonic_ns),"
      , "  CHECK (duration_ns IS NULL OR duration_ns >= 0)"
      , ");"
      ]
  , unlines
      [ "CREATE TABLE IF NOT EXISTS events ("
      , "  id INTEGER PRIMARY KEY,"
      , "  session_id INTEGER NOT NULL REFERENCES sessions(id) ON DELETE CASCADE,"
      , "  trace_id TEXT,"
      , "  span_id INTEGER REFERENCES spans(id) ON DELETE SET NULL,"
      , "  workspace_root_id INTEGER REFERENCES workspace_roots(id) ON DELETE SET NULL,"
      , "  project_id INTEGER REFERENCES projects(id) ON DELETE SET NULL,"
      , "  timestamp_monotonic_ns INTEGER NOT NULL,"
      , "  name TEXT NOT NULL,"
      , "  category TEXT NOT NULL,"
      , "  metadata_json TEXT NOT NULL DEFAULT '{}' CHECK (json_valid(metadata_json))"
      , ");"
      ]
  , unlines
      [ "CREATE TABLE IF NOT EXISTS measurements ("
      , "  id INTEGER PRIMARY KEY,"
      , "  session_id INTEGER NOT NULL REFERENCES sessions(id) ON DELETE CASCADE,"
      , "  trace_id TEXT,"
      , "  span_id INTEGER REFERENCES spans(id) ON DELETE SET NULL,"
      , "  workspace_root_id INTEGER REFERENCES workspace_roots(id) ON DELETE SET NULL,"
      , "  project_id INTEGER REFERENCES projects(id) ON DELETE SET NULL,"
      , "  timestamp_monotonic_ns INTEGER NOT NULL,"
      , "  name TEXT NOT NULL,"
      , "  value REAL NOT NULL,"
      , "  unit TEXT NOT NULL,"
      , "  metadata_json TEXT NOT NULL DEFAULT '{}' CHECK (json_valid(metadata_json))"
      , ");"
      ]
  , unlines
      [ "CREATE TABLE IF NOT EXISTS compile_targets ("
      , "  id INTEGER PRIMARY KEY,"
      , "  session_id INTEGER NOT NULL REFERENCES sessions(id) ON DELETE CASCADE,"
      , "  span_id INTEGER NOT NULL REFERENCES spans(id) ON DELETE CASCADE,"
      , "  project_id INTEGER NOT NULL REFERENCES projects(id) ON DELETE CASCADE,"
      , "  primary_entrypoint TEXT NOT NULL,"
      , "  entrypoints_json TEXT NOT NULL CHECK (json_valid(entrypoints_json)),"
      , "  status TEXT CHECK (status IN ('ok', 'error', 'cancelled', 'skipped')),"
      , "  duration_ns INTEGER,"
      , "  metadata_json TEXT NOT NULL DEFAULT '{}' CHECK (json_valid(metadata_json)),"
      , "  CHECK (duration_ns IS NULL OR duration_ns >= 0)"
      , ");"
      ]
  ]

migrationSql :: [String]
migrationSql =
  [ "INSERT OR IGNORE INTO trace_schema(version, installed_at_unix_ms) VALUES (1, CAST(strftime('%s', 'now') AS INTEGER) * 1000);"
  ]

indexSql :: [String]
indexSql =
  [ "CREATE INDEX IF NOT EXISTS workspace_roots_session_root_idx ON workspace_roots(session_id, root);"
  , "CREATE INDEX IF NOT EXISTS projects_session_root_idx ON projects(session_id, root);"
  , "CREATE INDEX IF NOT EXISTS projects_session_project_root_idx ON projects(session_id, project_root);"
  , "CREATE INDEX IF NOT EXISTS spans_session_name_idx ON spans(session_id, name);"
  , "CREATE INDEX IF NOT EXISTS spans_session_category_idx ON spans(session_id, category);"
  , "CREATE INDEX IF NOT EXISTS spans_trace_idx ON spans(trace_id);"
  , "CREATE INDEX IF NOT EXISTS spans_project_duration_idx ON spans(project_id, duration_ns);"
  , "CREATE INDEX IF NOT EXISTS events_session_name_idx ON events(session_id, name);"
  , "CREATE INDEX IF NOT EXISTS measurements_session_name_idx ON measurements(session_id, name);"
  , "CREATE INDEX IF NOT EXISTS compile_targets_project_entrypoint_idx ON compile_targets(project_id, primary_entrypoint);"
  ]
