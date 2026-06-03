module Ext.Trace.Types
  ( RootOriginKind(..)
  , rootOriginKindSql
  , SpanStatus(..)
  , spanStatusSql
  ) where

data RootOriginKind
  = LspInitializeWorkspaceFolder
  | LspInitializeRootPath
  | LspInitializeRootUri
  | DevServerRootArgument
  deriving (Eq, Show)

rootOriginKindSql :: RootOriginKind -> String
rootOriginKindSql kind =
  case kind of
    LspInitializeWorkspaceFolder ->
      "lsp.initialize.workspaceFolder"

    LspInitializeRootPath ->
      "lsp.initialize.rootPath"

    LspInitializeRootUri ->
      "lsp.initialize.rootUri"

    DevServerRootArgument ->
      "dev-server.rootArgument"

data SpanStatus
  = SpanOk
  | SpanError
  | SpanCancelled
  | SpanSkipped
  deriving (Eq, Show)

spanStatusSql :: SpanStatus -> String
spanStatusSql status =
  case status of
    SpanOk ->
      "ok"

    SpanError ->
      "error"

    SpanCancelled ->
      "cancelled"

    SpanSkipped ->
      "skipped"
