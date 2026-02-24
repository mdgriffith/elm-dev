#!/usr/bin/env bash

set -euo pipefail

repo_dir="/Users/griff/projects/elm-dev"
original_dir="$PWD"
stack_bin="${STACK_BIN:-$(command -v stack || true)}"

# Keep stdio clean for JSON-RPC (MCP/LSP): disable Elm Dev debug env flags
unset ElmDevWithLabels
unset ElmDevPerformance
unset ElmDevPerformanceTiming
unset ElmDevLive
unset ElmDevQuestions
unset ElmDevTest
unset ElmDevVerboseServer
unset ElmDevMemoryCache
unset ElmDevFileProxy
unset ElmDevFileWatch
unset ElmDevElmCompilerInfo
unset ElmDevElmCompilerError
unset ElmDevMisc
unset ElmDevLSP
unset ElmDevDeps

if [ "$#" -lt 1 ]; then
  echo "Usage: $0 <elm-dev-subcommand> [args...]" >&2
  echo "Example: $0 mcp" >&2
  exit 2
fi

if [ -z "$stack_bin" ]; then
  echo "error: stack not found on PATH" >&2
  echo "hint: set STACK_BIN to an absolute stack path in OpenCode MCP environment" >&2
  exit 127
fi

cd "$repo_dir"

# Prefer `stack exec` for fast startup (no rebuild checks like `stack run`).
# If the binary has not been built yet, fall back to `stack run`.
if "$stack_bin" exec --cwd "$original_dir" elm-dev -- "$@"; then
  exit 0
fi

"$stack_bin" run --cwd "$original_dir" -- "$@"
