#!/usr/bin/env bash

set -euo pipefail

# Packages and publishes the VS Code extension using vsce.
# Requires vsce login/token configured.

ROOT_DIR="$(cd "$(dirname "$0")/.." && pwd)"
VS_DIR="$ROOT_DIR/apps/vscode"

pushd "$VS_DIR" >/dev/null
  if command -v bun >/dev/null 2>&1; then
    bun i
    bun run package
  else
    echo "bun not installed; attempting esbuild via npm scripts ..."
    npm i
    npm run package
  fi

  if ! command -v vsce >/dev/null 2>&1; then
    echo "vsce not found. Install with: npm i -g @vscode/vsce" >&2
    exit 1
  fi

  echo "Publishing VS Code extension ..."
  vsce publish --packagePath ./dist/elm-dev-vscode.vsix
popd >/dev/null

echo "Done publishing VS Code extension."


