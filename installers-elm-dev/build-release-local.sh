#!/usr/bin/env bash

set -euo pipefail

# Unified local build + package orchestrator (no CI):
# 1) Build Rust proxy for all targets (runs from apps/proxy)
# 2) Download Haskell elm-dev binaries
# 3) Package ts-tool into root npm package via prepack
# 4) Prepare VS Code extension build

ROOT_DIR="$(cd "$(dirname "$0")/.." && pwd)"
NPM_DIR="$ROOT_DIR/installers-elm-dev/npm"
VS_DIR="$ROOT_DIR/apps/vscode"

echo "[1/4] Building Rust proxy for all targets ..."
"$ROOT_DIR/scripts/build-proxy-binaries.sh"

echo "[2/4] Downloading elm-dev binaries ..."
pushd "$NPM_DIR" >/dev/null
  if [[ "${SKIP_BINARY_DOWNLOAD:-}" == "1" ]]; then
    echo "SKIP_BINARY_DOWNLOAD=1 set; skipping binary download. Ensure packages/* contain current elm-dev binaries."
  elif [[ -x "scripts/download-binaries.sh" ]]; then
    ./scripts/download-binaries.sh
  else
    echo "download-binaries.sh not found or not executable." >&2
    exit 1
  fi
popd >/dev/null

echo "[3/4] Packaging ts-tool into npm root via prepack ..."
pushd "$NPM_DIR" >/dev/null
  node scripts/prepack.js
popd >/dev/null

echo "[4/4] Preparing VS Code extension build ..."
pushd "$VS_DIR" >/dev/null
  if command -v bun >/dev/null 2>&1; then
    bun i
    bun run build
  else
    echo "bun not installed; install bun or build extension manually."
  fi
popd >/dev/null

echo "Done. Next steps: publish platform packages, root package, and VS Code extension."

