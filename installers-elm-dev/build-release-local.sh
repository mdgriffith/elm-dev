#!/usr/bin/env bash

set -euo pipefail

# Unified local build + package orchestrator (no CI):
# 1) Build Rust proxy for all targets (runs from apps/proxy)
# 2) Download Haskell elm-dev binaries (or rely on local placement)
# 3) Package ts-tool into root npm package via prepack
# 4) Prepare VS Code extension build

ROOT_DIR="$(cd "$(dirname "$0")/.." && pwd)"
NPM_DIR="$ROOT_DIR/installers-elm-dev/npm"
PROXY_DIR="$ROOT_DIR/apps/proxy"
VS_DIR="$ROOT_DIR/apps/vscode"

echo "[1/4] Building Rust proxy for all targets ..."
pushd "$PROXY_DIR" >/dev/null
  ./build_all_for_release.sh
popd >/dev/null

echo "[2/4] Downloading elm-dev binaries (optional if you built locally) ..."
pushd "$NPM_DIR" >/dev/null
  if [[ -x "scripts/download-binaries.sh" ]]; then
    ./scripts/download-binaries.sh || true
  else
    echo "download-binaries.sh not found/executable, skipping. Ensure packages/* contain elm-dev binaries."
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


