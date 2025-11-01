#!/usr/bin/env bash

set -euo pipefail

# Build Elm generator assets used by the Haskell project.
# This is the canonical generator build script; do not call another script.
# Run from repo root or anywhere; uses known paths.

ROOT_DIR="$(cd "$(dirname "$0")/.." && pwd)"
GEN_DIR="$ROOT_DIR/ext-generate/generator"

pushd "$GEN_DIR" >/dev/null
  # Prepare codegen
  cd codegen
  rm -rf Gen
  elm-codegen install
  cd ..

  # Typecheck
  bun run typecheck

  # Compile generator artifacts
  rm -rf dist
  echo "Compiling runner in $PWD"
  elm make main/Run.elm --output=dist/generate.js --optimize
  echo "Compiling interactive generator..."
  (cd dev; elm make Generate.elm --output=../dist/interactive-generate.js --optimize)
  bun build ./index.ts --outfile=dist/run.js
  bun build ./interactiveIndex.ts --outfile=dist/interactive-run.js

  # Copy built JS to embedded location
  mkdir -p ../Gen/js
  cp ./dist/run.js ../Gen/js/run.js
  cp ./dist/interactive-run.js ../Gen/js/interactive-run.js

  # Remove intermediates
  rm ./dist/generate.js || true
  rm ./dist/interactive-generate.js || true

  # Update embedded version in Haskell sources
  RANDOM_VERSION=$(openssl rand -hex 8)
  sed -i '' "s/version = \".*\"/version = \"$RANDOM_VERSION\"/" ../Gen/Javascript.hs
  sed -i '' "s/version = \".*\"/version = \"$RANDOM_VERSION\"/" ../Gen/Templates.hs
popd >/dev/null

echo "Generator assets built under ext-generate/generator/dist and embedded sources updated."


