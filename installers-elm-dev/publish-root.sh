#!/usr/bin/env bash

set -euo pipefail

# Publishes the root npm package after per-platform packages are live.
# Requires npm auth configured with publish rights.

ROOT_DIR="$(cd "$(dirname "$0")/npm" && pwd)"

pushd "$ROOT_DIR" >/dev/null
  echo "Running prepack to ensure ts-tool is included ..."
  node scripts/prepack.js
  echo "Publishing elm-dev root package ..."
  npm publish --access public
popd >/dev/null

echo "Done publishing root package."


