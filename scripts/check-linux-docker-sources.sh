#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "$0")/.." && pwd)"
DOCKERFILE="$ROOT_DIR/distribution/docker/x86_64-musl.dockerfile"

source_dirs=(
  builder
  compiler
  terminal
  ext-common
  ext-debug
  ext-dev
  ext-generate
  ext-optimization
  ext-sentry
  ext-trace
  ext-watchtower
)

for source_dir in "${source_dirs[@]}"; do
  if ! grep -Eq "^COPY ${source_dir}( |/)" "$DOCKERFILE"; then
    echo "Linux Docker build context is missing Cabal source directory: $source_dir" >&2
    exit 1
  fi
done

echo "Linux Docker build context includes all Cabal source directories"
