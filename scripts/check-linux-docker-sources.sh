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

if ! grep -Fq 'COPY stack.yaml.lock ./' "$DOCKERFILE"; then
  echo "Linux x86_64 Docker build does not copy the Stack lockfile" >&2
  exit 1
fi

if grep -Fq 'cabal update' "$DOCKERFILE"; then
  echo "Linux x86_64 Docker build must not update through Cabal" >&2
  exit 1
fi

if ! grep -Fq 'stack $STACKOPTS install' "$DOCKERFILE"; then
  echo "Linux x86_64 Docker build does not build with Stack" >&2
  exit 1
fi

ARM64_SCRIPT="$ROOT_DIR/distribution/build-linux-arm64-musl.sh"

if grep -Fq 'cabal update' "$ARM64_SCRIPT"; then
  echo "Linux arm64 Docker build must not update through Cabal" >&2
  exit 1
fi

if ! grep -Fq 'stack $STACKOPTS install' "$ARM64_SCRIPT"; then
  echo "Linux arm64 Docker build does not build with Stack" >&2
  exit 1
fi

if ! grep -Fq 'statically linked' "$DOCKERFILE" || ! grep -Fq 'statically linked' "$ARM64_SCRIPT"; then
  echo "Linux Docker builds do not verify static linkage" >&2
  exit 1
fi

STACK_LINUX_CONFIG="$ROOT_DIR/distribution/stack-linux-config.yaml"

if ! grep -Fq -- '--enable-executable-static' "$STACK_LINUX_CONFIG"; then
  echo "Linux Stack configuration does not enable fully static executables" >&2
  exit 1
fi

if ! grep -Fq 'stack-linux-config.yaml' "$DOCKERFILE" || ! grep -Fq 'stack-linux-config.yaml' "$ARM64_SCRIPT"; then
  echo "Linux Docker builds do not load the static Stack configuration" >&2
  exit 1
fi

echo "Linux Docker builds use locked Stack dependencies and verify static linkage"
