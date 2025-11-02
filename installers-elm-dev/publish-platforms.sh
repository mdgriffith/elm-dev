#!/usr/bin/env bash

set -euo pipefail

# Publishes per-platform npm packages in the required order.
# Requires npm auth configured with publish rights.

ROOT_DIR="$(cd "$(dirname "$0")" && pwd)"
PKGS_DIR="$ROOT_DIR/npm/packages"

packages=(
  "darwin_arm64"
  "darwin_x64"
  "linux_arm64"
  "linux_x64"
  "win32_x64"
)

for p in "${packages[@]}"; do
  echo "Publishing @elm_dev_binaries/${p} ..."
  pushd "$PKGS_DIR/$p" >/dev/null
    npm publish --access public
  popd >/dev/null
done

echo "Done publishing per-platform packages."


