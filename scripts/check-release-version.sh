#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "$0")/.." && pwd)"
VERSION="$(node -p "require('./installers-elm-dev/npm/package.json').version" 2>/dev/null)"

check_value() {
  local label="$1"
  local actual="$2"

  if [[ "$actual" != "$VERSION" ]]; then
    echo "Version mismatch: $label is $actual, expected $VERSION" >&2
    exit 1
  fi
}

cd "$ROOT_DIR"

check_value "elm-dev.cabal" "$(awk '/^Version:/ { print $2; exit }' elm-dev.cabal)"
check_value "cabal.project.freeze" "$(awk '/elm-dev ==/ { sub(/.*==/, ""); print; exit }' cabal.project.freeze)"
check_value "distribution/common.sh" "$(awk -F'"' '/^export version=/ { print $2; exit }' distribution/common.sh)"
check_value "installers-elm-dev/npm/scripts/download-binaries.sh" "$(awk -F'"' '/^VERSION=/ { print $2; exit }' installers-elm-dev/npm/scripts/download-binaries.sh)"

for package_json in installers-elm-dev/npm/packages/*/package.json; do
  check_value "$package_json" "$(node -p "require('./$package_json').version")"
done

echo "Release version metadata is aligned at $VERSION"
