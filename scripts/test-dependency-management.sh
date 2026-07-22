#!/usr/bin/env bash

set -euo pipefail

if [[ "${ELM_DEV_RUN_NETWORK_TESTS:-}" != "1" ]]; then
  echo "Skipping dependency network smoke tests. Set ELM_DEV_RUN_NETWORK_TESTS=1 to run them."
  exit 0
fi

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
BIN="$(cd "$ROOT" && cabal list-bin exe:elm-dev)"
WORK="$(mktemp -d "${TMPDIR:-/tmp}/elm-dev-dependencies.XXXXXX")"
export ELM_DEV_HOME="$WORK/home"

cleanup() {
  rm -rf "$WORK"
}
trap cleanup EXIT

APP="$WORK/application"
PACKAGE="$WORK/package"
mkdir -p "$APP/src" "$PACKAGE/src"

cat > "$APP/elm.json" <<'JSON'
{
  "type": "application",
  "source-directories": ["src"],
  "elm-version": "0.19.1",
  "dependencies": {
    "direct": {
      "elm/core": "1.0.5",
      "elm/json": "1.1.3"
    },
    "indirect": {}
  },
  "test-dependencies": {
    "direct": {},
    "indirect": {}
  }
}
JSON

cat > "$PACKAGE/elm.json" <<'JSON'
{
  "type": "package",
  "name": "author/dependency-smoke-test",
  "summary": "Dependency management smoke-test fixture",
  "license": "BSD-3-Clause",
  "version": "1.0.0",
  "exposed-modules": ["Smoke"],
  "elm-version": "0.19.0 <= v < 0.20.0",
  "dependencies": {
    "elm/core": "1.0.0 <= v < 2.0.0"
  },
  "test-dependencies": {}
}
JSON

cat > "$PACKAGE/src/Smoke.elm" <<'ELM'
module Smoke exposing (value)

value : Int
value =
    1
ELM

run_in() {
  local directory="$1"
  shift
  (cd "$directory" && "$BIN" "$@")
}

echo "Cold-cache application dry run"
cp "$APP/elm.json" "$APP/elm.json.before"
dry_run="$(run_in "$APP" install elm/http --dry-run --format=json)"
grep -q '"status":"planned"' <<<"$dry_run"
grep -q '"written":false' <<<"$dry_run"
cmp "$APP/elm.json.before" "$APP/elm.json"

echo "Application install, test install, tree, upgrade, and uninstall"
grep -q '"status":"applied"' <<<"$(run_in "$APP" install elm/http --yes --format=json)"
grep -q '"status":"applied"' <<<"$(run_in "$APP" test install elm-explorations/test --yes --format=json)"
grep -q '"scope":"production"' <<<"$(run_in "$APP" dep tree --format=json)"
grep -q '"operation":"upgrade"' <<<"$(run_in "$APP" upgrade elm/http --dry-run --format=json)"
grep -q '"status":"applied"' <<<"$(run_in "$APP" uninstall elm/http --yes --format=json)"

echo "Package major install, exact rejection, tree, and uninstall"
grep -q '"status":"applied"' <<<"$(run_in "$PACKAGE" install elm/json@1 --yes --format=json)"
if run_in "$PACKAGE" install elm/json@1.1.3 --yes --format=json >"$WORK/exact.out" 2>"$WORK/exact.err"; then
  echo "Expected package exact requirement to fail" >&2
  exit 1
fi
grep -q 'unsupported-package-exact-requirement' "$WORK/exact.out"
grep -q '"scope":"production"' <<<"$(run_in "$PACKAGE" dep tree --format=json)"
grep -q '"status":"applied"' <<<"$(run_in "$PACKAGE" uninstall elm/json --yes --format=json)"

echo "Warm-cache offline fallback"
export HTTPS_PROXY="http://127.0.0.1:1"
export HTTP_PROXY="http://127.0.0.1:1"
grep -q '"status":"applied"' <<<"$(run_in "$APP" install elm/http --yes --format=json)"
grep -q '"scope":"production"' <<<"$(run_in "$APP" dep tree --format=json)"

echo "Dependency management network smoke tests passed."
