#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "$0")/.." && pwd)"
PASSES=3
BUILD=1
FIXTURE="all"

usage() {
  cat <<'EOF'
Usage: scripts/bench-compile-modes.sh [options]

Bench compile times for debug, dev, --optimize, -O2, and -O3.

Options:
  --passes N          Number of passes per fixture (default: 3)
  --fixture NAME      Fixture to run: all, app, generator (default: all)
  --no-build          Reuse an already-built elm-dev executable
  -h, --help          Show this help
EOF
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --passes)
      PASSES="${2:-}"
      shift 2
      ;;
    --fixture)
      FIXTURE="${2:-}"
      shift 2
      ;;
    --no-build)
      BUILD=0
      shift
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      echo "Unknown option: $1" >&2
      usage >&2
      exit 1
      ;;
  esac
done

case "$PASSES" in
  ''|*[!0-9]*)
    echo "--passes must be a positive integer" >&2
    exit 1
    ;;
esac

if [[ "$PASSES" -lt 1 ]]; then
  echo "--passes must be at least 1" >&2
  exit 1
fi

case "$FIXTURE" in
  all|app|generator) ;;
  *)
    echo "--fixture must be one of: all, app, generator" >&2
    exit 1
    ;;
esac

if [[ "$BUILD" -eq 1 ]]; then
  (cd "$ROOT_DIR" && cabal build exe:elm-dev >/dev/null)
fi

BIN="$(cd "$ROOT_DIR" && cabal list-bin exe:elm-dev)"
RUN_ROOT="${TMPDIR:-/tmp}/elm-dev-compile-metrics-$(date +%s)"
mkdir -p "$RUN_ROOT"

now_ms() {
  perl -MTime::HiRes=time -e 'print int(time() * 1000)'
}

copy_fixture() {
  local fixture="$1"
  local dest="$2"

  mkdir -p "$dest"

  case "$fixture" in
    app)
      local src="$ROOT_DIR/apps/elm-dev"
      mkdir -p "$dest/elm-stuff"
      cp "$src/elm.json" "$dest/elm.json"
      cp -R "$src/src" "$dest/src"
      cp -R "$src/elm-stuff/generated" "$dest/elm-stuff/generated"
      ;;
    generator)
      local src="$ROOT_DIR/ext-generate/generator"
      cp "$src/elm.json" "$dest/elm.json"
      cp -R "$src/main" "$dest/main"
      cp -R "$src/app" "$dest/app"
      cp -R "$src/assets" "$dest/assets"
      cp -R "$src/theme" "$dest/theme"
      cp -R "$src/codegen" "$dest/codegen"
      ;;
  esac
}

entrypoint_for() {
  case "$1" in
    app) echo "src/app/Main.elm" ;;
    generator) echo "main/Run.elm" ;;
  esac
}

mode_flag_for() {
  case "$1" in
    debug) echo "--debug" ;;
    dev) echo "" ;;
    optimize) echo "--optimize" ;;
    O2) echo "-O2" ;;
    O3) echo "-O3" ;;
  esac
}

run_compile() {
  local project_dir="$1"
  local entrypoint="$2"
  local mode="$3"
  local output="$4"
  local flag
  flag="$(mode_flag_for "$mode")"

  if [[ -n "$flag" ]]; then
    (cd "$project_dir" && "$BIN" make "$entrypoint" "$flag" --output="$output" >/dev/null)
  else
    (cd "$project_dir" && "$BIN" make "$entrypoint" --output="$output" >/dev/null)
  fi
}

run_fixture() {
  local fixture="$1"
  local entrypoint
  entrypoint="$(entrypoint_for "$fixture")"

  echo
  echo "## $fixture"
  echo
  echo "| pass | mode | cold ms | warm ms | bytes |"
  echo "|---:|---|---:|---:|---:|"

  for pass in $(seq 1 "$PASSES"); do
    for mode in debug dev optimize O2 O3; do
      local project_dir="$RUN_ROOT/$fixture-pass-$pass-$mode"
      local start end cold warm bytes

      copy_fixture "$fixture" "$project_dir"

      start="$(now_ms)"
      run_compile "$project_dir" "$entrypoint" "$mode" "$project_dir/out.js"
      end="$(now_ms)"
      cold=$((end - start))

      start="$(now_ms)"
      run_compile "$project_dir" "$entrypoint" "$mode" "$project_dir/out-warm.js"
      end="$(now_ms)"
      warm=$((end - start))

      bytes="$(wc -c < "$project_dir/out.js" | tr -d ' ')"

      echo "| $pass | $mode | $cold | $warm | $bytes |"
    done
  done
}

echo "elm-dev: $BIN"
echo "temp: $RUN_ROOT"
echo "passes: $PASSES"

case "$FIXTURE" in
  all)
    run_fixture app
    run_fixture generator
    ;;
  *)
    run_fixture "$FIXTURE"
    ;;
esac
