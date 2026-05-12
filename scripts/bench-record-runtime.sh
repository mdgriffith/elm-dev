#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "$0")/.." && pwd)"
RUNS=5
WARMUPS=1
ITERATIONS=2000000
BUILD=1

usage() {
  cat <<'EOF'
Usage: scripts/bench-record-runtime.sh [options]

Compile focused Elm record benchmarks and measure generated JS runtime in Node.

Options:
  --runs N            Timed runs per mode/case (default: 5)
  --warmups N         Untimed warmup runs per mode/case (default: 1)
  --iterations N      Elm loop iterations per run (default: 2000000)
  --no-build          Reuse an already-built elm-dev executable
  -h, --help          Show this help
EOF
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --runs)
      RUNS="${2:-}"
      shift 2
      ;;
    --warmups)
      WARMUPS="${2:-}"
      shift 2
      ;;
    --iterations)
      ITERATIONS="${2:-}"
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

require_positive_int() {
  local name="$1"
  local value="$2"

  case "$value" in
    ''|*[!0-9]*)
      echo "$name must be a positive integer" >&2
      exit 1
      ;;
  esac

  if [[ "$value" -lt 1 ]]; then
    echo "$name must be at least 1" >&2
    exit 1
  fi
}

require_non_negative_int() {
  local name="$1"
  local value="$2"

  case "$value" in
    ''|*[!0-9]*)
      echo "$name must be a non-negative integer" >&2
      exit 1
      ;;
  esac
}

require_positive_int "--runs" "$RUNS"
require_positive_int "--iterations" "$ITERATIONS"
require_non_negative_int "--warmups" "$WARMUPS"

if [[ "$BUILD" -eq 1 ]]; then
  (cd "$ROOT_DIR" && cabal build exe:elm-dev >/dev/null)
fi

BIN="$(cd "$ROOT_DIR" && cabal list-bin exe:elm-dev)"
RUN_ROOT="${TMPDIR:-/tmp}/elm-dev-record-runtime-$(date +%s)"
mkdir -p "$RUN_ROOT"

write_elm_json() {
  local project_dir="$1"

  cat > "$project_dir/elm.json" <<'EOF'
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
EOF
}

write_literal_source() {
  local project_dir="$1"

  cat > "$project_dir/src/Main.elm" <<EOF
port module Main exposing (main)

import Platform
import Platform.Cmd as Cmd
import Platform.Sub as Sub


port done : String -> Cmd msg


port start : (() -> msg) -> Sub msg


iterations : Int
iterations =
    $ITERATIONS


type alias Rec =
    { a : Int, b : Int, c : Int, d : Int, e : Int, f : Int, g : Int, h : Int }


make : Int -> Rec
make i =
    { a = i, b = i + 1, c = i + 2, d = i + 3, e = i + 4, f = i + 5, g = i + 6, h = i + 7 }


loop : Int -> Int -> Int
loop i acc =
    if i <= 0 then
        acc

    else
        let
            r =
                make i
        in
        loop (i - 1) (acc + r.a + r.h)


type Msg
    = Start


main : Program () () Msg
main =
    Platform.worker
        { init = \_ -> ( (), Cmd.none )
        , update = \msg model ->
            case msg of
                Start ->
                    ( model, done (String.fromInt (loop iterations 0)) )
        , subscriptions = \_ -> start (\_ -> Start)
        }
EOF
}

write_update_source() {
  local project_dir="$1"

  cat > "$project_dir/src/Main.elm" <<EOF
port module Main exposing (main)

import Platform
import Platform.Cmd as Cmd
import Platform.Sub as Sub


port done : String -> Cmd msg


port start : (() -> msg) -> Sub msg


iterations : Int
iterations =
    $ITERATIONS


type alias Rec =
    { a : Int, b : Int, c : Int, d : Int, e : Int, f : Int, g : Int, h : Int }


initial : Rec
initial =
    { a = 1, b = 2, c = 3, d = 4, e = 5, f = 6, g = 7, h = 8 }


loop : Int -> Rec -> Int -> Int
loop i rec acc =
    if i <= 0 then
        acc + rec.d + rec.h

    else
        let
            next =
                { rec | d = rec.d + 1, h = rec.h + 2 }
        in
        loop (i - 1) next (acc + next.d + next.h)


type Msg
    = Start


main : Program () () Msg
main =
    Platform.worker
        { init = \_ -> ( (), Cmd.none )
        , update = \msg model ->
            case msg of
                Start ->
                    ( model, done (String.fromInt (loop iterations initial 0)) )
        , subscriptions = \_ -> start (\_ -> Start)
        }
EOF
}

mode_flag_for() {
  case "$1" in
    optimize) echo "--optimize" ;;
    O2) echo "-O2" ;;
    O3) echo "-O3" ;;
  esac
}

write_runner() {
  local project_dir="$1"

  cat > "$project_dir/runner.js" <<'EOF'
const { performance } = require('perf_hooks');
const elm = require('./out.js');

const app = elm.Elm.Main.init({ flags: null });
let start = 0;

app.ports.done.subscribe((value) => {
  const end = performance.now();
  console.log(JSON.stringify({ ms: end - start, value }));
  process.exit(0);
});

setTimeout(() => {
  start = performance.now();
  app.ports.start.send(null);
}, 100);

setTimeout(() => {
  console.error('benchmark timed out');
  process.exit(2);
}, 30000);
EOF
}

prepare_project() {
  local case_name="$1"
  local project_dir="$2"

  mkdir -p "$project_dir/src"
  write_elm_json "$project_dir"

  case "$case_name" in
    literals) write_literal_source "$project_dir" ;;
    updates) write_update_source "$project_dir" ;;
  esac
}

compile_case() {
  local project_dir="$1"
  local mode="$2"
  local flag
  flag="$(mode_flag_for "$mode")"

  (cd "$project_dir" && "$BIN" make src/Main.elm "$flag" --output=out.js >/dev/null)
}

summarize_samples() {
  node -e '
const samples = process.argv.slice(1).map(Number).sort((a, b) => a - b);
const median = samples[Math.floor(samples.length / 2)];
const min = samples[0];
const max = samples[samples.length - 1];
const avg = samples.reduce((a, b) => a + b, 0) / samples.length;
console.log([min, median, avg, max].map(n => n.toFixed(2)).join(" "));
' "$@"
}

run_case_mode() {
  local case_name="$1"
  local mode="$2"
  local project_dir="$RUN_ROOT/$case_name-$mode"
  local bytes
  local samples=()

  prepare_project "$case_name" "$project_dir"
  compile_case "$project_dir" "$mode"
  write_runner "$project_dir"
  bytes="$(wc -c < "$project_dir/out.js" | tr -d ' ')"

  for _ in $(seq 1 "$WARMUPS"); do
    (cd "$project_dir" && node runner.js >/dev/null)
  done

  for _ in $(seq 1 "$RUNS"); do
    local json ms
    json="$(cd "$project_dir" && node runner.js)"
    ms="$(node -e 'console.log(JSON.parse(process.argv[1]).ms)' "$json")"
    samples+=("$ms")
  done

  read -r min median avg max < <(summarize_samples "${samples[@]}")
  echo "| $case_name | $mode | $min | $median | $avg | $max | $bytes |"
}

echo "elm-dev: $BIN"
echo "temp: $RUN_ROOT"
echo "iterations: $ITERATIONS"
echo "warmups: $WARMUPS"
echo "runs: $RUNS"
echo
echo "| case | mode | min ms | median ms | avg ms | max ms | bytes |"
echo "|---|---|---:|---:|---:|---:|---:|"

for case_name in literals updates; do
  for mode in optimize O2 O3; do
    run_case_mode "$case_name" "$mode"
  done
done
