#!/usr/bin/env bash
set -euo pipefail

# This script links development executables into /usr/local/bin:
#   - elm-dev: runs Stack in the project root captured at link time
#   - elm-dev-proxy: builds and runs the Rust stdio<->TCP proxy
#
# It only creates these files if they do not already exist.

PROJECT_ROOT_DIR="$(pwd)"
ELM_DEV_BIN_PATH="/usr/local/bin/elm-dev"
ELM_DEV_PROXY_BIN_PATH="/usr/local/bin/elm-dev-proxy"

require_cmd() {
  if ! command -v "$1" >/dev/null 2>&1; then
    echo "Error: required command '$1' not found in PATH" >&2
    exit 1
  fi
}

# Ensure we can write to /usr/local/bin (may require sudo)
ensure_writable() {
  local target_dir
  # Avoid external 'dirname' to work in restricted PATHs
  target_dir="${1%/*}"
  if [ -z "$target_dir" ] || [ "$target_dir" = "$1" ]; then
    target_dir="."
  fi
  if [ ! -w "$target_dir" ]; then
    if command -v sudo >/dev/null 2>&1; then
      SUDO="sudo"
    else
      echo "Error: '$target_dir' is not writable and 'sudo' is not available." >&2
      echo "       Re-run this script with sufficient permissions." >&2
      exit 1
    fi
  else
    SUDO=""
  fi
}

write_file() {
  local path
  path="$1"
  shift
  ensure_writable "$path"
  # Write file content only; do not change permissions
  printf "%s" "$*" > "$path"
}

create_elm_dev_bin() {
  if [ -e "$ELM_DEV_BIN_PATH" ]; then
    echo "Skipping: $ELM_DEV_BIN_PATH already exists"
    return 0
  fi

  require_cmd stack

  local script_content
  script_content="#!/bin/bash
set -euo pipefail
original_dir=\"\$PWD\"
cd \"$PROJECT_ROOT_DIR\" && stack run --cwd \"\$original_dir\" -- \"\$@\""

  write_file "$ELM_DEV_BIN_PATH" "$script_content"
  echo "Created: $ELM_DEV_BIN_PATH"
}

create_elm_dev_proxy_bin() {
  if [ -e "$ELM_DEV_PROXY_BIN_PATH" ]; then
    echo "Skipping: $ELM_DEV_PROXY_BIN_PATH already exists"
    return 0
  fi

  require_cmd cargo

  local proxy_dir
  proxy_dir="${PROJECT_ROOT_DIR}/apps/proxy"

  local script_content
  script_content="#!/bin/bash
set -euo pipefail
cd \"$proxy_dir\"
if ! command -v cargo >/dev/null 2>&1; then
  echo \"Error: cargo not found. Install Rust toolchain (https://rustup.rs)\" >&2
  exit 1
fi
cargo build --release --quiet
exec \"$proxy_dir/target/release/elm-dev-proxy\" \"\$@\""

  write_file "$ELM_DEV_PROXY_BIN_PATH" "$script_content"
  echo "Created: $ELM_DEV_PROXY_BIN_PATH"
}

create_elm_dev_bin
create_elm_dev_proxy_bin

echo "Done. You can now run 'elm-dev' and 'elm-dev-proxy'."
