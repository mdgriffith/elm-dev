#!/usr/bin/env bash

set -euo pipefail

# NOTE: This script is intended to be run from the apps/proxy directory.
# It cross-compiles elm-dev-proxy for common targets and copies the
# resulting binaries into the per-platform npm package folders under:
#   ../../installers-elm-dev/npm/packages

# Validate working directory
if [[ ! -f "Cargo.toml" ]] || ! grep -q "name = \"elm-dev-proxy\"" Cargo.toml; then
  echo "Please run this script from apps/proxy (where Cargo.toml lives)." >&2
  exit 1
fi

# Compute important paths
SCRIPT_DIR="$(pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"
PKGS_DIR="${REPO_ROOT}/installers-elm-dev/npm/packages"

# Ensure required tools / targets
if ! command -v rustup >/dev/null 2>&1; then
  echo "rustup is required. Install from https://rustup.rs/" >&2
  exit 1
fi

if ! command -v cargo >/dev/null 2>&1; then
  echo "cargo is required (installed by rustup)." >&2
  exit 1
fi

if ! command -v zig >/dev/null 2>&1; then
  echo "zig is recommended for cross-compiling to Linux/Windows. Install via brew (macOS): brew install zig" >&2
fi

if ! cargo zigbuild -V >/dev/null 2>&1; then
  echo "Installing cargo-zigbuild ..." >&2
  cargo install cargo-zigbuild >/dev/null 2>&1 || true
fi

# Add targets (idempotent)
rustup target add \
  aarch64-apple-darwin \
  x86_64-apple-darwin \
  x86_64-unknown-linux-musl \
  aarch64-unknown-linux-musl \
  x86_64-pc-windows-gnu >/dev/null 2>&1 || true

# macOS SDK setup for x86_64 on Apple Silicon
if command -v xcrun >/dev/null 2>&1; then
  export SDKROOT="$(xcrun --sdk macosx --show-sdk-path)"
fi
export CARGO_TARGET_X86_64_APPLE_DARWIN_LINKER=clang

echo "Building elm-dev-proxy for targets ..."

# macOS native builds
cargo build    --release --target aarch64-apple-darwin
cargo build    --release --target x86_64-apple-darwin

# Linux via MUSL (with zig toolchain)
cargo zigbuild --release --target x86_64-unknown-linux-musl
cargo zigbuild --release --target aarch64-unknown-linux-musl

# Windows (gnu)
cargo zigbuild --release --target x86_64-pc-windows-gnu

echo "Copying binaries into npm per-platform packages ..."

install -m 0755 "target/aarch64-apple-darwin/release/elm-dev-proxy"       "${PKGS_DIR}/darwin_arm64/elm-dev-proxy"
install -m 0755 "target/x86_64-apple-darwin/release/elm-dev-proxy"        "${PKGS_DIR}/darwin_x64/elm-dev-proxy"
install -m 0755 "target/x86_64-unknown-linux-musl/release/elm-dev-proxy"  "${PKGS_DIR}/linux_x64/elm-dev-proxy"
install -m 0755 "target/aarch64-unknown-linux-musl/release/elm-dev-proxy" "${PKGS_DIR}/linux_arm64/elm-dev-proxy"
install -m 0755 "target/x86_64-pc-windows-gnu/release/elm-dev-proxy.exe"  "${PKGS_DIR}/win32_x64/elm-dev-proxy.exe"

echo "Done. Proxies placed in: ${PKGS_DIR}/{darwin_*,linux_*,win32_x64}"


