#!/usr/bin/env bash

set -euo pipefail

# Build elm-dev-proxy for all target platforms and copy into npm per-platform packages.
# Run from repo root or any directory; this script manages its own working dirs.

ROOT_DIR="$(cd "$(dirname "$0")/.." && pwd)"
PROXY_DIR="$ROOT_DIR/apps/proxy"
PKGS_DIR="$ROOT_DIR/installers-elm-dev/npm/packages"

if [[ ! -f "$PROXY_DIR/Cargo.toml" ]] || ! grep -q 'name = "elm-dev-proxy"' "$PROXY_DIR/Cargo.toml"; then
  echo "Could not find apps/proxy/Cargo.toml for elm-dev-proxy." >&2
  exit 1
fi

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
pushd "$PROXY_DIR" >/dev/null
  # macOS native builds
  cargo build    --release --target aarch64-apple-darwin
  cargo build    --release --target x86_64-apple-darwin

  # Linux via MUSL (with zig toolchain)
  cargo zigbuild --release --target x86_64-unknown-linux-musl
  cargo zigbuild --release --target aarch64-unknown-linux-musl

  # Windows (gnu)
  cargo zigbuild --release --target x86_64-pc-windows-gnu
popd >/dev/null

echo "Copying binaries into npm per-platform packages ..."

install -m 0755 "$PROXY_DIR/target/aarch64-apple-darwin/release/elm-dev-proxy"       "$PKGS_DIR/darwin_arm64/elm-dev-proxy"
install -m 0755 "$PROXY_DIR/target/x86_64-apple-darwin/release/elm-dev-proxy"        "$PKGS_DIR/darwin_x64/elm-dev-proxy"
install -m 0755 "$PROXY_DIR/target/x86_64-unknown-linux-musl/release/elm-dev-proxy"  "$PKGS_DIR/linux_x64/elm-dev-proxy"
install -m 0755 "$PROXY_DIR/target/aarch64-unknown-linux-musl/release/elm-dev-proxy" "$PKGS_DIR/linux_arm64/elm-dev-proxy"
install -m 0755 "$PROXY_DIR/target/x86_64-pc-windows-gnu/release/elm-dev-proxy.exe"  "$PKGS_DIR/win32_x64/elm-dev-proxy.exe"

echo "Done. Proxies placed in: $PKGS_DIR/{darwin_*,linux_*,win32_x64}"


