#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "$0")/.." && pwd)"
PROXY_DIR="$ROOT_DIR/apps/proxy"
PKGS_DIR="$ROOT_DIR/installers-elm-dev/npm/packages"

if ! command -v cargo >/dev/null 2>&1; then
  echo "cargo is required. Install Rust via https://rustup.rs/" >&2
  exit 1
fi

os="$(uname -s)"
arch="$(uname -m)"

target=""
pkg_dir=""
bin_name="elm-dev-proxy"

case "$os:$arch" in
  Darwin:arm64)
    target="aarch64-apple-darwin"
    pkg_dir="darwin_arm64"
    ;;
  Darwin:x86_64)
    target="x86_64-apple-darwin"
    pkg_dir="darwin_x64"
    ;;
  Linux:x86_64)
    target="x86_64-unknown-linux-musl"
    pkg_dir="linux_x64"
    ;;
  Linux:aarch64)
    target="aarch64-unknown-linux-musl"
    pkg_dir="linux_arm64"
    ;;
  *)
    echo "Unsupported platform: $os/$arch" >&2
    exit 1
    ;;
esac

if [[ "$os" == "Linux" ]] && ! cargo zigbuild -V >/dev/null 2>&1; then
  echo "cargo-zigbuild is required for Linux MUSL targets." >&2
  echo "Install with: cargo install cargo-zigbuild" >&2
  exit 1
fi

echo "Building elm-dev-proxy for $target..."
if [[ "$os" == "Linux" ]]; then
  cargo zigbuild --release --target "$target" --manifest-path "$PROXY_DIR/Cargo.toml"
else
  cargo build --release --target "$target" --manifest-path "$PROXY_DIR/Cargo.toml"
fi

src="$PROXY_DIR/target/$target/release/$bin_name"
dst="$PKGS_DIR/$pkg_dir/$bin_name"

echo "Installing $src -> $dst"
install -m 0755 "$src" "$dst"

echo "Done."
