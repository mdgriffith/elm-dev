#!/bin/bash

# Make this script runnable from anywhere (repo root or this dir)
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
NPM_DIR="$SCRIPT_DIR/.."
cd "$NPM_DIR"

VERSION="0.2.1"
RELEASE_DIR="releases/$VERSION"

# Define URLs for download
URL_LINUX_ARM64="https://static.lamdera.com/bin/elm-dev/elm-dev-next-linux-arm64"
URL_LINUX_X86_64="https://static.lamdera.com/bin/elm-dev/elm-dev-next-linux-x86_64"
URL_MACOS_ARM64="https://static.lamdera.com/bin/elm-dev/elm-dev-next-macos-arm64"
URL_MACOS_X86_64="https://static.lamdera.com/bin/elm-dev/elm-dev-next-macos-x86_64"
URL_WINDOWS_X86_64="https://static.lamdera.com/bin/elm-dev/elm-dev-next-windows-x86_64.zip"


# Create target directories
mkdir -p packages/linux_arm64
mkdir -p packages/linux_x64
mkdir -p packages/darwin_arm64
mkdir -p packages/darwin_x64
mkdir -p packages/win32_x64
mkdir -p $RELEASE_DIR

# Download and save files to the respective directories
curl -L $URL_LINUX_ARM64 -o packages/linux_arm64/elm-dev
curl -L $URL_LINUX_X86_64 -o packages/linux_x64/elm-dev
curl -L $URL_MACOS_ARM64 -o packages/darwin_arm64/elm-dev
curl -L $URL_MACOS_X86_64 -o packages/darwin_x64/elm-dev
curl -L $URL_WINDOWS_X86_64 -o packages/win32_x64/elm-dev.zip


# # Unzip the Windows file and rename to elm-dev.exe
unzip packages/win32_x64/elm-dev.zip -d packages/win32_x64
mv packages/win32_x64/elm-dev/elm-dev.exe packages/win32_x64/elm-dev.exe
rm packages/win32_x64/elm-dev.zip # Remove the zip file after extraction
rm -r packages/win32_x64/elm-dev # Remove the directory after extraction

# Make all files executable
chmod +x packages/linux_arm64/elm-dev
chmod +x packages/linux_x64/elm-dev
chmod +x packages/darwin_arm64/elm-dev
chmod +x packages/darwin_x64/elm-dev
chmod +x packages/win32_x64/elm-dev.exe

# Mark proxy executables
chmod +x packages/linux_arm64/elm-dev-proxy || true
chmod +x packages/linux_x64/elm-dev-proxy || true
chmod +x packages/darwin_arm64/elm-dev-proxy || true
chmod +x packages/darwin_x64/elm-dev-proxy || true

# Update version in package.json for each directory
for dir in packages/linux_arm64 packages/linux_x64 packages/darwin_arm64 packages/darwin_x64 packages/win32_x64; do
    if [ -f "$dir/package.json" ]; then
        # Use jq to update the version. jq must be installed.
        jq --arg v "$VERSION" '.version = $v' "$dir/package.json" > tmp.json && mv tmp.json "$dir/package.json"
    fi
done

# Copy and gzip each file into the releases directory
cp packages/linux_arm64/elm-dev "$RELEASE_DIR/elm-dev-linux-arm64"
gzip --force "$RELEASE_DIR/elm-dev-linux-arm64"

cp packages/linux_x64/elm-dev "$RELEASE_DIR/elm-dev-linux-x64"
gzip --force "$RELEASE_DIR/elm-dev-linux-x64"

cp packages/darwin_arm64/elm-dev "$RELEASE_DIR/elm-dev-darwin-arm64"
gzip --force "$RELEASE_DIR/elm-dev-darwin-arm64"

cp packages/darwin_x64/elm-dev "$RELEASE_DIR/elm-dev-darwin-x64"
gzip --force "$RELEASE_DIR/elm-dev-darwin-x64"

cp packages/win32_x64/elm-dev.exe "$RELEASE_DIR/elm-dev-win32-x64.exe"
gzip --force "$RELEASE_DIR/elm-dev-win32-x64.exe"

# Proxy artifacts (required)
[ -f packages/linux_arm64/elm-dev-proxy ] || { echo "Missing packages/linux_arm64/elm-dev-proxy" >&2; exit 1; }
cp packages/linux_arm64/elm-dev-proxy "$RELEASE_DIR/elm-dev-proxy-linux-arm64"
gzip --force "$RELEASE_DIR/elm-dev-proxy-linux-arm64"

[ -f packages/linux_x64/elm-dev-proxy ] || { echo "Missing packages/linux_x64/elm-dev-proxy" >&2; exit 1; }
cp packages/linux_x64/elm-dev-proxy "$RELEASE_DIR/elm-dev-proxy-linux-x64"
gzip --force "$RELEASE_DIR/elm-dev-proxy-linux-x64"

[ -f packages/darwin_arm64/elm-dev-proxy ] || { echo "Missing packages/darwin_arm64/elm-dev-proxy" >&2; exit 1; }
cp packages/darwin_arm64/elm-dev-proxy "$RELEASE_DIR/elm-dev-proxy-darwin-arm64"
gzip --force "$RELEASE_DIR/elm-dev-proxy-darwin-arm64"

[ -f packages/darwin_x64/elm-dev-proxy ] || { echo "Missing packages/darwin_x64/elm-dev-proxy" >&2; exit 1; }
cp packages/darwin_x64/elm-dev-proxy "$RELEASE_DIR/elm-dev-proxy-darwin-x64"
gzip --force "$RELEASE_DIR/elm-dev-proxy-darwin-x64"

[ -f packages/win32_x64/elm-dev-proxy.exe ] || { echo "Missing packages/win32_x64/elm-dev-proxy.exe" >&2; exit 1; }
cp packages/win32_x64/elm-dev-proxy.exe "$RELEASE_DIR/elm-dev-proxy-win32-x64.exe"
gzip --force "$RELEASE_DIR/elm-dev-proxy-win32-x64.exe"