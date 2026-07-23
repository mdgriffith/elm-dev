#!/usr/bin/env bash
set -ex                                                   # Be verbose and exit immediately on error instead of trying to continue

source "common.sh"
os="linux"
arch="arm64"

buildTag="$project-$version-$os-$arch"
dist=distribution/dist
bin=$dist/$buildTag

scriptDir=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
compilerRoot="$scriptDir/.."

if [ "$GITHUB_ACTIONS" == "true" ]; then
    # We're running in the Github Actions environment, scope folders accordingly
    mountRoot="$compilerRoot"
    cacheRoot="/home/github/user-build-cache/linux-arm64"
    rsyncCompilerPath=""
    dockerHost=""
elif [ "$LOCAL_DOCKER" == "true" ]; then
    mountRoot="$compilerRoot"
    cacheRoot="$HOME/.docker/user-build-cache/linux-arm64"
    rsyncCompilerPath=""
    dockerHost=""
else
    # We're executing the build manually against the lamdera-community-build-arm64 server
    mountRoot="/root/compiler"
    cacheRoot="/home/github/user-build-cache/linux-arm64"
    rsyncCompilerPath="$project@lamdera-community-build-arm64:$mountRoot"
    dockerHost="-H ssh://$project@lamdera-community-build-arm64"
fi

# Determine whether we need to force Docker to use ARM64 platform (for x64 CI with QEMU)
hostArch="$(uname -m)"
if [ "$hostArch" != "aarch64" ] && [ "$hostArch" != "arm64" ]; then
    dockerPlatformArgs="--platform linux/arm64"
else
    dockerPlatformArgs=""
fi

cd "$compilerRoot"                                                   # Move into the project root
git submodule init && git submodule update

if [ -z "$rsyncCompilerPath" ]; then
    echo "rsyncCompilerPath is empty, skipping"
else
    # GOAL: syncronise all relevant files to the remote host
    rsync -avz \
        --exclude=".stack-work" \
        --exclude="ext-package-cache" \
        --exclude="distribution/dist" \
        --exclude="dist-newstyle" \
        --exclude="extra/npm" \
        --exclude="node_modules" \
        --exclude="elm-stuff" \
        ./ $rsyncCompilerPath

    ssh $project@lamdera-community-build-arm64 "mkdir -p $cacheRoot || true"
fi


build_binary_docker() {
    set -ex
    local bin="$1"
    local actions="$2"
    local userId="$3"
    local groupId="$4"
    local compilerRoot="/root/compiler"
    cd $compilerRoot

    cleanup() {
        # Work around ownership issues that prevent GH actions from managing the files later
        [ "$actions" == "true" ] && chown -R "$userId:$groupId" ./* || true
    }
    trap cleanup EXIT

    git config --global --add safe.directory /root/compiler

    # Keep Stack's package and build caches outside the container.
    export STACK_ROOT=/root/cache/stack

    mkdir -p "$STACK_ROOT"

    # Our options required for static linking
    STACKOPTS="--system-ghc --no-install-ghc --allow-different-user"
    GHCOPTS="-j4 +RTS -A256m -RTS -static -split-sections -optc-Os -optl=-pthread"

    stack $STACKOPTS build --only-dependencies --ghc-options="$GHCOPTS"

    # GOAL: build the binary statically
    # Inexplicably the first build fails, but the second succeeds
    local outputDir
    outputDir="$(dirname "$bin")"
    (stack $STACKOPTS install --local-bin-path "$outputDir" --ghc-options="$GHCOPTS" || true) && \
        stack $STACKOPTS install --local-bin-path "$outputDir" --ghc-options="$GHCOPTS"

    mv "$outputDir/elm-dev" "$bin"
    strip "$bin"
    file "$bin" | grep -q "statically linked"

    # Work around ownership issues that prevent GH actions from managing the files later
    [ "$actions" == "true" ] && chown -R "$userId:$groupId" ./* || true
}
declare -f build_binary_docker

# GOAL: get a suitable build environment with GHC & Cabal build for arm64 in an Alpine container using MUSL instead of GLIBC, so we can build portable static binaries

# Here's how to use it without the bash injection for manual by-hand testing:
# docker -H ssh://$project@lamdera-community-build-arm64 run \
#     -v /root/compiler:/root/compiler \
#     -it glcr.b-data.ch/ghc/ghc-musl:9.2.8 \
#     /bin/bash

mkdir -p $dist

[ "$GITHUB_ACTIONS" == "true" ] && runMode="--rm -i" || runMode="-it"
docker $dockerHost run $dockerPlatformArgs \
    -v "$mountRoot:/root/compiler" \
    -v "$cacheRoot:/root/cache" \
    $runMode glcr.b-data.ch/ghc/ghc-musl:9.2.8 \
    bash -c "$(declare -f build_binary_docker); build_binary_docker '$bin' '$GITHUB_ACTIONS' '$(id -u)' '$(id -g)'"
# From https://github.com/benz0li/ghc-musl

if [ -z "$rsyncCompilerPath" ]; then
    echo "rsyncCompilerPath is empty, skipping"
else
    # GOAL: get the remote binary locally
    scp $rsyncCompilerPath/"$bin" "$bin"
fi

ls -alh "$bin"
chmod a+x "$bin"
file "$bin"
ls -alh "$bin"
echo "put $bin $project/$project-next-$os-$arch" | sftp -i ~/.ssh/id_ed25519 -P 22 github@apps.lamdera.com
