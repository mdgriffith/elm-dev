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
    cacheRoot="/home/github/cabal-caches/linux-arm64"
    rsyncCompilerPath=""
    dockerHost=""
else
    # We're executing the build manually against the lamdera-community-build-arm64 server
    mountRoot="/root/compiler"
    cacheRoot="/home/github/cabal-caches/linux-arm64"
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

    # GOAL: get the cabal caches into the mounted folder so they persist outside the Docker run lifetime and we don't needlessly rebuild hundreds of super expensive deps repeatedly forever
    # This is documented but doesn't seem to work https://cabal.readthedocs.io/en/3.6/installing-packages.html#environment-variables
    export CABAL_DIR=/root/cache
    mkdir -p /root/cache || true
    ln -sf /root/cache ~/.cabal

    # GOAL: pin our dependencies so we can build them one by one
    cabal update
    # We have to freeze the deps to get a cohesive deps set, otherwise `cabal build <dep>` will install the latest version instead of the one we need
    rm cabal.project.freeze || true # Remove the freeze file so we can update the deps
    cabal freeze

    # Our options required for static linking
    CABALOPTS="--allow-newer -f-export-dynamic -fembed_data_files --enable-executable-static -j4"
    GHCOPTS="-j4 +RTS -A256m -RTS -split-sections -optc-Os -optl=-pthread"

    # GOAL: build non-elm/elm-format deps first to save time and cut out baseline issues
    # cabal build --dry-run | grep ' - ' | grep -v 'elm-' | grep -v 'avh4' | cut -d' ' -f3 | sed 's/-[^-]*$//' | xargs -I{} cabal build {} --only-dependencies $CABALOPTS --ghc-options="$GHCOPTS"
    cabal build --only-dependencies $CABALOPTS --ghc-options="$GHCOPTS"

    # GOAL: build the binary statically
    # Inexplicably the first build fails, but the second succeeds
    (cabal build $CABALOPTS --ghc-options="$GHCOPTS" || true) && cabal build $CABALOPTS --ghc-options="$GHCOPTS"

    cp "$(cabal list-bin .)" "$bin"
    strip "$bin"

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
