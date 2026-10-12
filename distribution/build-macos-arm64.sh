#!/usr/bin/env bash
set -ex                                                   # Be verbose and exit immediately on error instead of trying to continue

source "common.sh"
os="macos"
arch="arm64"

buildTag="$project-$version-$os-$arch"
dist=distribution/dist
mkdir -p $dist
bin=$dist/$buildTag
scriptDir=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

if ! uname -a | grep "Darwin" && uname -a | grep "arm64"; then
  echo "This build can only be run on an Apple M-series chipset build host."
  exit 1;
fi

isolate=~/.ghcup/$os-$arch/$stackVersion
mkdir -p $isolate
stack="$isolate/stack"

                                                          # Ensure correct arch toolchain is installed, or install it
                                                          # Hopefully in future ghcup has better multi-arch support
if [ ! -x "$stack" ] || [ "$("$stack" --numeric-version)" != "$stackVersion" ]; then
  ghcup install stack "$stackVersion" --isolate "$isolate" --force
fi

test "$("$stack" --numeric-version)" = "$stackVersion"

cd "$scriptDir/.."                                        # Move into the project root
git submodule init && git submodule update

$stack install --local-bin-path $dist

cp $dist/$project $bin                                    # Copy built binary to dist
strip $bin                                                # Strip symbols to reduce binary size (90M -> 56M)
file $bin
ls -alh $bin
echo "put $bin $project/$project-next-$os-$arch" | sftp -i ~/.ssh/id_ed25519 -P 22 github@apps.lamdera.com
