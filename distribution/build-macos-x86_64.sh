#!/usr/bin/env bash
set -ex                                                   # Be verbose and exit immediately on error instead of trying to continue

source "common.sh"
os="macos"
arch="x86_64"

buildTag="$project-$version-$os-$arch"
dist=distribution/dist
mkdir -p $dist
bin=$dist/$buildTag
scriptDir=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )






isolate=~/.ghcup/$os-$arch/$stackVersion
mkdir -p $isolate
stack="$isolate/stack"

                                                          # Ensure correct arch toolchain is installed, or install it
                                                          # Hopefully in future ghcup has better multi-arch support
if [ ! -x "$stack" ] || [ "$("$stack" --numeric-version)" != "$stackVersion" ]; then
  ghcup install stack "$stackVersion" --isolate "$isolate" --force -p x86_64-apple-darwin
fi

test "$("$stack" --numeric-version)" = "$stackVersion"

cd "$scriptDir/.."                                        # Move into the project root
git submodule init && git submodule update

install_stack_with_retry "$stack" "$dist"

cp $dist/$project $bin                                    # Copy built binary to dist
strip $bin                                                # Strip symbols to reduce binary size (90M -> 56M)
"$bin" --version-full
file $bin
ls -alh $bin
echo "put $bin $project/$project-next-$os-$arch" | sftp -i ~/.ssh/id_ed25519 -P 22 github@apps.lamdera.com
