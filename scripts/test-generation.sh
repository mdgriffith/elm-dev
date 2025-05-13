#!/usr/bin/env bash
set -euo pipefail



# Clear the output directory and elm stuff
rm -rf playground
mkdir -p playground

# Create a new project
stack run --cwd playground -- init

# Run the generation
stack run --cwd playground -- make elm-stuff/generated/Main.elm

cd playground
bun install
bun run dev