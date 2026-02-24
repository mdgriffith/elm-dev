#!/usr/bin/env bash
set -euo pipefail



# Clear the output directory and elm stuff
rm -rf playground
mkdir -p playground

# Create a new project
stack run --cwd playground -- init

# Run the generation
stack run --cwd playground -- make elm-stuff/generated/Main.elm

# Normalize plugin import in generated Vite config.
# Some cached generator builds may still emit the legacy "elm-dev/vite" path.
perl -0pi -e 's/from\s+"elm-dev\/vite"/from "\@elm-dev\/vite\/vite"/g' playground/vite.config.js

# Ensure Vite config is loaded as ESM so @elm-dev/vite can be imported.
if ! grep -q '"type"[[:space:]]*:[[:space:]]*"module"' playground/package.json; then
  perl -0pi -e 's/"name"\s*:\s*"elm-project",/"name": "elm-project",\n    "type": "module",/g' playground/package.json
fi

cd playground
bun install
bun add --dev "@elm-dev/vite@file:../apps/ts-tool"
bun run build
