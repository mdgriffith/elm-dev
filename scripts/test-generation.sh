#!/usr/bin/env bash
set -euo pipefail



# Clear the output directory and elm stuff
rm -rf playground
mkdir -p playground

# Create a new project
stack run --cwd playground -- init

# Run the generation
stack run --cwd playground -- make elm-stuff/generated/Main.elm

# Ensure the generated app uses the published elm-dev package shape.
grep -q 'from "elm-dev/vite"' playground/vite.config.js
grep -q '"elm-dev"' playground/package.json

# Ensure Vite config is loaded as ESM so elm-dev/vite can be imported.
if ! grep -q '"type"[[:space:]]*:[[:space:]]*"module"' playground/package.json; then
  perl -0pi -e 's/"name"\s*:\s*"elm-project",/"name": "elm-project",\n    "type": "module",/g' playground/package.json
fi

# Use the local package for this repo test while preserving the published import path.
perl -0pi -e 's/"elm-dev"\s*:\s*"[^"]+"/"elm-dev": "file:..\/installers-elm-dev\/npm"/g' playground/package.json

cd playground
bun install
bun run build
