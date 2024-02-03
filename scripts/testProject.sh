#!/usr/bin/env bash
set -euo pipefail



# Clear the output directory and elm stuff
rm -rf apps/vscode/testProject/output
mkdir -p apps/vscode/testProject/output

# Docs
rm -rf apps/vscode/testProject/elm-stuff
stack run --cwd apps/vscode/testProject -- docs src/Main.elm src/Imported.elm --output=output/docs-files.json

# Docs, Modules
rm -rf apps/vscode/testProject/elm-stuff
stack run --cwd apps/vscode/testProject -- docs Main Imported Alias --output=output/docs-modules.json

# Docs, Package
rm -rf apps/vscode/testProject/elm-stuff
stack run --cwd apps/vscode/testProject -- docs mdgriffith/elm-ui --output=output/docs-package.json


# Warnings
rm -rf apps/vscode/testProject/elm-stuff
stack run --cwd apps/vscode/testProject -- warnings Main --output=output/warnings.json

# Entrypoints
rm -rf apps/vscode/testProject/elm-stuff
stack run --cwd apps/vscode/testProject -- entrypoints --output=output/entrypoints.json


# Imports
rm -rf apps/vscode/testProject/elm-stuff
stack run --cwd apps/vscode/testProject -- imports Main, AlternativeMain --output=output/imports.json


# Usage
rm -rf apps/vscode/testProject/elm-stuff
stack run --cwd apps/vscode/testProject -- usage Imported --output=output/usage.json

# Usage, type
rm -rf apps/vscode/testProject/elm-stuff
stack run --cwd apps/vscode/testProject -- usage type Imported.MyType --output=output/usage-type.json


# Explain
rm -rf apps/vscode/testProject/elm-stuff
stack run --cwd apps/vscode/testProject -- explain Main.Regions --output=output/explain.json

# Callgraph
rm -rf apps/vscode/testProject/elm-stuff
stack run --cwd apps/vscode/testProject -- callgraph Main --output=output/callgraph.json