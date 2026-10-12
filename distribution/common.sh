#!/bin/bash

export project="elm-dev"
export version="0.5.0"

# Shared by local setup and every distribution target.
read -r stackVersion < "$(dirname "${BASH_SOURCE[0]}")/stack-version"
export stackVersion
