#!/usr/bin/env bash


mkdir ./out/interactive
mkdir ./out/interactive/src
cp ./src/interactive/elm.json ./out/interactive/elm.json
cp -r ./codegen/helpers/* ./out/interactive/src
cp ./node_modules/elm-format/unpacked_bin/elm-format ./out/elm-format
cp ./node_modules/elm/bin/elm ./out/elm