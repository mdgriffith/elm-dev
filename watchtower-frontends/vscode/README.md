# Elm Watchtower

## Development Installation

First, run `yarn install` and `yarn build`.

Then, you might be able to use VSCode's fancy Development Extension Host by pressing `f5`.

If that doesn't work (like it didn't for me), you can [install the extension locally by making a symlink to the proper folder](https://vscode-docs.readthedocs.io/en/stable/extensions/install-extension/). For me that meant running:

`ln -s $PWD ~/.vscode/extensions/mgriff.elm-watchtower-0.0.1`

Then reload VSCode in the window you want to develop in via `Cmd + R`

## Developing

- run `yarn watch`.
- When you change the code, you can reload the extension in VSCode by running `Cmd + R`

## Overview of code

- `extension.ts` - Code for registering the VSCode plugin.
- `watchtower.ts` - The main content of the extension.
- `watchtower/question.ts` - Helpers for constructing messages for the watchtower server.

## Refrences

**VSCode**

- [Memento for persisting data for a workspace](https://code.visualstudio.com/api/references/vscode-api#Memento)

* [Node Elm Compiler Runner](https://github.com/rtfeldman/node-elm-compiler)
* [Elmi to JSON](https://github.com/stoeffel/elmi-to-json) - convert `.elmi` files to JSON. `.elmi` files capture the interface for every module including function names and type signatures, type aliases and unions.
* [elm-xref](https://github.com/zwilias/elm-xref) - Find unused function declarations. Looks to be a fairly simple implementation, which could be modified to extract other information.
* [elmjutsu](https://atom.io/packages/elmjutsu) -
