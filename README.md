# Elm Dev

Elm Dev is a version of the Elm compiler that is made to support editing tools.

Install via `npm install -g elm-dev` if you want to play with it.

It's currently experimental, but will likely be stable soon.

It is currently a command line tool with the following commands that print or output JSON.

- `warnings` - List missing type signatures and unused values.
- `entrypoints` - Detect what `.elm` files are the potential roots of a project. This will also report any ports relevant to a specific entrypoint as well as the type signatures of those ports.
- `docs` - Generate `docs.json` for any package, or any local `.elm` file.
- `imports` - Given a list of modules, report all files and packages that they collectively depends on. This is useful for
- `usage` - Given a module, return all usages of that module in a given project.
- `explain` - Given a fully qualified type name, provide it's full definition.

Each command may instead report compilation errors if the file or project fails to compile.

## Roadmap

The above functionality is a first pass on what would be useful for `elm-dev` to report and has been published to allow downstream projects to try stuff out.

In the medium term, the intention is to support the language-server protocol and to adjust functionaltiy based on downstream projects.
