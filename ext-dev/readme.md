# elm-dev

This is both an elm compiler extension + server, as well as an app that talks to that server.

## Gettings Started

1. Put the following into `~/.ghci`

```
:set -fbyte-code
:set -fobject-code
:set prompt "\ESC[34mλ: \ESC[m"
:def rr const $ return $ unlines ["Ext.Common.killTrackedThreads",":r","Test.target"]
```

2. Run `stack ghci`

3. Run `:set -XOverloadedStrings`

4. Run `Test.target` - Seems weird, but this starts the elm-dev server!

5. Then the dev feedback loop goes as follows:

- Make changes to Haskell code
- Run `:rr` to kill all threads, typecheck + recompile, and re-run `Test.target`
- Fix any issues, then `:rr` again
- If you want to just type-check _without_ running, use `:r`

Easier to change the target definition than constantly adjust the `:def` in `~/.ghci`!

Check out `WatchTower.Test` to see the cli args provided for development.

## What the server does

When the server starts, then `Ext.Dev.Project.discover` will run in the current directory.

This recursively searches for `elm.json` files and notes them.

At the moment, it also looks for `src/Main.elm` as the entrypoint, though there are still questions about what to do about files that aren't imported by the entrypoint.

## Install Watchtower locally

Run `stack install` and it will compile and copy the `elm-dev` binaries to your PATH.
