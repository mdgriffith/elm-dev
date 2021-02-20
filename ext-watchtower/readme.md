
# Watchtower

This is both an elm compiler extension + server, as well as an app that talks to that server.



## Gettings Started

1. Put the following into `~/.ghci`

```
:set -fbyte-code
:set -fobject-code
:def rr const $ return $ unlines [":r","Test.target"]
:set prompt "\ESC[34mλ: \ESC[m"
```

Last line is optional, but it's cool! Lambda prompt!

2. Run `stack ghci`

3. Run `:set -XOverloadedStrings`

4. Then the dev feedback loop goes as follows:

- Make changes to Haskell code
- Run `:rr` to recompile + typecheck, and re-run `Test.target`
- Fix any issues, then `:rr` again
- If you want to just type-check _without_ running, use `:r`

Easier to change the target definition than constantly adjust the `:def` in `~/.ghci`!