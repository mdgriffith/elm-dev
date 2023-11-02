module Test where

{-

Setup your ~/.ghci config:

```
:set -fbyte-code
:set -fobject-code
:set prompt "\ESC[34mλ: \ESC[m"

:def rr const $ return $ unlines ["Ext.Common.killTrackedThreads",":r","Test.target"]
```

Then use `:rr` to kill threads, recompile and re-run Test.target.

The current working directory will be reset back to wherever `stack ghci` was
first invoked, before the `:r` is handled.

Note: make sure to use `Ext.Common.trackedForkIO` instead of `Control.Concurrent.forkIO`,
otherwise there's no way to find+kill all child threads in GHCI context!

-}

import Watchtower.Test

target =
  Watchtower.Test.serve
