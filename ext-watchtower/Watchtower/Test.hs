module Watchtower.Test where

import Control.Concurrent (threadDelay)
import qualified System.Directory as Dir
import qualified System.Environment as Env

import Ext.Common
import qualified Watchtower.Server

{-

Allows us to use `:rr` in GHCI to quickly kill, typecheck, rebuild & reboot the server,
without having to build a binary and reboot it in shell.

-}

serve = do
  dir <- Dir.getCurrentDirectory
  Env.setEnv "PROJECT" $ dir </> "ext-watchtower/app"
  threadDelay (10 * 1000) -- 10ms delay to let env changes settle

  trackedForkIO $
    withDebug $
      Watchtower.Server.serve
          (Just "/Users/matthewgriffith/projects/blissfully/elm-watchtower-compiler/watchtower-frontends/vscode/testProject")
          (Watchtower.Server.Flags Nothing)
