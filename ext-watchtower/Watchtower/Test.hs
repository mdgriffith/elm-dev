module Watchtower.Test where

import Control.Concurrent (threadDelay)
import qualified Control.Exception
import Ext.Common
import qualified Ext.CompileMode
import qualified Ext.Log
import qualified System.Directory as Dir
import qualified System.Environment as Env
import qualified Watchtower.Server

{-

Allows us to use `:rr` in GHCI to quickly kill, typecheck, rebuild & reboot the server,
without having to build a binary and reboot it in shell.

-}

serve :: IO ()
serve = do
  projectDir <-
    Control.Exception.catch
      (Env.lookupEnv "ELM_WATCHTOWER_START_PROJECT")
      (const $ pure Nothing :: Control.Exception.IOException -> IO (Maybe String))
  Ext.CompileMode.setModeDisk
  -- Ext.CompileMode.setModeMemory
  trackedForkIO
    $ Ext.Log.withAllBut
      []
    -- Add any flags to exclude here!
    $ Watchtower.Server.serve
      projectDir
      (Watchtower.Server.Flags (Just 51213))
