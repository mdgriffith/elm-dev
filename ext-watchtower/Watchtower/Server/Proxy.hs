{-# LANGUAGE CPP #-}

module Watchtower.Server.Proxy (run) where

import qualified Control.Exception as Exception
import qualified Ext.Log
import qualified System.Directory as Dir
import qualified System.Environment
import qualified System.Exit as Exit
import qualified System.FilePath as Path
import qualified System.IO as IO
import qualified System.Process as Process

#if !defined(mingw32_HOST_OS)
import qualified System.Posix.Process as Posix
#endif


-- | Always exec the external elm-dev-proxy. If unavailable or it fails, exit with an error.
run ::  String -> Int -> IO ()
run host port = do
  maybeProxy <- findProxyPath
  case maybeProxy of
    Just proxyPath -> do
#if !defined(mingw32_HOST_OS)
      let args = [host, show port]
      let handler :: Exception.SomeException -> IO ()
          handler e = do
            IO.hPutStrLn IO.stderr ("Failed to execute elm-dev-proxy: " ++ show e)
            IO.hPutStrLn IO.stderr "Ensure elm-dev-proxy is installed and accessible."
            Exit.exitFailure
      Exception.catch (Posix.executeFile proxyPath False args Nothing) handler
#else
      let cp = (Process.proc proxyPath [host, show port])
      let runProc = Process.withCreateProcess cp { Process.std_in = Process.Inherit, Process.std_out = Process.Inherit, Process.std_err = Process.Inherit } $ \_ _ _ ph -> Process.waitForProcess ph >> pure ()
      let handler :: Exception.SomeException -> IO ()
          handler e = do
            IO.hPutStrLn IO.stderr ("Failed to execute elm-dev-proxy: " ++ show e)
            IO.hPutStrLn IO.stderr "Ensure elm-dev-proxy is installed and accessible."
            Exit.exitFailure
      Exception.catch runProc handler
#endif
    Nothing -> do
      IO.hPutStrLn IO.stderr "elm-dev-proxy not found."
      IO.hPutStrLn IO.stderr "Set ELM_DEV_PROXY to the proxy path or place elm-dev-proxy alongside the elm-dev executable."
      Exit.exitFailure


findProxyPath :: IO (Maybe FilePath)
findProxyPath = do
  -- 1) Explicit env var override
  envOverride <- System.Environment.lookupEnv "ELM_DEV_PROXY"
  case envOverride of
    Just p -> do
      exists <- Dir.doesFileExist p
      if exists then pure (Just p) else pure Nothing
    Nothing -> do
      -- 2) Look next to current executable (npm hard-link case as well as direct installs)
      exePath <- System.Environment.getExecutablePath
      let exeDir = Path.takeDirectory exePath
#if defined(mingw32_HOST_OS)
      let candidate = exeDir Path.</> "elm-dev-proxy.exe"
#else
      let candidate = exeDir Path.</> "elm-dev-proxy"
#endif
      exists2 <- Dir.doesFileExist candidate
      if exists2 then pure (Just candidate) else pure Nothing



