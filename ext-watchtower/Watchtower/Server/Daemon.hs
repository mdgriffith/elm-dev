{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Watchtower.Server.Daemon
  ( StateInfo(..)
  , ServeParams(..)
  , allocateServeParams
  , start
  , stop
  , status
  , ensureRunning
  , printBanner
  , serve
  ) where

import Control.Applicative ((<|>))
import qualified Control.Concurrent as Concurrent
import qualified Control.Monad as Monad
import qualified Control.Exception as Exception
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as List
import qualified Ext.CompileMode
import qualified Ext.Log
import Data.Time.Clock (getCurrentTime)
import qualified System.Directory as Dir
import qualified System.Environment as Env
import qualified System.FilePath as Path
import qualified System.IO as IO
import qualified Network.Socket as Net
import qualified Watchtower.Server.DevWS as DevWS
import qualified Watchtower.Live.Client
import qualified Stuff
import qualified Watchtower.Server.Run
import qualified Watchtower.Server.LSP as LSP
import qualified Watchtower.Server.MCP as MCP
import qualified Snap.Http.Server as Server
import qualified Watchtower.Server.Dev
import qualified Snap.Core (route)
import qualified Data.Bits as Bits
import qualified Data.Word as Word
import qualified System.Process as Process
import qualified Control.Exception as Exception

#if !defined(mingw32_HOST_OS)
import qualified System.Posix.Process as Posix
import qualified System.Posix.IO as PosixIO
import qualified System.Posix.Types as PosixTypes
import qualified System.Posix.Signals as PosixSig
#endif

#if defined(mingw32_HOST_OS)
import qualified System.Win32.Process as Win32
#endif

data StateInfo = StateInfo
  { pid :: !Int
  , domain :: !String
  , lspPort :: !Int
  , mcpPort :: !Int
  , httpPort :: !Int
  , startedAt :: !String
  , version :: !String
  } deriving (Show, Eq)

instance JSON.ToJSON StateInfo where
  toJSON s = JSON.object
    [ ("pid", JSON.toJSON (pid s))
    , ("domain", JSON.toJSON (domain s))
    , ("lspPort", JSON.toJSON (lspPort s))
    , ("mcpPort", JSON.toJSON (mcpPort s))
    , ("httpPort", JSON.toJSON (httpPort s))
    , ("startedAt", JSON.toJSON (startedAt s))
    , ("version", JSON.toJSON (version s))
    ]

instance JSON.FromJSON StateInfo where
  parseJSON = JSON.withObject "StateInfo" $ \o -> do
    -- domain was introduced later; default to 127.0.0.1 for older state files
    domainVal <- o JSON..:? "domain" JSON..!= "127.0.0.1"
    StateInfo <$> o JSON..: "pid"
              <*> pure domainVal
              <*> o JSON..: "lspPort"
              <*> o JSON..: "mcpPort"
              <*> o JSON..: "httpPort"
              <*> o JSON..: "startedAt"
              <*> o JSON..: "version"

-- Global daemon state directory/file (single daemon across all workspaces)
stateDir :: IO FilePath
stateDir = do
  elmHome <- Stuff.getElmHome
  let dir = elmHome Path.</> "daemon"
  Dir.createDirectoryIfMissing True dir
  pure dir

stateFilePath :: IO FilePath
stateFilePath = do
  dir <- stateDir
  pure (dir Path.</> "state.json")

lockFilePath :: IO FilePath
lockFilePath = do
  dir <- stateDir
  pure (dir Path.</> "daemon.lock")

readState :: IO (Maybe StateInfo)
readState = do
  path <- stateFilePath
  exists <- Dir.doesFileExist path
  if not exists then pure Nothing else do
    bytes <- LBS.readFile path
    case JSON.eitherDecode bytes of
      Left _ -> pure Nothing
      Right s -> pure (Just s)

writeState :: StateInfo -> IO ()
writeState s = do
  path <- stateFilePath
  let tmp = path ++ ".tmp"
  LBS.writeFile tmp (JSON.encode s)
  Dir.renameFile tmp path

-- Probe for a free TCP port on 127.0.0.1 by binding to port 0
allocatePort :: IO Int
allocatePort = do
  addrs <- Net.getAddrInfo (Just (Net.defaultHints { Net.addrFlags = [Net.AI_PASSIVE] })) (Just "127.0.0.1") (Just "0")
  let addr = head addrs
  sock <- Net.socket (Net.addrFamily addr) Net.Stream Net.defaultProtocol
  Net.setSocketOption sock Net.ReuseAddr 1
  Net.bind sock (Net.addrAddress addr)
  sockAddr <- Net.getSocketName sock
  port <- case sockAddr of
            Net.SockAddrInet p _ -> pure (fromIntegral p)
            Net.SockAddrInet6 p _ _ _ -> pure (fromIntegral p)
            _ -> pure 0
  Net.close sock
  pure port

getPid :: IO Int
getPid = do
#if defined(mingw32_HOST_OS)
  p <- Win32.getCurrentProcessId
  pure (fromIntegral p)
#else
  p <- Posix.getProcessID
  pure (fromIntegral p)
#endif

versionString :: String
versionString = "1.0.0"

-- Parameters for starting the daemon services
data ServeParams = ServeParams
  { spDomain :: !String
  , spLspPort :: !Int
  , spMcpPort :: !Int
  , spHttpPort :: !Int
  }

-- Allocate domain and free ports required for all services
allocateServeParams :: IO ServeParams
allocateServeParams = do
  lspP <- allocatePort
  mcpP <- allocatePort
  httpP <- allocatePort
  pure (ServeParams { spDomain = "127.0.0.1", spLspPort = lspP, spMcpPort = mcpP, spHttpPort = httpP })

-- | Start the daemon servers for a workspace root; if already running and healthy, return its state
ensureRunning :: IO StateInfo
ensureRunning = do
  mState <- status
  case mState of
    Just s -> pure s
    Nothing -> withDaemonLock $ do
      -- Double-check under lock in case another process just started it
      m2 <- status
      case m2 of
        Just s2 -> pure s2
        Nothing -> start

-- | Launch the daemon in the background if not running, wait until state is ready, and return it
start :: IO StateInfo
start = do
  exe <- Env.getExecutablePath
  let cp = (Process.proc exe ["dev","serve","--silent"])
              { Process.std_in = Process.NoStream
              , Process.std_out = Process.NoStream
              , Process.std_err = Process.Inherit
#if !defined(mingw32_HOST_OS)
              , Process.new_session = True
#endif
              }
  _ <- Process.createProcess cp
  -- Wait for state.json to appear and for services to become healthy
  let waitLoop n = do
        ms <- status
        case ms of
          Just s -> pure s
          Nothing -> if n <= 0 then ioError (userError "dev did not become healthy") else do
            Concurrent.threadDelay 200000 -- 200ms
            waitLoop (n - 1)
  waitLoop (50 :: Int)

-- | Blocking daemon server: start services and keep running
serve :: ServeParams -> IO ()
serve params = do
  IO.hSetBuffering IO.stdout IO.LineBuffering
  IO.hSetBuffering IO.stderr IO.LineBuffering
  path <- stateFilePath
  Exception.bracket_ (pure ()) (safeRemove path) $ do
    Ext.Log.withAllBut [Ext.Log.Performance, Ext.Log.FileProxy] $ do
      Ext.CompileMode.setModeMemory
      let httpUrl = "http://" ++ spDomain params ++ ":" ++ show (spHttpPort params)
      let wsUrl = "ws://" ++ spDomain params ++ ":" ++ show (spHttpPort params) ++ "/ws"
      let lspUrl = "tcp://" ++ spDomain params ++ ":" ++ show (spLspPort params)
      let mcpUrl = "tcp://" ++ spDomain params ++ ":" ++ show (spMcpPort params)
      live <- Watchtower.Live.Client.initState (Watchtower.Live.Client.Urls (Just lspUrl) (Just mcpUrl) httpUrl wsUrl)
      let debug = False
      _ <- Concurrent.forkIO $ Server.httpServe 
            (Watchtower.Server.Run.config (spHttpPort params) debug)
            (DevWS.websocket live 
              <|> Snap.Core.route (Watchtower.Server.Dev.routes live)
            )
      -- Start LSP TCP
      _ <- Concurrent.forkIO $ Watchtower.Server.Run.runTcp live LSP.serve LSP.handleNotification (spDomain params) (spLspPort params)
      -- Start MCP TCP
      _ <- Concurrent.forkIO $ Watchtower.Server.Run.runTcp live MCP.serve (\_ _ _ _ -> pure ()) (spDomain params) (spMcpPort params)
      -- Wait until listeners accept connections before writing state
      let waitReady attempts = do
            l1 <- canConnect (spDomain params) (spLspPort params)
            l2 <- canConnect (spDomain params) (spMcpPort params)
            l3 <- canConnect (spDomain params) (spHttpPort params)
            if l1 && l2 && l3 then pure () else if attempts <= (0 :: Int)
              then ioError (userError "daemon failed to start listeners")
              else do
                Concurrent.threadDelay 100000 -- 100ms
                waitReady (attempts - 1)
      waitReady (100 :: Int)
      now <- fmap show getCurrentTime
      p <- getPid
      let st = StateInfo { pid = p, domain = spDomain params, lspPort = spLspPort params, mcpPort = spMcpPort params, httpPort = spHttpPort params, startedAt = now, version = versionString }
      writeState st
      -- Block forever
      let loop = do Concurrent.threadDelay 10000000 >> loop
      loop

stop :: IO ()
stop = do
  maybeState <- readState
  case maybeState of
    Nothing -> pure ()
    Just state -> do
 -- Unix-like OSes: Linux, macOS, etc. (not Windows): send SIGTERM and SIGKILL to the daemon process
#if !defined(mingw32_HOST_OS) 
      let pid' = PosixTypes.CPid (fromIntegral (pid state))
      _ <- (Exception.try (PosixSig.signalProcess PosixSig.sigTERM pid') :: IO (Either IOError ()))
      _ <- waitForDeath 50 (pid state)
      stillAlive2 <- pidAlive (pid state)
      Monad.when stillAlive2 $ do
        _ <- (Exception.try (PosixSig.signalProcess PosixSig.sigKILL pid') :: IO (Either IOError ()))
        _ <- waitForDeath 25 (pid state)
        pure ()
#else
      -- Windows: use taskkill to gracefully stop the daemon process
      let cp = (Process.proc "taskkill" ["/PID", show (pid state), "/T"]) -- gentle; Windows lacks SIGTERM
      _ <- Process.withCreateProcess cp { Process.std_in = Process.Inherit, Process.std_out = Process.Inherit, Process.std_err = Process.Inherit } $ \_ _ _ ph -> Process.waitForProcess ph >> pure ()
      -- Wait for ports to close as our best indication of shutdown on Windows
      _ <- waitPortsClosed st 50
#endif
      path <- stateFilePath
      safeRemove path
        

status :: IO (Maybe StateInfo)
status = do
  ms <- readState
  case ms of
    Just s -> do
      ok <- isHealthy s
      if ok then pure (Just s) else pure Nothing
    Nothing -> pure Nothing

-- | Try to connect to host:port, return True if successful
canConnect :: String -> Int -> IO Bool
canConnect host port = do
  infos <- Net.getAddrInfo Nothing (Just host) (Just (show port))
  let addr = head infos
  sock <- Net.socket (Net.addrFamily addr) Net.Stream Net.defaultProtocol
  res <- Exception.try (Net.connect sock (Net.addrAddress addr)) :: IO (Either IOError ())
  Net.close sock
  case res of
    Right _ -> pure True
    Left _ -> pure False

-- | Basic health check for daemon ports
isHealthy :: StateInfo -> IO Bool
isHealthy st = do
  alive <- pidAlive (pid st)
  if not alive then pure False else do
    let host = domain st
    l <- canConnect host (lspPort st)
    m <- canConnect host (mcpPort st)
    h <- canConnect host (httpPort st)
    pure (l && m && h)

-- Pretty startup banner with colors and service ports
printBanner :: ServeParams -> IO ()
printBanner sp = do
  let cyan s  = "\ESC[36m" ++ s ++ "\ESC[0m"
  let grey s  = "\ESC[90m" ++ s ++ "\ESC[0m"
  let yellow s = "\ESC[33m" ++ s ++ "\ESC[0m"
  let logo =
        [ ""
        , cyan "                Elm Dev Server"
        , ""
        ]
  mapM_ (IO.hPutStrLn IO.stdout) logo
  let host = spDomain sp
  IO.hPutStrLn IO.stdout (yellow (" HTTP ") ++ " http://" ++ host ++ ":" ++ show (spHttpPort sp) ++ grey "  (dev server)")
  IO.hPutStrLn IO.stdout (yellow (" WS   ") ++ "   ws://" ++ host ++ ":" ++ show (spHttpPort sp) ++ "/ws")
  IO.hPutStrLn IO.stdout (yellow (" LSP  ") ++ "  tcp://" ++ host ++ ":" ++ show (spLspPort sp))
  IO.hPutStrLn IO.stdout (yellow (" MCP  ") ++ "  tcp://" ++ host ++ ":" ++ show (spMcpPort sp))
  IO.hPutStrLn IO.stdout ""



-- | Acquire a simple inter-process lock to serialize daemon startup.
-- On POSIX, use O_EXCL file creation for atomicity; on other platforms, best-effort fallback.
withDaemonLock :: IO a -> IO a
withDaemonLock action = do
  path <- lockFilePath
#if !defined(mingw32_HOST_OS)
  let flags = PosixIO.defaultFileFlags { PosixIO.exclusive = True, PosixIO.trunc = False, PosixIO.noctty = True }
  let acquire = Exception.try (PosixIO.openFd path PosixIO.WriteOnly (Just 0o600) flags) :: IO (Either IOError PosixTypes.Fd)
  res <- acquire
  case res of
    Right fd -> Exception.bracket (pure fd)
                                  (\fd' -> PosixIO.closeFd fd' >> safeRemove path)
                                  (\_ -> action)
    Left _ -> do
      -- Another process holds the lock; wait briefly for a healthy daemon
      _ <- waitForHealthy 50
      action
#else
  -- Fallback without atomic lock; proceed directly
  action
#endif

-- | Wait up to N attempts for an existing daemon to become healthy.
waitForHealthy :: Int -> IO Bool
waitForHealthy n = do
  ms <- status
  case ms of
    Just _ -> pure True
    Nothing -> delayAndRetry
  where
    delayAndRetry = if n <= 0
      then pure False
      else do
        Concurrent.threadDelay 200000 -- 200ms
        waitForHealthy (n - 1)

safeRemove :: FilePath -> IO ()
safeRemove p = do
  e <- Dir.doesFileExist p
  if e then (Exception.catch (Dir.removeFile p) ignoreIO) else pure ()
  where
    ignoreIO :: Exception.IOException -> IO ()
    ignoreIO _ = pure ()

-- | Best-effort check whether a process with the given PID is alive.
pidAlive :: Int -> IO Bool
#if !defined(mingw32_HOST_OS)
pidAlive p = do
  let pid' = PosixTypes.CPid (fromIntegral p)
  res <- Exception.try (PosixSig.signalProcess PosixSig.nullSignal pid') :: IO (Either IOError ())
  case res of
    Right _ -> pure True
    Left _ -> pure False
#else
pidAlive _ = pure True
#endif

-- | Wait up to N attempts (200ms each) for a PID to exit.
waitForDeath :: Int -> Int -> IO Bool
waitForDeath n p = do
  alive <- pidAlive p
  if not alive then pure True else if n <= 0 then pure False else do
    Concurrent.threadDelay 200000
    waitForDeath (n - 1) p

-- | Wait until all daemon ports are closed.
waitPortsClosed :: StateInfo -> Int -> IO Bool
waitPortsClosed st n = do
  let host = domain st
  l <- canConnect host (lspPort st)
  m <- canConnect host (mcpPort st)
  h <- canConnect host (httpPort st)
  if (not l) && (not m) && (not h)
    then pure True
    else if n <= 0 then pure False else do
      Concurrent.threadDelay 200000
      waitPortsClosed st (n - 1)

