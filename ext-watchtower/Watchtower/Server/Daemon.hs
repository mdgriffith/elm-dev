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
import qualified Watchtower.Live
import qualified Watchtower.Live.Client
import qualified Watchtower.Server.Run
import qualified Watchtower.Server.LSP as LSP
import qualified Watchtower.Server.MCP as MCP
import qualified Snap.Http.Server as Server
import qualified Watchtower.Server.Dev
import qualified Data.Bits as Bits
import qualified Data.Word as Word
import qualified System.Process as Process
import qualified Control.Exception as Exception

#if !defined(mingw32_HOST_OS)
import qualified System.Posix.Process as Posix
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
  xdg <- Dir.getXdgDirectory Dir.XdgCache "elm-dev"
  let dir = xdg Path.</> "daemon"
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
  mState <- readState
  case mState of
    Just s -> do
      healthy <- isHealthy s
      if healthy then pure s else start
    Nothing -> start

-- | Launch the daemon in the background if not running, wait until state is ready, and return it
start :: IO StateInfo
start = do
  exe <- Env.getExecutablePath
  let cp = (Process.proc exe ["daemon","serve"])
              { Process.std_in = Process.NoStream
              , Process.std_out = Process.NoStream
              , Process.std_err = Process.Inherit
#if !defined(mingw32_HOST_OS)
              , Process.new_session = True
#endif
              }
  _ <- Process.createProcess cp
  -- Wait for state.json to appear and parse
  let waitLoop n = do
        ms <- readState
        case ms of
          Just s -> pure s
          Nothing -> if n <= 0 then ioError (userError "daemon did not start") else do
            Concurrent.threadDelay 200000 -- 200ms
            waitLoop (n - 1)
  waitLoop (50 :: Int)

-- | Blocking daemon server: start services and keep running
serve :: ServeParams -> IO ()
serve params = do
  IO.hSetBuffering IO.stdout IO.LineBuffering
  IO.hSetBuffering IO.stderr IO.LineBuffering
  Ext.Log.withAllBut [Ext.Log.Performance, Ext.Log.FileProxy] $ do
    Ext.CompileMode.setModeMemory
    let httpUrl = "http://" ++ spDomain params ++ ":" ++ show (spHttpPort params)
    let wsUrl = "ws://" ++ spDomain params ++ ":" ++ show (spHttpPort params) ++ "/ws"
    let lspUrl = "tcp://" ++ spDomain params ++ ":" ++ show (spLspPort params)
    let mcpUrl = "tcp://" ++ spDomain params ++ ":" ++ show (spMcpPort params)
    live <- Watchtower.Live.initWithUrls (Watchtower.Live.Client.Urls (Just lspUrl) (Just mcpUrl) httpUrl wsUrl)
    let debug = False
    _ <- Concurrent.forkIO $ Server.httpServe 
          (Watchtower.Server.Run.config (spHttpPort params) debug)
          (Watchtower.Live.websocket live 
            <|> Watchtower.Server.Run.runHttp live Watchtower.Server.Dev.serve (\_ _ _ -> pure ())
          )
    -- Start LSP TCP
    _ <- Concurrent.forkIO $ Watchtower.Server.Run.runTcp live LSP.serve LSP.handleNotification (spDomain params) (spLspPort params)
    -- Start MCP TCP
    _ <- Concurrent.forkIO $ Watchtower.Server.Run.runTcp live MCP.serve (\_ _ _ -> pure ()) (spDomain params) (spMcpPort params)
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
    printBanner params
    now <- fmap show getCurrentTime
    p <- getPid
    let st = StateInfo { pid = p, domain = spDomain params, lspPort = spLspPort params, mcpPort = spMcpPort params, httpPort = spHttpPort params, startedAt = now, version = versionString }
    writeState st
    -- Block forever
    let loop = do Concurrent.threadDelay 10000000 >> loop
    loop

stop :: IO ()
stop = do
  -- Minimal stub; proper stop via PID signaling can be added per-OS
  pure ()

status :: IO (Maybe StateInfo)
status = readState

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
  l <- canConnect "127.0.0.1" (lspPort st)
  m <- canConnect "127.0.0.1" (mcpPort st)
  pure (l && m)

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
  mapM_ (IO.hPutStrLn IO.stderr) logo
  let host = spDomain sp
  IO.hPutStrLn IO.stderr (yellow (" HTTP ") ++ " http://" ++ host ++ ":" ++ show (spHttpPort sp) ++ grey "  (dev server)")
  IO.hPutStrLn IO.stderr (yellow ("  LSP ") ++ "  tcp://" ++ host ++ ":" ++ show (spLspPort sp))
  IO.hPutStrLn IO.stderr (yellow ("  MCP ") ++ "  tcp://" ++ host ++ ":" ++ show (spMcpPort sp))
  IO.hPutStrLn IO.stderr ""


