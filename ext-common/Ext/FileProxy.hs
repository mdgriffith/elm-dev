module Ext.FileProxy where

{- This is a proxy for Elm's regular File.hs, allowing us to dynamically
   replace the project-wide implementation for performance testing.
 -}

-- Only ONE of the following two imports can be set
import qualified File
import qualified Ext.FileCache as FileCache

import Control.Concurrent.MVar
import System.IO.Unsafe (unsafePerformIO)
import qualified System.Environment as Env

import qualified Codec.Archive.Zip as Zip
import qualified Data.Binary as Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B

import Ext.Common (debug)

data ProxyMode = Disk | Memory deriving (Show)

-- https://stackoverflow.com/questions/16811376/simulate-global-variable trick
{-# NOINLINE proxyMode #-}
proxyMode :: MVar ProxyMode
proxyMode = unsafePerformIO $ do
  debugM <- Env.lookupEnv "MODE"
  let mode =
        case debugM of
          Just "disk" -> Disk
          Just "memory" -> Memory
          _ -> Memory
  debug $ "setting fileproxy mode: " <> show mode
  newMVar mode


withModeDisk :: IO a -> IO a
withModeDisk io = do
  modifyMVar_ proxyMode (\_ -> pure Disk)
  res <- io
  modifyMVar_ proxyMode (\_ -> pure Memory)
  pure res

{-# NOINLINE getMode #-}
getMode :: ProxyMode
getMode = unsafePerformIO $ readMVar proxyMode


type Time = File.Time



debugSummary =
  case getMode of
    Memory -> FileCache.debugSummary
    Disk -> FileCache.debugSummary -- @TODO specialise for disk mode

-- Interface proxies

getTime :: FilePath -> IO Time
getTime path =
  case getMode of
    Memory -> FileCache.getTime path
    Disk -> File.getTime path

zeroTime :: Time
zeroTime =
  case getMode of
    Memory -> FileCache.zeroTime
    Disk -> File.zeroTime

writeBinary :: (Binary.Binary a) => FilePath -> a -> IO ()
writeBinary path =
  case getMode of
    Memory -> FileCache.writeBinary path
    Disk -> File.writeBinary path

readBinary :: (Binary.Binary a) => FilePath -> IO (Maybe a)
readBinary path =
  case getMode of
    Memory -> FileCache.readBinary path
    Disk -> File.readBinary path

writeUtf8 :: FilePath -> BS.ByteString -> IO ()
writeUtf8 path content =
  case getMode of
    Memory -> FileCache.writeUtf8 path content
    Disk -> File.writeUtf8 path content

readUtf8 :: FilePath -> IO BS.ByteString
readUtf8 path =
  case getMode of
    Memory -> FileCache.readUtf8 path
    Disk -> File.readUtf8 path

writeBuilder :: FilePath -> B.Builder -> IO ()
writeBuilder path content =
  case getMode of
    Memory -> FileCache.writeBuilder path content
    Disk -> File.writeBuilder path content

writePackage :: FilePath -> Zip.Archive -> IO ()
writePackage path content =
  case getMode of
    Memory -> FileCache.writePackage path content
    Disk -> File.writePackage path content

exists :: FilePath -> IO Bool
exists path =
  case getMode of
    Memory -> FileCache.exists path
    Disk -> File.exists path

remove :: FilePath -> IO ()
remove path =
  case getMode of
    Memory -> FileCache.remove path
    Disk -> File.remove path

removeDir :: FilePath -> IO ()
removeDir path =
  case getMode of
    Memory -> FileCache.removeDir path
    Disk -> File.removeDir path
