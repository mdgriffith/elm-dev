module Ext.FileProxy where

-- @TODO this setup is probably no longer necessary given we have CompileProxy now?


{- This is a proxy for Elm's regular File.hs, allowing us to dynamically
   replace the project-wide implementation for performance testing.
 -}

import qualified File
import qualified Ext.FileCache as FileCache

import Control.Concurrent.MVar
import System.IO.Unsafe (unsafePerformIO)
import qualified System.Environment as Env

import qualified Codec.Archive.Zip as Zip
import qualified Data.Binary as Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B


import Ext.CompileMode (getMode, CompileMode(..))


type Time = File.Time


debugSummary =
  case getMode of
    Memory -> FileCache.debugSummary
    Disk -> FileCache.debugSummary
    Race -> FileCache.debugSummary

-- Interface proxies

getTime :: FilePath -> IO Time
getTime path =
  case getMode of
    Memory -> FileCache.getTime path
    Disk -> File.getTime path
    Race -> File.getTime path

zeroTime :: Time
zeroTime =
  case getMode of
    Memory -> FileCache.zeroTime
    Disk -> File.zeroTime
    Race -> File.zeroTime

writeBinary :: (Binary.Binary a) => FilePath -> a -> IO ()
writeBinary path =
  case getMode of
    Memory -> FileCache.writeBinary path
    Disk -> File.writeBinary path
    Race -> File.writeBinary path

readBinary :: (Binary.Binary a) => FilePath -> IO (Maybe a)
readBinary path =
  case getMode of
    Memory -> FileCache.readBinary path
    Disk -> File.readBinary path
    Race -> File.readBinary path

writeUtf8 :: FilePath -> BS.ByteString -> IO ()
writeUtf8 path content =
  case getMode of
    Memory -> FileCache.writeUtf8 path content
    Disk -> File.writeUtf8 path content
    Race -> File.writeUtf8 path content

readUtf8 :: FilePath -> IO BS.ByteString
readUtf8 path =
  case getMode of
    Memory -> FileCache.readUtf8 path
    Disk -> File.readUtf8 path
    Race -> File.readUtf8 path

writeBuilder :: FilePath -> B.Builder -> IO ()
writeBuilder path content =
  case getMode of
    Memory -> FileCache.writeBuilder path content
    Disk -> File.writeBuilder path content
    Race -> File.writeBuilder path content

writePackage :: FilePath -> Zip.Archive -> IO ()
writePackage path content =
  case getMode of
    Memory -> FileCache.writePackage path content
    Disk -> File.writePackage path content
    Race -> File.writePackage path content

exists :: FilePath -> IO Bool
exists path =
  case getMode of
    Memory -> FileCache.exists path
    Disk -> File.exists path
    Race -> File.exists path

remove :: FilePath -> IO ()
remove path =
  case getMode of
    Memory -> FileCache.remove path
    Disk -> File.remove path
    Race -> File.remove path

removeDir :: FilePath -> IO ()
removeDir path =
  case getMode of
    Memory -> FileCache.removeDir path
    Disk -> File.removeDir path
    Race -> File.removeDir path
