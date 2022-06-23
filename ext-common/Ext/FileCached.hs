{-# LANGUAGE OverloadedStrings #-}

module Ext.FileCached where

import Prelude hiding (lookup, log)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.HashTable.IO as H
-- import Data.Text (Text)
-- import qualified Data.Text as T
-- import qualified Data.Text.IO
-- import qualified Data.Text.Lazy.Encoding as TLE

import qualified File
import qualified Data.Binary as Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Codec.Archive.Zip as Zip

import Ext.Common

type HashTable k v = H.CuckooHashTable k v

-- https://stackoverflow.com/questions/16811376/simulate-global-variable trick
{-# NOINLINE fileCache #-}
fileCache :: HashTable FilePath BS.ByteString
fileCache = unsafePerformIO $ do
  ht <- H.new
  -- H.insert ht "" 1
  pure ht


lookup path = do
  -- log $ "👀 " ++ show path
  H.lookup fileCache path

insert path value = do
  -- log $ "✍️ " ++ show path
  H.insert fileCache path value


log :: String -> IO ()
log v =
  -- pure ()
  debug v


{- builder/src/File.* interface equivalents -}

exists :: FilePath -> IO Bool
exists path = do
  res <- lookup path
  case res of
    Just x -> do
      log $ "✅👀x " ++ show path
      pure True
    Nothing -> do
      log $ "❌👀x " ++ show path
      exists <- File.exists path
      -- @watch can probably be dropped when we know we can rely on fsnotify setup
      onlyWhen exists $ do
        -- Optimistically cache the file knowing it will likely be ready shortly
        t <- File.readUtf8 path
        insert path t
      pure exists


readUtf8 :: FilePath -> IO BS.ByteString
readUtf8 path = do
  res <- lookup path
  case res of
    Just x -> do
      log $ "✅👀r " ++ show path
      pure x
    Nothing -> do
      log $ "❌👀r " ++ show path
      -- @watch can probably be dropped when we know we can rely on fsnotify setup
      t <- File.readUtf8 path
      insert path t
      pure t


-- @TODO potentially skip binary serialisation entirely
writeBinary :: (Binary.Binary a) => FilePath -> a -> IO ()
writeBinary path value = do
  log $ "✍️ " ++ show path
  insert path $ BSL.toStrict $ Binary.encode value


-- @TODO potentially skip binary serialisation entirely
readBinary :: (Binary.Binary a) => FilePath -> IO (Maybe a)
readBinary path = do
  res <- lookup path
  case res of
    Just x -> do
      log $ "✅👀rb " ++ show path
      case Binary.decodeOrFail $ BSL.fromStrict x of
        Right (bs, offset, a) ->
          pure (Just a)
        Left (bs, offset, message) ->
          pure Nothing

    Nothing -> do
      -- log $ "❌👀rb " ++ show path
      t <- readUtf8 path
      insert path t
      case Binary.decodeOrFail $ BSL.fromStrict t of
        Right (bs, offset, a) ->
          pure (Just a)
        Left (bs, offset, message) ->
          pure Nothing


type Time = File.Time


getTime :: FilePath -> IO Time
getTime path =
  -- @TODO File modification times should become irrelevant in FileCached mode
  pure (File.Time 0)


-- Not needed in cache?
writePackage :: FilePath -> Zip.Archive -> IO ()
writePackage = File.writePackage

