{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ext.FileCache where

import Prelude hiding (lookup, log)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.HashTable.IO as H
import qualified Data.List as List
import qualified Data.Binary as Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BSL
import qualified Codec.Archive.Zip as Zip
import qualified Data.Time.Clock as Time
import qualified Data.Time.Clock.POSIX as Time

import qualified File

import Ext.Common ((&), debug, track, onlyWhen)
import qualified GHC.Stats as RT
-- import qualified GHC.DataSize
import qualified System.Mem


type HashTable k v = H.CuckooHashTable k v


{-# NOINLINE fileCache #-}
fileCache :: HashTable FilePath (Time, BS.ByteString)
fileCache = unsafePerformIO H.new


lookup path = do
  -- log $ "👀 " ++ show path
  H.lookup fileCache path

insert path value = do
  -- log $ "✍️ " ++ show path
  t <- currentTime
  H.insert fileCache path (t, value)


log :: String -> IO ()
log v =
  -- pure ()
  debug v


currentTime :: IO Time
currentTime =
  fmap (File.Time . Time.nominalDiffTimeToSeconds . Time.utcTimeToPOSIXSeconds) Time.getCurrentTime



{- builder/src/File.* interface equivalents -}

exists :: FilePath -> IO Bool
exists path = do
  res <- lookup path
  case res of
    Just (t, x) -> do
      -- log $ "✅👀x " ++ show path
      pure (x /= "X")
    Nothing -> do
      -- log $ "❌👀x " ++ show path
      exists <- File.exists path
      if exists
        then do
          insert path "E"
          pure exists
          -- Optimistically cache the file knowing it will likely be read shortly
          -- t <- File.readUtf8 path
          -- insert path t
        else do
          insert path "X"
          pure exists
      -- @watch can probably be dropped when we know we can rely on fsnotify setup
      -- onlyWhen exists $ do
      --   -- Optimistically cache the file knowing it will likely be ready shortly
      --   t <- File.readUtf8 path
      --   insert path t
      -- pure exists


readUtf8 :: FilePath -> IO BS.ByteString
readUtf8 path = do
  res <- lookup path
  case res of
    Just (t, x) -> do
      -- log $ "✅👀r " ++ show path
      if x == "E"
        then do
          t <- File.readUtf8 path
          insert path t
          pure t
        else
          pure x

    Nothing -> do
      log $ "❌👀r " ++ show path
      -- @watch can probably be dropped when we know we can rely on fsnotify setup
      t <- File.readUtf8 path
      insert path t
      pure t

writeUtf8 :: FilePath -> BS.ByteString -> IO ()
writeUtf8 path content = do
  log $ "✍️ " ++ show path
  -- onlyWhen (not $ List.isInfixOf "/elm-stuff/" path) $
  insert path content
  File.writeUtf8 path content


-- @TODO potentially skip binary serialisation entirely
writeBinary :: (Binary.Binary a) => FilePath -> a -> IO ()
writeBinary path value = do
  log $ "✍️ B " ++ show path
  -- onlyWhen (not $ List.isInfixOf "/elm-stuff/" path) $
  insert path $ BSL.toStrict $ Binary.encode value
  File.writeBinary path value


-- @TODO potentially skip binary serialisation entirely
readBinary :: (Binary.Binary a) => FilePath -> IO (Maybe a)
readBinary path = do
  res <- lookup path
  case res of
    Just (t, x) -> do
      if x == "E"
        then do
          t <- readUtf8 path
          insert path t
          case Binary.decodeOrFail $ BSL.fromStrict t of
            Right (bs, offset, a) ->
              pure (Just a)
            Left (bs, offset, message) ->
              pure Nothing
        else
          -- log $ "✅👀rb " ++ show path
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
getTime path = do
  -- @TODO File modification times should become irrelevant in FileCache mode?
  res <- lookup path
  case res of
    Just (t, x) -> do
      pure t

    Nothing -> do
      File.getTime path


zeroTime :: Time
zeroTime =
  File.Time 0


-- Not needed in cache?
writePackage :: FilePath -> Zip.Archive -> IO ()
writePackage = File.writePackage

writeBuilder :: FilePath -> B.Builder -> IO ()
writeBuilder = File.writeBuilder

remove :: FilePath -> IO ()
remove path = File.remove path

removeDir :: FilePath -> IO ()
removeDir path = File.removeDir path




-- Debugging

debugSummary = do
  -- track "🧹 majorGC" $ System.Mem.performMajorGC

  fileCacheList <- H.toList fileCache
  s <- RT.getRTSStats

  overhead <- H.computeOverhead fileCache

  -- Sadly this does not perform well enough to ever print even on a small Elm project...? :(
  -- size <- GHC.DataSize.recursiveSize fileCache

  let
    entries = Prelude.length fileCacheList

    -- @TODO memory overhead for string is 4 words per char + 2 words for the char... should swap to text/BS instead.
    size = fileCacheList & fmap (\(k,(t, v)) -> (length k * 6) + BS.length v) & sum & fromIntegral & bytesToMb & show
    -- (show $ bytesToMb (fromIntegral size))

    -- Sum of live bytes across all major GCs. Divided by major_gcs gives the average live data over the lifetime of the program.
    averageLiveData = bytesToMb $ RT.cumulative_live_bytes s `div` (fromIntegral $ RT.major_gcs s)

    gc = RT.gc s

    stats =
      [
        -- (RT.gcdetails_gen, "") -- = 1
      -- , (RT.gcdetails_threads, "") -- = 1
       (RT.gcdetails_allocated_bytes, "allocated since last GC") -- = 65812848
      , (RT.gcdetails_live_bytes, "live heap after last GC") -- = 320114080
      , (RT.gcdetails_large_objects_bytes, "large objects") -- = 76883456
      , (RT.gcdetails_compact_bytes, "compact regions") -- = 0
      , (RT.gcdetails_slop_bytes, "slop (wasted memory)") -- = 9568864
      , (RT.gcdetails_mem_in_use_bytes, "mem use RTS") -- = 884998144
      , (RT.gcdetails_copied_bytes, "copied in GC") -- = 243233048
      -- , (RT.gcdetails_par_max_copied_bytes, "") -- = 0
      -- , (RT.gcdetails_par_balanced_copied_bytes, "par GC balance copy") -- = 0
      -- , (RT.gcdetails_sync_elapsed_ns, "") -- = 11208
      -- , (RT.gcdetails_cpu_ns, "") -- = 268024000
      -- , (RT.gcdetails_elapsed_ns, "") -- = 304688708
      -- , (RT.gcdetails_nonmoving_gc_sync_cpu_ns, "") -- = 0
      -- , (RT.gcdetails_nonmoving_gc_sync_elapsed_ns, "") -- = 0}}
      ]
      & fmap (\(fn,label) -> label ++ ": " ++ show (bytesToMb (fromIntegral $ fn gc)) ++"MB" )
      & List.intercalate "\n"

  debug $ "🧠 filecache entries:" ++ show entries ++ " size:~" ++ size ++ "MB overhead:" ++ show overhead
  debug $ "🧠 average live data:~" ++ show averageLiveData ++ "MB"
  -- debug $ "🧠 gc:" ++ show (gcstatsHuman s)
  debug $ "🧠 gc:\n" ++ stats


gcstatsHuman s =
  s { RT.gcs = RT.gcs s
  , RT.major_gcs = RT.major_gcs s
  , RT.allocated_bytes = RT.allocated_bytes s & bytesToMb
  , RT.max_live_bytes = RT.max_live_bytes s & bytesToMb
  , RT.max_large_objects_bytes = RT.max_large_objects_bytes s & bytesToMb
  , RT.max_compact_bytes = RT.max_compact_bytes s & bytesToMb
  , RT.max_slop_bytes = RT.max_slop_bytes s & bytesToMb
  , RT.max_mem_in_use_bytes = RT.max_mem_in_use_bytes s & bytesToMb
  , RT.cumulative_live_bytes = RT.cumulative_live_bytes s & bytesToMb
  , RT.copied_bytes = RT.copied_bytes s & bytesToMb
  , RT.par_copied_bytes = RT.par_copied_bytes s & bytesToMb
  , RT.cumulative_par_max_copied_bytes = RT.cumulative_par_max_copied_bytes s & bytesToMb
  , RT.cumulative_par_balanced_copied_bytes = RT.cumulative_par_balanced_copied_bytes s & bytesToMb
  , RT.init_cpu_ns = RT.init_cpu_ns s
  , RT.init_elapsed_ns = RT.init_elapsed_ns s
  , RT.mutator_cpu_ns = RT.mutator_cpu_ns s
  , RT.mutator_elapsed_ns = RT.mutator_elapsed_ns s
  , RT.gc_cpu_ns = RT.gc_cpu_ns s
  , RT.gc_elapsed_ns = RT.gc_elapsed_ns s
  , RT.cpu_ns = RT.cpu_ns s
  , RT.elapsed_ns = RT.elapsed_ns s
  , RT.nonmoving_gc_sync_cpu_ns = RT.nonmoving_gc_sync_cpu_ns s
  , RT.nonmoving_gc_sync_elapsed_ns = RT.nonmoving_gc_sync_elapsed_ns s
  , RT.nonmoving_gc_sync_max_elapsed_ns = RT.nonmoving_gc_sync_max_elapsed_ns s
  , RT.nonmoving_gc_cpu_ns = RT.nonmoving_gc_cpu_ns s
  , RT.nonmoving_gc_elapsed_ns = RT.nonmoving_gc_elapsed_ns s
  , RT.nonmoving_gc_max_elapsed_ns = RT.nonmoving_gc_max_elapsed_ns s
  , RT.gc = RT.gc s
  }


bytesToMb :: Binary.Word64 -> Binary.Word64
bytesToMb b =
  b `div` (1024 * 1024)
