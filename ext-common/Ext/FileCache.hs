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
import qualified System.Environment as Env
import qualified Data.Time.Clock.POSIX as Time
import qualified System.FilePath

import qualified File

import Ext.Common ((&), track, onlyWhen, justs)
import qualified GHC.Stats as RT
-- import qualified GHC.DataSize
import qualified System.Mem
import qualified Ext.Log

type HashTable k v = H.CuckooHashTable k v


{-# NOINLINE fileCache #-}
fileCache :: HashTable FilePath (Time, BS.ByteString)
fileCache = unsafePerformIO H.new


lookup :: FilePath -> IO (Maybe (Time, BS.ByteString))
lookup path = do
  -- log $ "👀 " ++ show path
  H.lookup fileCache path


insert :: FilePath -> BS.ByteString -> IO ()
insert path value = do
  -- log $ "✍️ " ++ show path
  t <- currentTime
  H.insert fileCache path (t, value)


delete :: FilePath -> IO ()
delete path = do
  t <- currentTime
  H.delete fileCache path


log :: (() -> String) -> IO ()
log toMessage =
  Ext.Log.ifActive Ext.Log.FileProxy
    (\() -> 
      Ext.Log.log Ext.Log.FileProxy (toMessage ())    
    )
  


currentTime :: IO Time
currentTime =
  fmap (File.Time . Time.nominalDiffTimeToSeconds . Time.utcTimeToPOSIXSeconds) Time.getCurrentTime



handleIfChanged :: [FilePath] -> ([FilePath] -> IO a) -> IO a
handleIfChanged paths action = do
  changes <- justs <$> traverse upsertPath paths
  if changes == []
    then do
      log $ \() -> "🙈 handleIfChanged: no changes! ignoring: " <> show paths
      action changes
    else do
      log $ \() -> "👀 handleIfChanged: some changes: " <> show changes
      action changes


upsertPath :: FilePath -> IO (Maybe FilePath)
upsertPath path = do
  res <- lookup path
  case res of
    Just (oldTime, oldContents) -> do
      newContents <- File.readUtf8 path
      if oldContents /= newContents
        then do
          insert path newContents
          pure $ Just path
        else do
          pure Nothing

    Nothing -> do
      newContents <- File.readUtf8 path
      insert path newContents
      pure $ Just path


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
      -- putStrLn $ "✅👀r " ++ show path
      if x == "E"
        then do
          t <- File.readUtf8 path
          insert path t
          pure t
        else
          pure x

    Nothing -> do
      log $ \() -> "❌👀r " ++ (System.FilePath.takeFileName (show path))
      -- @watch can probably be dropped when we know we can rely on fsnotify setup
      t <- File.readUtf8 path
      insert path t
      pure t

writeUtf8 :: FilePath -> BS.ByteString -> IO ()
writeUtf8 path content = do
  log $ \() -> "✍️ " ++ (System.FilePath.takeFileName (show path))
  -- onlyWhen (not $ List.isInfixOf "/elm-stuff/" path) $
  insert path content
  File.writeUtf8 path content


-- @TODO potentially skip binary serialisation entirely
writeBinary :: (Binary.Binary a) => FilePath -> a -> IO ()
writeBinary path value = do
  log $ \() -> "✍️  Binary " ++ (System.FilePath.takeFileName (show path))
  -- onlyWhen (not $ List.isInfixOf "/elm-stuff/" path) $
  insert path $ BSL.toStrict $ Binary.encode value
  -- File.writeBinary path value


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
      exists <- File.exists path
      if exists then do 
        -- log $ "❌👀rb " ++ show path
        t <- readUtf8 path
        insert path t
        case Binary.decodeOrFail $ BSL.fromStrict t of
          Right (bs, offset, a) ->
            pure (Just a)
          Left (bs, offset, message) ->
            pure Nothing
      else 
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
remove path = do
  delete path
  File.remove path

removeDir :: FilePath -> IO ()
removeDir path = do
  delete path
  File.removeDir path




-- Debugging
debugSummary :: IO ()
debugSummary = 
  Ext.Log.ifActive Ext.Log.Performance $ \_ -> do
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
        & fmap (\(fn,label) -> "    " ++  label ++ ": " ++ show (bytesToMb (fromIntegral $ fn gc)) ++"MB" )
        & List.intercalate "\n"

    log $ \() -> "🧠 filecache entries:" ++ show entries ++ " size:~" ++ size ++ "MB overhead:" ++ show overhead
    log $ \() -> "🧠 average live data:~" ++ show averageLiveData ++ "MB"
    -- log $ "🧠 gc:" ++ show (gcstatsHuman s)
    log $ \() -> "🧠 gc:\n" ++ stats


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
