{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ext.FileCache where

import qualified Codec.Archive.Zip as Zip
import qualified Control.Monad as Monad
import qualified Data.Binary as Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashTable.IO as H
import qualified Data.List as List
import qualified Data.Time.Clock as Time
import qualified Data.Time.Clock.POSIX as Time
import Ext.Common (justs, onlyWhen, track, (&))
-- import qualified GHC.DataSize

import qualified Ext.Log
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified File
import qualified GHC.Stats as RT
import qualified System.Environment as Env
import System.FilePath ((</>))
import qualified System.FilePath
import System.IO.Unsafe (unsafePerformIO)
import qualified System.Mem
import Prelude hiding (log, lookup)

type HashTable k v = H.CuckooHashTable k v

-- * Text editing types

-- | Position in a text document (zero-based)
data Position = Position
  { positionLine :: Int,
    positionCharacter :: Int
  }
  deriving (Show, Eq)

-- | Range in a text document
data Range = Range
  { rangeStart :: Position,
    rangeEnd :: Position
  }
  deriving (Show, Eq)

-- | Text document content change event
data TextEdit = TextEdit
  { textEditRange :: Maybe Range,
    textEditText :: Text.Text
  }
  deriving (Show, Eq)

-- | File operation errors
data FileError
  = FileNotFound FilePath
  | InvalidEdit String
  deriving (Show, Eq)

virtualDir :: FilePath
virtualDir =
  "elm-stuff" </> "virtual"

-- Toggle for allowing filesystem writes
allowWrites :: Bool
allowWrites = False

allowPackageWrites :: Bool
allowPackageWrites = True

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
  Ext.Log.ifActive
    Ext.Log.FileProxy
    ( \() ->
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

-- | Apply text edits to a file in the cache
edit :: FilePath -> [TextEdit] -> IO (Either FileError ())
edit path edits = do
  res <- lookup path
  case res of
    Nothing -> do
      -- File not in cache, try to read from disk
      fileExists <- File.exists path
      if fileExists
        then do
          content <- File.readUtf8 path
          insert path content
          applyEdits path (Text.decodeUtf8 content) edits
        else
          pure $ Left (FileNotFound path)
    Just (_, content) -> do
      applyEdits path (Text.decodeUtf8 content) edits

-- | Apply a list of text edits to file content
applyEdits :: FilePath -> Text.Text -> [TextEdit] -> IO (Either FileError ())
applyEdits path content edits = do
  case foldM applyEdit content edits of
    Left err -> pure $ Left (InvalidEdit err)
    Right newContent -> do
      insert path (Text.encodeUtf8 newContent)
      pure $ Right ()
  where
    foldM :: (a -> b -> Either String a) -> a -> [b] -> Either String a
    foldM _ acc [] = Right acc
    foldM f acc (x:xs) = case f acc x of
      Left err -> Left err
      Right acc' -> foldM f acc' xs

-- | Apply a single text edit to content
applyEdit :: Text.Text -> TextEdit -> Either String Text.Text
applyEdit content (TextEdit maybeRange newText) =
  case maybeRange of
    Nothing -> 
      -- Full document replacement
      Right newText
    Just range ->
      -- Incremental edit
      applyRangeEdit content range newText

-- | Apply a ranged edit to text content
applyRangeEdit :: Text.Text -> Range -> Text.Text -> Either String Text.Text
applyRangeEdit content range newText = do
  startOffset <- positionToOffset content (rangeStart range)
  endOffset <- positionToOffset content (rangeEnd range)
  if startOffset > endOffset
    then Left $ "Invalid range: start offset " ++ show startOffset ++ " is after end offset " ++ show endOffset
    else
      Right
        ( Text.take startOffset content
            <> newText
            <> Text.drop endOffset content
        )

positionToOffset :: Text.Text -> Position -> Either String Int
positionToOffset content (Position line char)
  | line < 0 = Left $ "Invalid line number: " ++ show line
  | char < 0 = Left $ "Invalid character position: " ++ show char
  | otherwise =
      let segments = Text.splitOn "\n" content
          lineCount = length segments
       in if line > lineCount
            then Left $ "Line out of bounds: " ++ show line ++ ", total lines: " ++ show lineCount
            else
              if line == lineCount
                then
                  if char == 0
                    then Right (Text.length content)
                    else Left $ "Character out of bounds at EOF line: " ++ show char
                else
                  let lineText = segments !! line
                      lineLength = Text.length lineText
                   in if char > lineLength
                        then
                          Left
                            ( "Character out of bounds: "
                                ++ show char
                                ++ ", line length: "
                                ++ show lineLength
                                ++ ", line: "
                                ++ show line
                            )
                        else
                          let charsBeforeLine = sum (map Text.length (take line segments)) + line
                           in Right (charsBeforeLine + char)

{- builder/src/File.* interface equivalents -}

exists :: FilePath -> IO Bool
exists path = do
  res <- lookup path
  case res of
    Just (t, x) -> do
      -- log $ "✅👀x " ++ show path
      pure True
    Nothing -> do
      -- log $ "❌👀x " ++ show path
      File.exists path

readUtf8 :: FilePath -> IO BS.ByteString
readUtf8 path = do
  res <- lookup path
  case res of
    Just (t, x) -> do
      pure x
    Nothing -> do
      log $ \() -> "❌👀r " ++ System.FilePath.takeFileName (show path)
      -- @watch can probably be dropped when we know we can rely on fsnotify setup
      t <- File.readUtf8 path
      insert path t
      pure t

-- | Read a UTF-8 file and return a String using the cache
readFileString :: FilePath -> IO String
readFileString path = do
  bytes <- readUtf8 path
  pure (Text.unpack (Text.decodeUtf8 bytes))

isVirtualPath :: FilePath -> Bool
isVirtualPath path =
  virtualDir `List.isInfixOf` path

writeUtf8 :: FilePath -> BS.ByteString -> IO ()
writeUtf8 path content = do
  log $ \() -> "✍️ " ++ System.FilePath.takeFileName (show path)
  insert path content
  Monad.when (allowWrites && not (isVirtualPath path)) $
    File.writeUtf8 path content


writeUtf8AllTheWayToDisk :: FilePath -> BS.ByteString -> IO ()
writeUtf8AllTheWayToDisk path content = do
  log $ \() -> "✍️ all the way to disk" ++ System.FilePath.takeFileName (show path)
  insert path content
  File.writeUtf8 path content

-- @TODO potentially skip binary serialisation entirely
writeBinary :: (Binary.Binary a) => FilePath -> a -> IO ()
writeBinary path value = do
  log $ \() -> "✍️  Binary " ++ System.FilePath.takeFileName (show path)
  insert path $ BSL.toStrict $ Binary.encode value
  Monad.when (allowWrites && not (isVirtualPath path)) $
    File.writeBinary path value

-- @TODO potentially skip binary serialisation entirely
readBinary :: (Binary.Binary a) => FilePath -> IO (Maybe a)
readBinary path = do
  res <- lookup path
  case res of
    Just (t, x) -> do
      log $ \() -> "✅👀 rb " ++ show path
      case Binary.decodeOrFail $ BSL.fromStrict x of
        Right (bs, offset, a) ->
          pure (Just a)
        Left (bs, offset, message) ->
          pure Nothing
    Nothing -> do
      exists <- File.exists path
      if exists
        then do
          t <- readUtf8 path
          insert path t
          case Binary.decodeOrFail $ BSL.fromStrict t of
            Right (bs, offset, a) ->
              pure (Just a)
            Left (bs, offset, message) ->
              pure Nothing
        else do
          log $ \() -> "❌👀rb doesnotexist " ++ show path
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

debug :: IO ()
debug = do
  fileCacheList <- H.toList fileCache
  putStrLn "File cache:"
  Monad.forM_ fileCacheList $ \(key, val) -> do
    putStrLn $
      "    " <> show key

zeroTime :: Time
zeroTime =
  File.Time 0

-- Not needed in cache?
writePackage :: FilePath -> Zip.Archive -> IO ()
writePackage path archive =
  Monad.when allowPackageWrites $
    File.writePackage path archive

writeBuilder :: FilePath -> B.Builder -> IO ()
writeBuilder path builder = do
  insert path $ BSL.toStrict $ B.toLazyByteString builder
  Monad.when (allowWrites && not (isVirtualPath path)) $
    File.writeBuilder path builder

remove :: FilePath -> IO ()
remove path = do
  delete path
  Monad.when allowWrites $
    File.remove path

removeDir :: FilePath -> IO ()
removeDir path = do
  delete path
  Monad.when allowWrites $
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

    let entries = Prelude.length fileCacheList

        -- @TODO memory overhead for string is 4 words per char + 2 words for the char... should swap to text/BS instead.
        size = fileCacheList & fmap (\(k, (t, v)) -> (length k * 6) + BS.length v) & sum & fromIntegral & bytesToMb & show
        -- (show $ bytesToMb (fromIntegral size))

        -- Sum of live bytes across all major GCs. Divided by major_gcs gives the average live data over the lifetime of the program.
        averageLiveData = bytesToMb $ RT.cumulative_live_bytes s `div` (fromIntegral $ RT.major_gcs s)

        gc = RT.gc s

        stats =
          [ -- (RT.gcdetails_gen, "") -- = 1
            -- , (RT.gcdetails_threads, "") -- = 1
            (RT.gcdetails_allocated_bytes, "allocated since last GC"), -- = 65812848
            (RT.gcdetails_live_bytes, "live heap after last GC"), -- = 320114080
            (RT.gcdetails_large_objects_bytes, "large objects"), -- = 76883456
            (RT.gcdetails_compact_bytes, "compact regions"), -- = 0
            (RT.gcdetails_slop_bytes, "slop (wasted memory)"), -- = 9568864
            (RT.gcdetails_mem_in_use_bytes, "mem use RTS"), -- = 884998144
            (RT.gcdetails_copied_bytes, "copied in GC") -- = 243233048
            -- , (RT.gcdetails_par_max_copied_bytes, "") -- = 0
            -- , (RT.gcdetails_par_balanced_copied_bytes, "par GC balance copy") -- = 0
            -- , (RT.gcdetails_sync_elapsed_ns, "") -- = 11208
            -- , (RT.gcdetails_cpu_ns, "") -- = 268024000
            -- , (RT.gcdetails_elapsed_ns, "") -- = 304688708
            -- , (RT.gcdetails_nonmoving_gc_sync_cpu_ns, "") -- = 0
            -- , (RT.gcdetails_nonmoving_gc_sync_elapsed_ns, "") -- = 0}}
          ]
            & fmap (\(fn, label) -> "    " ++ label ++ ": " ++ show (bytesToMb (fromIntegral $ fn gc)) ++ "MB")
            & List.intercalate "\n"

    log $ \() -> "🧠 filecache entries:" ++ show entries ++ " size:~" ++ size ++ "MB overhead:" ++ show overhead
    log $ \() -> "🧠 average live data:~" ++ show averageLiveData ++ "MB"
    -- log $ "🧠 gc:" ++ show (gcstatsHuman s)
    log $ \() -> "🧠 gc:\n" ++ stats

-- | Compute a lightweight summary of the virtual file cache.
-- Returns (number of entries, estimated bytes stored, hashtable overhead ratio).
-- The byte estimate mirrors the approach used in debugSummary: sum of
-- ByteString lengths plus an approximate cost for String keys.
fileCacheStats :: IO (Int, Binary.Word64, Double)
fileCacheStats = do
  fileCacheList <- H.toList fileCache
  overhead <- H.computeOverhead fileCache
  let entries = Prelude.length fileCacheList
      size :: Binary.Word64
      size =
        sum (fmap (\(k, (_t, v)) -> fromIntegral (length k * 6) + fromIntegral (BS.length v)) fileCacheList)
  pure (entries, size, overhead)

gcstatsHuman s =
  s
    { RT.gcs = RT.gcs s,
      RT.major_gcs = RT.major_gcs s,
      RT.allocated_bytes = RT.allocated_bytes s & bytesToMb,
      RT.max_live_bytes = RT.max_live_bytes s & bytesToMb,
      RT.max_large_objects_bytes = RT.max_large_objects_bytes s & bytesToMb,
      RT.max_compact_bytes = RT.max_compact_bytes s & bytesToMb,
      RT.max_slop_bytes = RT.max_slop_bytes s & bytesToMb,
      RT.max_mem_in_use_bytes = RT.max_mem_in_use_bytes s & bytesToMb,
      RT.cumulative_live_bytes = RT.cumulative_live_bytes s & bytesToMb,
      RT.copied_bytes = RT.copied_bytes s & bytesToMb,
      RT.par_copied_bytes = RT.par_copied_bytes s & bytesToMb,
      RT.cumulative_par_max_copied_bytes = RT.cumulative_par_max_copied_bytes s & bytesToMb,
      RT.cumulative_par_balanced_copied_bytes = RT.cumulative_par_balanced_copied_bytes s & bytesToMb,
      RT.init_cpu_ns = RT.init_cpu_ns s,
      RT.init_elapsed_ns = RT.init_elapsed_ns s,
      RT.mutator_cpu_ns = RT.mutator_cpu_ns s,
      RT.mutator_elapsed_ns = RT.mutator_elapsed_ns s,
      RT.gc_cpu_ns = RT.gc_cpu_ns s,
      RT.gc_elapsed_ns = RT.gc_elapsed_ns s,
      RT.cpu_ns = RT.cpu_ns s,
      RT.elapsed_ns = RT.elapsed_ns s,
      RT.nonmoving_gc_sync_cpu_ns = RT.nonmoving_gc_sync_cpu_ns s,
      RT.nonmoving_gc_sync_elapsed_ns = RT.nonmoving_gc_sync_elapsed_ns s,
      RT.nonmoving_gc_sync_max_elapsed_ns = RT.nonmoving_gc_sync_max_elapsed_ns s,
      RT.nonmoving_gc_cpu_ns = RT.nonmoving_gc_cpu_ns s,
      RT.nonmoving_gc_elapsed_ns = RT.nonmoving_gc_elapsed_ns s,
      RT.nonmoving_gc_max_elapsed_ns = RT.nonmoving_gc_max_elapsed_ns s,
      RT.gc = RT.gc s
    }

bytesToMb :: Binary.Word64 -> Binary.Word64
bytesToMb b =
  b `div` (1024 * 1024)
