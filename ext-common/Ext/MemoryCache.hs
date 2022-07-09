{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ext.MemoryCache where

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


-- type HashTable k v = H.CuckooHashTable k v

-- -- https://stackoverflow.com/questions/16811376/simulate-global-variable trick
-- {-# NOINLINE memoryCache #-}
-- memoryCache :: HashTable FilePath (Time, BS.ByteString)
-- memoryCache = unsafePerformIO H.new


-- lookup path = do
--   -- log $ "👀 " ++ show path
--   H.lookup fileCache path

-- insert path value = do
--   -- log $ "✍️ " ++ show path
--   t <- currentTime
--   H.insert fileCache path (t, value)
