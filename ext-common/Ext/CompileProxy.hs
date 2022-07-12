{-# LANGUAGE ScopedTypeVariables #-}

module Ext.CompileProxy where

{- This is a proxy for all compilation related functions
   that ensures we can transparently swap compilation providers/methods
   (i.e. Disk vs MemoryCached)
 -}

-- import qualified File
-- import qualified Ext.FileCache as FileCache

import Control.Concurrent.MVar
import System.IO.Unsafe (unsafePerformIO)
import qualified System.Environment as Env

import qualified Codec.Archive.Zip as Zip
import qualified Data.Binary as Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.NonEmptyList as NE
import qualified Data.Map as Map

import qualified Json.Encode as Encode
import Ext.Common ((&))
import qualified Ext.Common
import qualified Watchtower.Compile.Classic
import qualified Watchtower.Compile.MemoryCached



import Ext.CompileMode (getMode, CompileMode(..))


type AggregateStatistics = Map.Map CompileMode Double


{-# NOINLINE aggregateStatistics #-}
aggregateStatistics :: MVar AggregateStatistics
aggregateStatistics = unsafePerformIO $
  let blank :: AggregateStatistics = Map.fromList [ (Disk, 0) , (Memory, 0) ]
  in
  newMVar blank


{-# NOINLINE addToAggregate #-}
addToAggregate mode t = do
  Ext.Common.debug $ "📈 " <> show mode <> " compile +" <> show t
  modifyMVar_ aggregateStatistics (\agg -> pure $ Map.update (\existing -> Just $ existing + t) mode agg )


aggregateSummary :: IO String
aggregateSummary = do
  x <- readMVar aggregateStatistics
  pure $ show x


-- Interfaces


compileToJson :: FilePath -> NE.List FilePath -> IO (Either Encode.Value Encode.Value)
compileToJson root paths = do
  res <- compileToJson_ root paths
  summary <- aggregateSummary
  Ext.Common.debug $ "📊 " <> summary
  pure res


compileToJson_ :: FilePath -> NE.List FilePath -> IO (Either Encode.Value Encode.Value)
compileToJson_ root paths = do
  Ext.Common.debug $ "👁 compileProxy:compileToJson:" <> show getMode <> " " <> root <> " " <> show paths
  case getMode of
    Disk -> do
      (t, result) <- Ext.Common.track__ "🎻 classic  " $ Watchtower.Compile.Classic.compileToJson root paths
      addToAggregate Disk t
      pure result

    Memory -> do
      (t, result) <- Ext.Common.track__ "🧠 memcached" $ Watchtower.Compile.MemoryCached.compileToJson root paths
      addToAggregate Memory t
      pure result

    Race -> do
      results <- Ext.Common.race
        [ ("🎻 classic  ", Watchtower.Compile.Classic.compileToJson root paths)
        , ("🧠 memcached", Watchtower.Compile.MemoryCached.compileToJson root paths)
        ]

      results & zip [Memory, Disk] & mapM_ (\(m, (t, r)) -> addToAggregate m t)

      (results !! 1) & snd & pure
