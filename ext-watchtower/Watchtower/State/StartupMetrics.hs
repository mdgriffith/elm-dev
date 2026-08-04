module Watchtower.State.StartupMetrics
  ( add
  , increment
  , setGauge
  , snapshot
  ) where

import qualified Control.Concurrent.STM as STM
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Key as Key
import qualified Data.Map.Strict as Map
import qualified System.IO.Unsafe as Unsafe

{-# NOINLINE counters #-}
counters :: STM.TVar (Map.Map String Int)
counters = Unsafe.unsafePerformIO (STM.newTVarIO Map.empty)

add :: String -> Int -> IO ()
add name amount = STM.atomically (STM.modifyTVar' counters (Map.insertWith (+) name amount))

increment :: String -> IO ()
increment name = add name 1

setGauge :: String -> Int -> IO ()
setGauge name value = STM.atomically (STM.modifyTVar' counters (Map.insert name value))

snapshot :: IO JSON.Value
snapshot = do
  values <- STM.readTVarIO counters
  pure (JSON.object [Key.fromString name JSON..= value | (name, value) <- Map.toList values])
