module Ext.CompileMode where

{- This is a proxy for all compilation related functions
   that ensures we can transparently swap compilation providers/methods
   (i.e. Disk vs MemoryCached)
 -}

import Control.Concurrent.MVar
import System.IO.Unsafe (unsafePerformIO)
import qualified System.Environment as Env

import qualified Codec.Archive.Zip as Zip
import qualified Data.Binary as Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B

import Ext.Log


data CompileMode = Disk | Memory | Race deriving (Show, Ord, Eq)


{-# NOINLINE compileMode #-}
compileMode :: MVar CompileMode
compileMode = unsafePerformIO $ do
  debugM <- Env.lookupEnv "MODE"
  let mode =
        case debugM of
          Just "disk" -> Disk
          Just "memory" -> Memory
          Just "race" -> Race
          _ -> Disk
  newMVar mode


withModeDisk :: IO a -> IO a
withModeDisk io = do
  modifyMVar_ compileMode (\_ -> pure Disk)
  res <- io
  modifyMVar_ compileMode (\_ -> pure Memory)
  pure res


setModeRace :: IO ()
setModeRace = do
  Ext.Log.log Ext.Log.Performance "🏎 race mode set"
  modifyMVar_ compileMode (\_ -> pure Race)


setModeMemory :: IO ()
setModeMemory = do
  Ext.Log.log Ext.Log.Performance "🧠 memory mode set"
  modifyMVar_ compileMode (\_ -> pure Memory)


setModeDisk :: IO ()
setModeDisk = do
  Ext.Log.log Ext.Log.Performance "💾 disk mode set"
  modifyMVar_ compileMode (\_ -> pure Disk)


{-# NOINLINE getMode #-}
getMode :: CompileMode
getMode = unsafePerformIO $ readMVar compileMode
