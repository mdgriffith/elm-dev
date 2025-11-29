{-# LANGUAGE ScopedTypeVariables #-}
module Watchtower.State.Versions
  ( Versions(..)
  , getOrInit
  , readVersions
  , bumpFsVersion
  , setCompileVersionTo
  ) where

import qualified Control.Concurrent.STM as STM
import qualified Data.Map.Strict as Map
import qualified System.IO.Unsafe as Unsafe

data Versions =
  Versions
    { fsVersion :: !Int
    , compileVersion :: !Int
    }
  deriving (Show, Eq)

{-# NOINLINE versionsTable #-}
versionsTable :: STM.TVar (Map.Map FilePath (STM.TVar Versions))
versionsTable = Unsafe.unsafePerformIO (STM.newTVarIO Map.empty)

getOrInit :: FilePath -> IO (STM.TVar Versions)
getOrInit projectRoot = do
  table <- STM.readTVarIO versionsTable
  case Map.lookup projectRoot table of
    Just tv -> pure tv
    Nothing -> do
      tv <- STM.newTVarIO (Versions 0 0)
      STM.atomically $ do
        cur <- STM.readTVar versionsTable
        case Map.lookup projectRoot cur of
          Just existing -> pure ()
          Nothing -> STM.writeTVar versionsTable (Map.insert projectRoot tv cur)
      pure tv

readVersions :: FilePath -> IO Versions
readVersions projectRoot = do
  tv <- getOrInit projectRoot
  STM.readTVarIO tv

bumpFsVersion :: FilePath -> IO Int
bumpFsVersion projectRoot = do
  tv <- getOrInit projectRoot
  STM.atomically $ do
    v <- STM.readTVar tv
    let n = fsVersion v + 1
    STM.writeTVar tv (v { fsVersion = n })
    pure n

setCompileVersionTo :: FilePath -> Int -> IO ()
setCompileVersionTo projectRoot n = do
  tv <- getOrInit projectRoot
  STM.atomically $ do
    v <- STM.readTVar tv
    STM.writeTVar tv (v { compileVersion = n })


