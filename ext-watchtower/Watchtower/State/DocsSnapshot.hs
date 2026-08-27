module Watchtower.State.DocsSnapshot
  ( publish
  , remove
  , exists
  , lookupFresh
  , modulesForProject
  , allModules
  ) where

import qualified Control.Concurrent.STM as STM
import qualified Data.Map.Strict as Map
import qualified Elm.Docs as Docs
import qualified Ext.CompileHelpers.Generic as CompileHelpers
import qualified System.IO.Unsafe as Unsafe


data Snapshot = Snapshot
  { snapshotVersion :: !Int
  , snapshotFlags :: !CompileHelpers.Flags
  , snapshotModules :: !(Map.Map FilePath Docs.Module)
  }

{-# NOINLINE snapshots #-}
snapshots :: STM.TVar (Map.Map FilePath Snapshot)
snapshots = Unsafe.unsafePerformIO (STM.newTVarIO Map.empty)

publish :: FilePath -> Int -> CompileHelpers.Flags -> Map.Map FilePath Docs.Module -> IO ()
publish root version flags modules =
  STM.atomically $
    STM.modifyTVar' snapshots (Map.insert root (Snapshot version flags modules))

remove :: FilePath -> IO ()
remove root =
  STM.atomically $
    STM.modifyTVar' snapshots (Map.delete root)

exists :: FilePath -> IO Bool
exists root =
  Map.member root <$> STM.readTVarIO snapshots

lookupFresh :: FilePath -> Int -> CompileHelpers.Flags -> IO (Maybe (Map.Map FilePath Docs.Module))
lookupFresh root version flags = do
  stored <- STM.readTVarIO snapshots
  pure $ do
    snapshot <- Map.lookup root stored
    if snapshotVersion snapshot == version && snapshotFlags snapshot == flags
      then Just (snapshotModules snapshot)
      else Nothing

modulesForProject :: FilePath -> IO (Map.Map FilePath Docs.Module)
modulesForProject root = do
  stored <- STM.readTVarIO snapshots
  pure (maybe Map.empty snapshotModules (Map.lookup root stored))

allModules :: IO [(FilePath, Docs.Module)]
allModules = do
  stored <- STM.readTVarIO snapshots
  pure (concatMap Map.toList (map snapshotModules (Map.elems stored)))
