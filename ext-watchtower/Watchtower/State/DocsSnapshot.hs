module Watchtower.State.DocsSnapshot
  ( publish
  , remove
  , exists
  , lookupFresh
  , entriesForProject
  , modulesForProject
  , lookupModuleForProject
  , lookupModule
  , listedModulesForProject
  , allModules
  ) where

import qualified Control.Concurrent.STM as STM
import qualified Data.Map as Map
import qualified Data.Map.Lazy as LazyMap
import qualified Elm.Docs as Docs
import qualified Elm.ModuleName as ModuleName
import qualified Ext.CompileHelpers.Generic as CompileHelpers
import qualified System.IO.Unsafe as Unsafe


data Snapshot = Snapshot
  { snapshotVersion :: !Int
  , snapshotFlags :: !CompileHelpers.Flags
  , snapshotEntries :: !(Map.Map FilePath (ModuleName.Raw, Docs.Module))
  , snapshotModulesByName :: !(Map.Map ModuleName.Raw (FilePath, Docs.Module))
  }

{-# NOINLINE snapshots #-}
snapshots :: STM.TVar (Map.Map FilePath Snapshot)
snapshots = Unsafe.unsafePerformIO (STM.newTVarIO Map.empty)

publish :: FilePath -> Int -> CompileHelpers.Flags -> Map.Map FilePath (ModuleName.Raw, Docs.Module) -> IO ()
publish root version flags entries =
  STM.atomically $
    STM.modifyTVar' snapshots (Map.insert root (Snapshot version flags entries modulesByName))
  where
    modulesByName =
      LazyMap.foldrWithKey
        (\path (name, docsModule) -> LazyMap.insert name (path, docsModule))
        LazyMap.empty
        entries

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
      then Just (LazyMap.map snd (snapshotEntries snapshot))
      else Nothing

entriesForProject :: FilePath -> IO (Map.Map FilePath (ModuleName.Raw, Docs.Module))
entriesForProject root = do
  stored <- STM.readTVarIO snapshots
  pure (maybe Map.empty snapshotEntries (Map.lookup root stored))

modulesForProject :: FilePath -> IO (Map.Map FilePath Docs.Module)
modulesForProject root = do
  stored <- STM.readTVarIO snapshots
  pure (maybe Map.empty (LazyMap.map snd . snapshotEntries) (Map.lookup root stored))

lookupModuleForProject :: FilePath -> ModuleName.Raw -> IO (Maybe (FilePath, Docs.Module))
lookupModuleForProject root name = do
  stored <- STM.readTVarIO snapshots
  pure $ do
    snapshot <- Map.lookup root stored
    Map.lookup name (snapshotModulesByName snapshot)

lookupModule :: ModuleName.Raw -> IO (Maybe (FilePath, Docs.Module))
lookupModule name = do
  stored <- STM.readTVarIO snapshots
  pure (firstMatch name (Map.elems stored))

listedModulesForProject :: FilePath -> IO [(FilePath, Docs.Module)]
listedModulesForProject root = do
  stored <- STM.readTVarIO snapshots
  pure (maybe [] Map.elems (snapshotModulesByName <$> Map.lookup root stored))

allModules :: IO [(FilePath, Docs.Module)]
allModules = do
  stored <- STM.readTVarIO snapshots
  pure (concatMap (Map.elems . snapshotModulesByName) (Map.elems stored))

firstMatch :: ModuleName.Raw -> [Snapshot] -> Maybe (FilePath, Docs.Module)
firstMatch name stored =
  case stored of
    [] -> Nothing
    snapshot : rest ->
      case Map.lookup name (snapshotModulesByName snapshot) of
        Just found -> Just found
        Nothing -> firstMatch name rest
