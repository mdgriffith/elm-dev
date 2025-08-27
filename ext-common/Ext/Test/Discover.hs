module Ext.Test.Discover
  ( discoverTestFiles
  ) where

import qualified System.Directory as Dir
import qualified System.FilePath as FP
import qualified Control.Monad as Monad


-- | Recursively collect all .elm files under root/tests, excluding elm-stuff and node_modules
discoverTestFiles :: FilePath -> IO [FilePath]
discoverTestFiles root = do
  let testDir = root `FP.combine` "tests"
  exists <- Dir.doesDirectoryExist testDir
  if not exists then pure [] else listElmFiles testDir


listElmFiles :: FilePath -> IO [FilePath]
listElmFiles dir = do
  names <- Dir.listDirectory dir
  let paths = map (dir `FP.combine`) names
  files <- Monad.filterM Dir.doesFileExist paths
  dirs  <- Monad.filterM Dir.doesDirectoryExist paths
  let elmFiles = filter (\p -> FP.takeExtension p == ".elm") files
  nested <- fmap concat $ mapM listElmFiles (filter allowedDir dirs)
  pure (elmFiles ++ nested)


allowedDir :: FilePath -> Bool
allowedDir p =
  let base = FP.takeFileName p
  in base /= "elm-stuff" && base /= "node_modules"



