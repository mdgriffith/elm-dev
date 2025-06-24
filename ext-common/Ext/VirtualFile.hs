module Ext.VirtualFile (dir, write) where

-- \|
--
-- Just to centralize details like what the prefix for the VFS is.

import qualified Data.ByteString as BS
import qualified Ext.FileCache
import System.FilePath ((</>))

dir :: FilePath -> FilePath
dir root =
  root </> Ext.FileCache.virtualDir

write :: FilePath -> FilePath -> BS.ByteString -> IO ()
write root path value = do
  let virtualPath = root </> Ext.FileCache.virtualDir </> path
  -- putStrLn $ "VFS components " ++ show (Ext.FileCache.virtualFileSystemPrefix, path)
  putStrLn $ "WRITING VIRTUAL FILE " ++ virtualPath
  Ext.FileCache.insert virtualPath value
