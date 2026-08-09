module Ext.VirtualFile (dir, write) where

-- \|
--
-- Just to centralize details like what the prefix for the VFS is.

import qualified Data.ByteString as BS
import qualified Ext.FileCache
import qualified Ext.Log
import System.FilePath ((</>))

dir :: FilePath -> FilePath
dir root =
  root </> Ext.FileCache.virtualDir

write :: FilePath -> FilePath -> BS.ByteString -> IO ()
write root path value = do
  let virtualPath = root </> Ext.FileCache.virtualDir </> path
  Ext.Log.log Ext.Log.FileProxy ("Writing virtual file " ++ virtualPath)
  Ext.FileCache.insert virtualPath value
