{-# LANGUAGE OverloadedStrings #-}

module Watchtower.Trace
  ( newTraceId
  , fingerprintBytes
  , formatPaths
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.List as List
import qualified Data.Unique as Unique
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified System.FilePath as FilePath

newTraceId :: String -> IO String
newTraceId prefix = do
  now <- getPOSIXTime
  u <- Unique.newUnique
  let seed = prefix ++ ":" ++ show (round (now * 1000000) :: Integer) ++ ":" ++ show (Unique.hashUnique u)
      digest = take 8 (SHA.showDigest (SHA.sha1 (LBS8.pack seed)))
  pure (prefix ++ "#" ++ digest)

fingerprintBytes :: BS.ByteString -> String
fingerprintBytes bytes =
  let digest = take 10 (SHA.showDigest (SHA.sha1 (LBS.fromStrict bytes)))
   in digest ++ "/" ++ show (BS.length bytes) ++ "b"

formatPaths :: [FilePath] -> String
formatPaths paths =
  case paths of
    [] -> "[]"
    _ -> "[" ++ List.intercalate ", " (map FilePath.takeFileName paths) ++ "]"
