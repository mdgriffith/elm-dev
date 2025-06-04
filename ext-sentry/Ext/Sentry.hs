{-# LANGUAGE OverloadedStrings #-}

module Ext.Sentry where

import Control.Concurrent.MVar

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B

import Control.Exception
import Formatting
import Formatting.Clock
import System.Clock
import Json.Encode ((==>))
import qualified Json.Encode as Encode


data Cache =
  Cache
    { jsOutput :: MVar BS.ByteString
    , compileResult :: MVar (Either Encode.Value Encode.Value)
    }


instance Show Cache where
  show _ = "<Cache>"


init :: IO Cache
init = do
  mJsOutput <- newMVar ""
  -- @TODO watchtower specific? or invert so its not.
  mCompileresult <- newMVar $ Right $ Encode.object [ "compiled" ==> (Encode.bool True) ]
  pure $ Cache mJsOutput mCompileresult


updateCompileResult :: Cache -> IO (Either Encode.Value Encode.Value) -> IO ()
updateCompileResult (Cache _ compileResult) action = do
  modifyMVar_ compileResult (\_ -> action )


getCompileResult :: Cache -> IO (Either Encode.Value Encode.Value)
getCompileResult cache =
  readMVar $ compileResult cache


getJsOutput :: Cache -> IO BS.ByteString
getJsOutput cache =
  readMVar $ jsOutput cache


updateJsOutput :: Cache -> IO BS.ByteString -> IO ()
updateJsOutput (Cache mJsOutput _) recompile = do
  modifyMVar_ mJsOutput (\_ -> recompile )
