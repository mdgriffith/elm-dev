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
    -- @TODO in future we could key this by project, so one watchtower for all projects?
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


track label io = do
  m <- getTime Monotonic
  p <- getTime ProcessCPUTime
  t <- getTime ThreadCPUTime
  res <- io
  m_ <- getTime Monotonic
  p_ <- getTime ProcessCPUTime
  t_ <- getTime ThreadCPUTime
  fprint ("⏱  " % label % ": " % timeSpecs % " " % timeSpecs % " " % timeSpecs % "\n") m m_ p p_ t t_
  pure res
