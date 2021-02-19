{-# LANGUAGE OverloadedStrings #-}

module Ext.Sentry where

import Control.Concurrent.MVar

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B

import Control.Exception
import Formatting
import Formatting.Clock
import System.Clock


data Cache =
  Cache
    { jsOutput :: MVar BS.ByteString
    }


init :: IO Cache
init = do
  mJsOutput <- newMVar ""
  pure $ Cache mJsOutput


getJsOutput :: Cache -> IO BS.ByteString
getJsOutput cache =
  readMVar $ jsOutput cache


updateJsOutput :: Cache -> IO BS.ByteString -> IO ()
updateJsOutput (Cache mJsOutput) recompile = do
  track "recompile" $
    modifyMVar_ mJsOutput
      (\_ -> do
        bs <- recompile
        pure bs
      )
  pure ()


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
