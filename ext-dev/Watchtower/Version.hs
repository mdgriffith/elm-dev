{-# LANGUAGE TemplateHaskell #-}

module Watchtower.Version where

import GitHash

short :: String
short = "0.0.1"

full :: String
full =
  let
    gi = $$tGitInfoCwd
    dirty | giDirty gi = "-dirty"
          | otherwise  = ""
  in
  concat
    [ "watchtower-", short, "-", giHash gi, dirty
    , " (", giCommitDate gi, ")"
    , " (branch:", giBranch gi, ")"
    ]
