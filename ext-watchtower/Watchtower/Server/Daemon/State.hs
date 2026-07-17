{-# LANGUAGE OverloadedStrings #-}

module Watchtower.Server.Daemon.State
  ( StateInfo(..)
  , fingerprintMatches
  ) where

import qualified Data.Aeson as JSON


data StateInfo = StateInfo
  { pid :: !Int
  , domain :: !String
  , lspPort :: !Int
  , mcpPort :: !Int
  , httpPort :: !Int
  , startedAt :: !String
  , version :: !String
  , executableFingerprint :: !(Maybe String)
  } deriving (Show, Eq)


instance JSON.ToJSON StateInfo where
  toJSON state = JSON.object
    [ ("pid", JSON.toJSON (pid state))
    , ("domain", JSON.toJSON (domain state))
    , ("lspPort", JSON.toJSON (lspPort state))
    , ("mcpPort", JSON.toJSON (mcpPort state))
    , ("httpPort", JSON.toJSON (httpPort state))
    , ("startedAt", JSON.toJSON (startedAt state))
    , ("version", JSON.toJSON (version state))
    , ("executableFingerprint", JSON.toJSON (executableFingerprint state))
    ]


instance JSON.FromJSON StateInfo where
  parseJSON = JSON.withObject "StateInfo" $ \object -> do
    -- domain was introduced later; default to 127.0.0.1 for older state files
    domainValue <- object JSON..:? "domain" JSON..!= "127.0.0.1"
    StateInfo <$> object JSON..: "pid"
              <*> pure domainValue
              <*> object JSON..: "lspPort"
              <*> object JSON..: "mcpPort"
              <*> object JSON..: "httpPort"
              <*> object JSON..: "startedAt"
              <*> object JSON..: "version"
              <*> object JSON..:? "executableFingerprint" JSON..!= Nothing


fingerprintMatches :: String -> StateInfo -> Bool
fingerprintMatches current state = executableFingerprint state == Just current
