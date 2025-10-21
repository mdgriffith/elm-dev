{-# LANGUAGE TemplateHaskell #-}

module Watchtower.Server.MCP.Guides
  ( architectureMd
  ) where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Language.Haskell.TH as TH
import qualified Data.FileEmbed as FileEmbed
import System.FilePath ((</>))


-- | Embed the architecture guide markdown at compile time
architectureRaw :: BS.ByteString
architectureRaw = $( FileEmbed.bsToExp =<< TH.runIO (BS.readFile ("ext-watchtower" </> "Watchtower" </> "Server" </> "MCP" </> "guides" </> "architecture.md")) )


-- | Decoded markdown text for use in MCP responses
architectureMd :: T.Text
architectureMd = T.decodeUtf8 architectureRaw


version :: String
version = "2"