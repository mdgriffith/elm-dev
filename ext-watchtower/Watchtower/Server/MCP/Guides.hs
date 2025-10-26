{-# LANGUAGE TemplateHaskell #-}

module Watchtower.Server.MCP.Guides
  ( architectureMd
  , wellFormedElmCodeMd
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


-- | Embed the well-formed Elm code guide markdown at compile time
wellFormedElmCodeRaw :: BS.ByteString
wellFormedElmCodeRaw = $( FileEmbed.bsToExp =<< TH.runIO (BS.readFile ("ext-watchtower" </> "Watchtower" </> "Server" </> "MCP" </> "guides" </> "well_formed_elm_code.md")) )


-- | Decoded markdown text for use in MCP responses
wellFormedElmCodeMd :: T.Text
wellFormedElmCodeMd = T.decodeUtf8 wellFormedElmCodeRaw


version :: String
version = "2"