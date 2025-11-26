{-# LANGUAGE TemplateHaskell #-}

module Watchtower.Server.MCP.Guides
  ( architectureElmDevAppMd
  , architectureElmPackageMd
  , architectureElmAppMd
  , wellFormedElmCodeMd
  ) where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Language.Haskell.TH as TH
import qualified Data.FileEmbed as FileEmbed
import System.FilePath ((</>))





-- | Embed variant guides
architectureElmDevAppRaw :: BS.ByteString
architectureElmDevAppRaw = $( FileEmbed.bsToExp =<< TH.runIO (BS.readFile ("ext-watchtower" </> "Watchtower" </> "Server" </> "MCP" </> "guides" </> "architecture-elm-dev-app.md")) )

architectureElmPackageRaw :: BS.ByteString
architectureElmPackageRaw = $( FileEmbed.bsToExp =<< TH.runIO (BS.readFile ("ext-watchtower" </> "Watchtower" </> "Server" </> "MCP" </> "guides" </> "architecture-elm-package.md")) )

architectureElmAppRaw :: BS.ByteString
architectureElmAppRaw = $( FileEmbed.bsToExp =<< TH.runIO (BS.readFile ("ext-watchtower" </> "Watchtower" </> "Server" </> "MCP" </> "guides" </> "architecture-elm-app.md")) )

-- | Decoded markdown text for use in MCP responses
architectureElmDevAppMd :: T.Text
architectureElmDevAppMd = T.decodeUtf8 architectureElmDevAppRaw

architectureElmPackageMd :: T.Text
architectureElmPackageMd = T.decodeUtf8 architectureElmPackageRaw

architectureElmAppMd :: T.Text
architectureElmAppMd = T.decodeUtf8 architectureElmAppRaw


-- | Embed the well-formed Elm code guide markdown at compile time
wellFormedElmCodeRaw :: BS.ByteString
wellFormedElmCodeRaw = $( FileEmbed.bsToExp =<< TH.runIO (BS.readFile ("ext-watchtower" </> "Watchtower" </> "Server" </> "MCP" </> "guides" </> "well_formed_elm_code.md")) )


-- | Decoded markdown text for use in MCP responses
wellFormedElmCodeMd :: T.Text
wellFormedElmCodeMd = T.decodeUtf8 wellFormedElmCodeRaw


version :: String
version = "2"