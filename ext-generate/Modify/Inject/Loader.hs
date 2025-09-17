{-# LANGUAGE TemplateHaskell #-}
module Modify.Inject.Loader
  ( hotJs
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Language.Haskell.TH as TH
import qualified Data.FileEmbed as FileEmbed
import System.FilePath ((</>))


-- | Embed the raw hot client JS at compile time
hotJsTemplate :: BS.ByteString
hotJsTemplate = $( FileEmbed.bsToExp =<< TH.runIO (BS.readFile ("ext-generate" </> "Modify" </> "Inject" </> "hot.js")) )


-- | Produce the hot client JS with the websocket URL injected.
-- Replaces all occurrences of the placeholder {{ ELM_DEV_SERVER_WS_URL }}
-- with the provided URL and returns it as a Builder for easy appending.
hotJs :: String -> B.Builder
hotJs wsUrl =
  let replaced =
        T.replace (T.pack "{{ ELM_DEV_SERVER_WS_URL }}") (T.pack wsUrl)
          (T.decodeUtf8 hotJsTemplate)
  in B.byteString (T.encodeUtf8 replaced)


version :: String
version = "0.1.5"