{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module ElmDevVersion (version) where

import qualified Data.Aeson
import           Data.Aeson ((.:), withObject)
import qualified Data.Aeson.Types
import qualified Data.ByteString as BS
import qualified Data.Text
import qualified Language.Haskell.TH as TH
import System.FilePath ((</>))


{-|
This is the version of Elm Dev itself.

The version is read from installers-elm-dev/npm/package.json at compile time
to ensure a single source of truth.
-}
version :: String
version =
  $( do
       packageJsonPath <- TH.runIO (pure ("installers-elm-dev" </> "npm" </> "package.json"))
       contents <- TH.runIO (BS.readFile packageJsonPath)
       case Data.Aeson.eitherDecodeStrict' contents of
         Left err -> fail ("Failed to parse package.json: " ++ err)
         Right val ->
           case Data.Aeson.Types.parseMaybe (withObject "package.json" (\o -> o .: "version")) val of
             Just (Data.Aeson.String v) -> TH.litE (TH.StringL (Data.Text.unpack v))
             _ -> fail "Failed to extract version field from package.json"
   )

