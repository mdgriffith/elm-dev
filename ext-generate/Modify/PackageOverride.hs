module Modify.PackageOverride where

import qualified System.Environment as Env
import qualified Elm.Package as Pkg
import Data.Char (toLower)

{-
Hot override mapping for specific packages. When hot mode is enabled, we
replace registry downloads for certain packages with forks that support
hot-reload semantics.

Enable by setting environment variable ELM_DEV_HOT to "1".
-}

hotEnabled :: IO Bool
hotEnabled = do
  env <- Env.lookupEnv "ELM_DEV_HOT"
  case env of
    Nothing -> pure True
    Just val ->
      case fmap toLower val of
        "0" -> pure False
        "false" -> pure False
        _ -> pure True

-- Returns a direct zip URL if this package should be overridden.
-- We intentionally keep versions resolved by the solver; the fork contents
-- are written into the same package cache slot for the resolved version.
overrideUrl :: Pkg.Name -> Maybe String
overrideUrl pkg =
  case Pkg.toChars pkg of
    "elm/core" -> Just "https://github.com/lydell/core/archive/refs/heads/hot-reload-stop.zip"
    "elm/browser" -> Just "https://github.com/lydell/browser/archive/refs/heads/hot-reload-stop.zip"
    "elm/virtual-dom" -> Just "https://github.com/lydell/virtual-dom/archive/refs/heads/hot-reload-stop.zip"
    _ -> Nothing


