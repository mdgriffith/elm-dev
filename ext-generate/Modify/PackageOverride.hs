module Modify.PackageOverride where

import qualified System.Environment as Env
import qualified Elm.Package as Pkg
import Data.Char (toLower)
import qualified Codec.Archive.Zip as Zip
import qualified Data.List as List

{-
Hot override mapping for specific packages. When hot mode is enabled, we
replace registry downloads for certain packages with forks that support
hot-reload semantics.

Enable by setting environment variable ELM_DEV_HOT to "1".

Branch zip vs release zip (why normalization is needed):
  - Registry downloads use release artifacts that are structured in a way our
    extractor expects (top-level prefix handling is consistent).
  - GitHub "zipball" of a branch adds a top-level directory like
    "repo-branch/" before all paths. Without trimming that common prefix, the
    package contents end up nested (e.g. repo-branch/src/..), so the build
    cannot find modules under the expected "src/" root. We normalize override
    archives by removing the common top-level directory if present.
-}



-- Returns a direct zip URL if this package should be overridden.
-- We intentionally keep versions resolved by the solver; the fork contents
-- are written into the same package cache slot for the resolved version.
overrideUrl :: Pkg.Name -> Maybe String
overrideUrl pkg =
  case Pkg.toChars pkg of
    "elm/core" -> Just "https://github.com/lydell/core/archive/refs/heads/hot-reload-stop.zip"
    "elm/browser" -> Just "https://github.com/lydell/browser/archive/refs/heads/hot-reload-stop.zip"
    "elm/virtual-dom" -> Just "https://github.com/lydell/virtual-dom/archive/refs/heads/hot-reload-stop.zip"
    "elm/html" -> Just "https://github.com/lydell/html/archive/refs/heads/safe.zip"
    _ -> Nothing



-- Normalize a GitHub branch zip (zipball) by stripping the common top-level
-- directory (e.g. "repo-branch/") so package paths start at the project root.
-- If no common top directory exists, the archive is returned unchanged.
normalizeArchive :: Zip.Archive -> Zip.Archive
normalizeArchive archive =
  let entries = Zip.zEntries archive in
  case entries of
    [] -> archive
    _  ->
      let paths = map Zip.eRelativePath entries
          topSeg p = takeWhile (/= '/') p
          -- Compute common top-level segment across all entries
          commonTop a b = if a == b then a else ""
          rootDir = case paths of
            [] -> ""
            p:ps -> List.foldl' commonTop (topSeg p) (map topSeg ps)
          prefixLen = if null rootDir then 0 else length rootDir + 1 -- include '/'
      in
      if prefixLen == 0 then archive else
        let stripPrefix n p = drop n p
            rewrite e = e { Zip.eRelativePath = stripPrefix prefixLen (Zip.eRelativePath e) }
        in archive { Zip.zEntries = map rewrite entries }
