{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Gen.RunConfig where

import Control.Monad (forM)
import qualified Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isAlpha, toUpper, toLower)
import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEnc
import qualified Data.Text.IO as TextIO
import GHC.Generics
import qualified Gen.Config as Config
import qualified System.Directory as Dir
import qualified System.FilePath as FilePath

data RunConfig = RunConfig
  { app :: Maybe App,
    appView :: [String],
    docs :: Maybe Docs,
    assets :: Maybe [AssetGroup],
    theme :: Maybe Value
  }
  deriving (Generic)



instance ToJSON RunConfig where
  toJSON (RunConfig app_ appView_ docs_ assets_ theme_) =
    object
      [ "app" .= app_,
        "app-view" .= appView_,
        "docs" .= docs_,
        "assets" .= assets_,
        "theme" .= theme_
      ]

data App = App
  { pages :: [Page],
    stores :: [Store]
  }
  deriving (Generic)

instance FromJSON App

instance ToJSON App

data Page = Page
  { _id :: String,
    _moduleName :: [String],
    urlOnly :: Bool,
    route :: Maybe Route
  }
  deriving (Generic)

instance FromJSON Page

instance ToJSON Page where
  toJSON (Page id_ moduleName_ urlOnly_ route_) =
    object
      [ "id" .= id_,
        "moduleName" .= moduleName_,
        "urlOnly" .= urlOnly_,
        "route" .= route_
      ]

data Store = Store
  { storeId :: String
  }
  deriving (Generic)

instance FromJSON Store where
  parseJSON = withObject "Store" $ \v ->
    Store
      <$> v .: "id"

instance ToJSON Store where
  toJSON Store {..} =
    object
      [ "id" .= storeId
      ]

data Route = Route
  { routeId :: String,
    url :: String,
    redirectFrom :: [String]
  }
  deriving (Generic)

instance FromJSON Route

instance ToJSON Route where
  toJSON (Route id_ url_ redirectFrom_) =
    object
      [ "id" .= id_,
        "url" .= url_,
        "redirectFrom" .= redirectFrom_
      ]

-- DOCS

data Docs = Docs
  { readme :: Maybe String,
    guides :: [Guide],
    project :: Data.Aeson.Value,
    modules :: [Data.Aeson.Value],
    deps :: Map.Map String [Data.Aeson.Value]
  }
  deriving (Generic)

instance FromJSON Docs

instance ToJSON Docs

data Guide = Guide
  { guideName :: String,
    guideContent :: Maybe String
  }
  deriving (Generic)

instance FromJSON Guide

instance ToJSON Guide

-- | Asset types expected by Elm decoders in Options.Assets
data AssetGroup = AssetGroup
  { groupName :: String,
    files :: [AssetFile],
    fileInfo :: FileInfo
  }
  deriving (Generic)

instance ToJSON AssetGroup where
  toJSON AssetGroup {..} =
    object
      [ "name" .= groupName
      , "files" .= files
      , "fileInfo" .= fileInfo
      ]

data FileInfo = FileInfo
  { markdown :: MarkdownInfo
  }
  deriving (Generic)

instance ToJSON FileInfo

data MarkdownInfo = MarkdownInfo
  { frontmatter :: Map.Map String String
  }
  deriving (Generic)

instance ToJSON MarkdownInfo

data AssetFile = AssetFile
  { fileName :: String,
    crumbs :: [String],
    pathOnServer :: String,
    content :: Maybe String
  }
  deriving (Generic)

instance ToJSON AssetFile where
  toJSON AssetFile {..} =
    object
      [ "name" .= fileName
      , "crumbs" .= crumbs
      , "pathOnServer" .= pathOnServer
      , "content" .= content
      ]

-- | Format asset source path for name field
-- 1. Eliminate all leading punctuation and numbers
-- 2. Split the path into its pieces
-- 3. Capitalize every piece
-- 4. Join into a string
formatAssetName :: Text.Text -> String
formatAssetName assetSrc =
  let pathStr = Text.unpack assetSrc
      -- Split path into pieces and filter out empty strings
      pathPieces = filter (not . null) $ FilePath.splitPath pathStr
      -- For each piece, remove leading punctuation/numbers and capitalize
      formattedPieces = map formatPathPiece pathPieces
   in concat formattedPieces
  where
    formatPathPiece :: String -> String
    formatPathPiece piece =
      let -- Remove file extension if present
          withoutExt = FilePath.dropExtension piece
          -- Remove leading punctuation and numbers
          cleaned = dropWhile (not . isAlpha) withoutExt
          -- Capitalize first letter
          capitalized = case cleaned of
            [] -> []
            (c : cs) -> toUpper c : cs
       in capitalized

toRunConfig :: FilePath -> Config.Config -> IO RunConfig
toRunConfig cwd config = do
  -- Convert stores by reading store files
  storeIds <- findStores (cwd FilePath.</> "src" FilePath.</> "app" FilePath.</> "Store")

  let stores = map (\sid -> Store {storeId = sid}) storeIds

  -- Convert pages from top-level Config.configPages
  let pages = case Config.configPages config of
        Nothing -> []
        Just pageMap ->
          Map.foldrWithKey
            ( \k v acc ->
                let moduleName = "Page" : map Text.unpack (Text.splitOn "." k)
                    route =
                      if Config.pageUrlOnly v
                        then Nothing
                        else
                          Just $
                            Route
                              { routeId = Text.unpack k,
                                url = Text.unpack (Config.pageUrl v),
                                redirectFrom = map Text.unpack (Config.pageRedirectFrom v)
                              }
                 in Page
                      { _id = Text.unpack k,
                        _moduleName = moduleName,
                        urlOnly = Config.pageUrlOnly v,
                        route = route
                      }
                      : acc
            )
            []
            pageMap

  -- Convert docs
  docs <- case Config.configDocs config of
    Nothing -> return Nothing
    Just docsConfig -> do
      -- Read guides
      guides <- case Config.docsGuides docsConfig of
        Nothing -> return []
        Just guidePaths -> forM (map Text.unpack guidePaths) $ \guidePath -> do
          content <- tryReadFile (cwd FilePath.</> guidePath FilePath.<.> "md")
          return
            Guide
              { guideName = FilePath.dropExtension guidePath,
                guideContent = content
              }

      return $
        Just
          Docs
            { readme = Nothing,
              guides = guides,
              project = Data.Aeson.Null, -- Placeholder, would need project documentation
              modules = [], -- Placeholder, would need module documentation
              deps = Map.empty -- Placeholder, would need dependency documentation
            }

  -- Convert assets
  assets <- case Config.configAssets config of
    Nothing -> return Nothing
    Just assetMap -> do
      let assetList = Map.toList assetMap -- [(src,onServer)]
      groups <- forM assetList $ \(src,onServer) -> do
        let srcPath = Text.unpack src
            serverPath = Text.unpack onServer
            groupName = formatAssetName onServer
            srcRoot = cwd FilePath.</> srcPath
        files <- collectAssetFiles srcRoot srcRoot serverPath
        let fm = MarkdownInfo { frontmatter = Map.empty }
        return AssetGroup { groupName = groupName, files = files, fileInfo = FileInfo { markdown = fm } }

      return (Just groups)

  return
    RunConfig
      { app =
          if null pages && null stores
            then Nothing
            else Just App {pages = pages, stores = stores},
        appView = ["primary"],
        docs = Nothing, -- docs
        assets = assets,
        theme = Config.configTheme config
      }

-- | Helper function to safely read a file, returning Nothing if the file doesn't exist
tryReadFile :: FilePath -> IO (Maybe String)
tryReadFile path = do
  exists <- Dir.doesFileExist path
  if exists
    then Just <$> readFile path
    else return Nothing

-- | Find all store files in the given directory and return their names without extension
findStores :: FilePath -> IO [String]
findStores dir = do
  exists <- Dir.doesDirectoryExist dir
  if not exists
    then return []
    else do
      files <- Dir.listDirectory dir
      return $
        map FilePath.dropExtension $
          filter (\f -> FilePath.takeExtension f == ".elm") files

-- | Convert a RunConfig to a SHA hash string
toHash :: RunConfig -> Text.Text
toHash config =
  let jsonBytes = Data.Aeson.encode (toJSON config)
      digest = SHA.sha1 jsonBytes
   in Text.pack (SHA.showDigest digest)

-- | Recursively collect files under a source root, producing AssetFile entries
collectAssetFiles :: FilePath -> FilePath -> String -> IO [AssetFile]
collectAssetFiles root current serverBase = do
  isDir <- Dir.doesDirectoryExist current
  if not isDir
    then do
      isFile <- Dir.doesFileExist current
      if not isFile
        then return []
        else do
          rel <- pure (FilePath.makeRelative root current)
          let dirPart = FilePath.takeDirectory rel
              crumbs =
                if dirPart == "." || dirPart == ""
                  then []
                  else filter (not . null) $ map FilePath.dropTrailingPathSeparator (FilePath.splitPath dirPart)
              fileName = FilePath.takeBaseName rel
              pathOnServer = serverBase FilePath.</> rel
              ext = map toLower (FilePath.takeExtension rel)
          content <- if ext == ".md" || ext == ".markdown"
                      then tryReadFile current
                      else return Nothing
          return [AssetFile { fileName = fileName, crumbs = crumbs, pathOnServer = pathOnServer, content = content }]
    else do
      entries <- Dir.listDirectory current
      fmap concat $ forM entries $ \e -> do
        let next = current FilePath.</> e
        collectAssetFiles root next serverBase
