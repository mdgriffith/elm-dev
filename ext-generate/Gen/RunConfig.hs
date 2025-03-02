{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Gen.RunConfig where

import qualified Gen.Config as Config
import qualified Data.Aeson
import Data.Aeson.Types
import GHC.Generics
import qualified Data.ByteString as BS
import qualified Data.List as List
import qualified Data.Map as Map
import qualified System.Directory as Dir
import qualified System.FilePath as FilePath
import Control.Monad (forM)
import qualified Data.Text.IO as TextIO
import qualified Data.Text.Encoding as TextEnc
import qualified Data.Text as Text

data RunConfig = RunConfig
    { app :: Maybe App
    , docs :: Maybe Docs
    , assets :: Maybe [AssetGroup]
    , theme :: Maybe Config.Theme
    } deriving (Generic)

instance FromJSON RunConfig
instance ToJSON RunConfig


data App = App
    { pages :: [Page]
    , stores :: [Store]
    } deriving (Generic)

instance FromJSON App
instance ToJSON App

data Page = Page
    { _id :: String
    , _moduleName :: [String]
    , urlOnly :: Bool
    , route :: Maybe Route
    } deriving (Generic)

instance FromJSON Page
instance ToJSON Page

data Store = Store
    { storeId :: String
    } deriving (Generic)

instance FromJSON Store
instance ToJSON Store


data Route = Route
    { routeId :: String
    , url :: String
    , redirectFrom :: [String]
    } deriving (Generic)

instance FromJSON Route
instance ToJSON Route

-- DOCS

data Docs = Docs
    { readme :: Maybe String
    , guides ::  [Guide]
    , project :: Data.Aeson.Value
    , modules ::  [Data.Aeson.Value]
    , deps :: Map.Map String [Data.Aeson.Value]
    } deriving (Generic)

instance FromJSON Docs
instance ToJSON Docs


data Guide = Guide
    { guideName :: String
    , guideContent :: Maybe String
    } deriving (Generic)


instance FromJSON Guide
instance ToJSON Guide



data AssetGroup = AssetGroup
    { name :: String 
    , crumbs :: [String]
    , pathOnServer :: String
    , content :: String
    } deriving (Generic)    

instance FromJSON AssetGroup
instance ToJSON AssetGroup

toRunConfig :: FilePath -> Config.Config -> IO RunConfig
toRunConfig cwd config = do
    -- Convert stores by reading store files
    storeIds <- findStores (cwd FilePath.</> "src" FilePath.</> "app" FilePath.</> "Store")
    
    let stores = map (\sid -> Store { storeId = sid }) storeIds
    
    -- Convert pages from Config.AppConfig
    let pages = case Config.configApp config of
            Nothing -> []
            Just appConfig -> 
                Map.foldrWithKey (\k v acc -> 
                    let moduleName = Text.splitOn "." k
                        route = if Config.pageUrlOnly v
                            then Nothing
                            else Just $ Route 
                                { routeId = Text.unpack k
                                , url = Text.unpack (Config.pageUrl v)
                                , redirectFrom = map Text.unpack (Config.pageRedirectFrom v)
                                }
                    in Page 
                        { _id = Text.unpack k
                        , _moduleName = map Text.unpack moduleName
                        , urlOnly = Config.pageUrlOnly v
                        , route = route
                        } : acc
                ) [] (Config.appPages appConfig)

    -- Convert docs
    docs <- case Config.configDocs config of
        Nothing -> return Nothing
        Just docsConfig -> do
            let docsPath = cwd FilePath.</> Text.unpack (Config.docsSrc docsConfig)
            
            -- Read README if it exists
            readmeContent <- tryReadFile (docsPath FilePath.</> "README.md")
            
            -- Read guides
            guides <- case Config.docsGuides docsConfig of
                Nothing -> return []
                Just guidePaths -> forM (map Text.unpack guidePaths) $ \guidePath -> do
                    content <- tryReadFile (docsPath FilePath.</> guidePath FilePath.<.> "md")
                    return Guide 
                        { guideName = FilePath.dropExtension guidePath
                        , guideContent = content
                        }
            
            return $ Just Docs
                { readme = readmeContent
                , guides = guides
                , project = Data.Aeson.Null  -- Placeholder, would need project documentation
                , modules = []  -- Placeholder, would need module documentation
                , deps = Map.empty  -- Placeholder, would need dependency documentation
                }

    -- Convert assets
    assets <- case Config.configAssets config of
        Nothing -> return Nothing
        Just assetConfigs -> do
            assetGroups <- forM (Map.toList assetConfigs) $ \(name, conf) -> do
                let srcPath = Text.unpack (Config.assetSrc conf)
                    serverPath = Text.unpack (Config.assetOnServer conf)
                    crumbPath = FilePath.splitPath srcPath
                
                content <- tryReadFile (cwd FilePath.</> srcPath)
                return AssetGroup
                    { name = Text.unpack name
                    , crumbs = filter (not . null) $ map FilePath.dropTrailingPathSeparator crumbPath
                    , pathOnServer = serverPath
                    , content = maybe "" id content
                    }
            
            return (Just assetGroups)
    
    return RunConfig
        { app = if null pages && null stores 
                then Nothing 
                else Just App { pages = pages, stores = stores }
        , docs = docs
        , assets = assets
        , theme = Config.configTheme config
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
            return $ map FilePath.dropExtension $ 
                    filter (\f -> FilePath.takeExtension f == ".elm") files


