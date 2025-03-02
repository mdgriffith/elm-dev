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







data RunConfig = RunConfig
    { app :: Maybe App
    , docs :: Maybe Docs
    , assets :: Maybe Assets
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
    { id :: String
    , moduleName :: [String]
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


data Assets = List AssetGroup deriving (Generic)

instance FromJSON Assets
instance ToJSON Assets

data AssetGroup = AssetGroup
    { name :: String 
    , crumbs :: [String]
    , pathOnServer :: String
    , content :: String
    } deriving (Generic)    

instance FromJSON AssetGroup
instance ToJSON AssetGroup


