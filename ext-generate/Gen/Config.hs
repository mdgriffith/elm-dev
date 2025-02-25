{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Gen.Config 
    ( Config(..)
    , PackageManager(..)
    , ThemeTarget(..)
    , AppConfig(..)
    , PageConfig(..)
    , AssetConfig(..)
    , ThemeConfig(..)
    , GraphQLConfig(..)
    , DocsConfig(..)
    ) where

import qualified Data.ByteString as BS
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Aeson
import Data.Aeson.Types
import GHC.Generics
import Data.Maybe (catMaybes)
import Control.Applicative ((<|>))

-- | Main configuration type
data Config = Config
    { configPackageManager :: Maybe PackageManager
    , configSrc :: Maybe Text
    , configJs :: Maybe Text
    , configApp :: Maybe AppConfig
    , configAssets :: Maybe (Map.Map Text AssetConfig)
    , configTheme :: Maybe ThemeConfig
    , configGraphQL :: Maybe GraphQLConfig
    , configDocs :: Maybe DocsConfig
    } deriving (Generic)

instance FromJSON Config where
    parseJSON = withObject "Config" $ \v -> Config
        <$> v .:? "packageManager"
        <*> v .:? "src"
        <*> v .:? "js"
        <*> v .:? "app"
        <*> v .:? "assets"
        <*> v .:? "theme"
        <*> v .:? "graphql"
        <*> v .:? "docs"

instance ToJSON Config where
    toJSON Config{..} = object $ catMaybes
        [ ("packageManager",) . toJSON <$> configPackageManager
        , ("src",) . toJSON <$> configSrc
        , ("js",) . toJSON <$> configJs
        , ("app",) . toJSON <$> configApp
        , ("assets",) . toJSON <$> configAssets
        , ("theme",) . toJSON <$> configTheme
        , ("graphql",) . toJSON <$> configGraphQL
        , ("docs",) . toJSON <$> configDocs
        ]

data AssetConfig = AssetConfig
    { assetSrc :: Text
    , assetOnServer :: Text
    } deriving (Generic)

instance FromJSON AssetConfig
instance ToJSON AssetConfig

data ThemeConfig = ThemeConfig
    { themeTarget :: Maybe ThemeTarget
    , themeColors :: Maybe ColorsTheme
    , themeSpacing :: Maybe (Map.Map Text Int)
    , themeTypography :: Maybe [Font]
    , themeBorders :: Maybe BorderConfig
    } deriving (Generic)

instance FromJSON ThemeConfig
instance ToJSON ThemeConfig

data ColorsTheme = ColorsTheme
    { colorsPalette :: Maybe (Map.Map Text Text)
    , colorsAliases :: Maybe ColorAliasTheme
    , colorsText :: Maybe StyleSchema
    , colorsBackground :: Maybe StyleSchema
    , colorsBorder :: Maybe StyleSchema
    } deriving (Generic)

instance FromJSON ColorsTheme
instance ToJSON ColorsTheme

data ColorAliasTheme = ColorAliasTheme
    { aliasNeutral :: Text
    , aliasPrimary :: Text
    , aliasFocus :: Maybe Text
    , aliasSuccess :: Maybe Text
    , aliasError :: Maybe Text
    } deriving (Generic)

instance FromJSON ColorAliasTheme
instance ToJSON ColorAliasTheme

data StyleSchema
    = StyleString Text
    | StyleObject StyleObjectConfig
    deriving (Generic)

instance FromJSON StyleSchema where
    parseJSON v = (StyleString <$> parseJSON v)
              <|> (StyleObject <$> parseJSON v)

instance ToJSON StyleSchema where
    toJSON (StyleString t) = toJSON t
    toJSON (StyleObject o) = toJSON o

data StyleObjectConfig = StyleObjectConfig
    { styleColor :: Text
    , styleHover :: Maybe Text
    , styleActive :: Maybe Text
    , styleFocus :: Maybe Text
    , styleDarkmode :: Maybe (Either Text StyleObjectConfig)
    } deriving (Generic)

instance FromJSON StyleObjectConfig
instance ToJSON StyleObjectConfig

data Font = Font
    { fontFamily :: [Text]
    , fontCapitalSizing :: Maybe CapitalSizing
    , fontSizes :: Map.Map Text FontDetails
    } deriving (Generic)

instance FromJSON Font
instance ToJSON Font

data CapitalSizing = CapitalSizing
    { capitalTop :: Maybe Float
    , capitalBottom :: Maybe Float
    , capitalFontSizeByCapital :: Maybe Float
    } deriving (Generic)

instance FromJSON CapitalSizing
instance ToJSON CapitalSizing

data FontDetails = FontDetails
    { fontSize :: Int
    , fontLineHeight :: Maybe Float
    , fontWeight :: Maybe Int
    , fontWeights :: Maybe [Int]
    , fontVariant :: Maybe Text
    , fontVariants :: Maybe [Text]
    } deriving (Generic)

instance FromJSON FontDetails
instance ToJSON FontDetails

data BorderConfig = BorderConfig
    { borderRadius :: Maybe (Map.Map Text Float)
    , borderWidth :: Maybe (Map.Map Text Float)
    } deriving (Generic)

instance FromJSON BorderConfig
instance ToJSON BorderConfig

data GraphQLConfig = GraphQLConfig
    { graphqlSchema :: Text
    , graphqlNamespace :: Maybe Text
    , graphqlHeader :: [Text]
    , graphqlGenerateMocks :: Maybe Bool
    , graphqlQueries :: Maybe Text
    , graphqlGlobalFragments :: Maybe Text
    , graphqlExistingEnumDefinitions :: Maybe Text
    } deriving (Generic)

instance FromJSON GraphQLConfig
instance ToJSON GraphQLConfig

data DocsConfig = DocsConfig
    { docsSrc :: Text
    , docsModules :: Maybe [Text]
    , docsGuides :: Maybe [Text]
    } deriving (Generic)

instance FromJSON DocsConfig
instance ToJSON DocsConfig

data PackageManager
    = NPM
    | Yarn
    | PNpm
    | Bun
    deriving (Generic)

instance FromJSON PackageManager
instance ToJSON PackageManager

data ThemeTarget
    = Html
    | ElmUI
    deriving (Generic)

instance FromJSON ThemeTarget where
    parseJSON = withText "ThemeTarget" $ \case
        "html" -> pure Html
        "elm-ui" -> pure ElmUI
        _ -> fail "Invalid theme target"

instance ToJSON ThemeTarget where
    toJSON Html = String "html"
    toJSON ElmUI = String "elm-ui"

data PageConfig = PageConfig
    { pageUrl :: Text
    , pageRedirectFrom :: [Text]
    , pageUrlOnly :: Bool
    } deriving (Generic)

instance FromJSON PageConfig where
    parseJSON v = 
        case v of
            String url -> 
                pure $ PageConfig url [] False
            Object obj -> 
                (do
                    url <- obj .: "url"
                    redirectFrom <- obj .:? "redirectFrom" .!= []
                    pure $ PageConfig url redirectFrom False)
                <|>
                (do
                    urlOnly <- obj .: "urlOnly"
                    redirectFrom <- obj .:? "redirectFrom" .!= []
                    pure $ PageConfig urlOnly redirectFrom True)
            _ -> fail "Invalid PageConfig"

instance ToJSON PageConfig where
    toJSON PageConfig{..} =
        if pageUrlOnly then
            object $ catMaybes
                [ Just ("urlOnly" .= pageUrl)
                , if null pageRedirectFrom
                    then Nothing
                    else Just ("redirectFrom" .= pageRedirectFrom)
                ]
        else
            object $ catMaybes
                [ Just ("url" .= pageUrl)
                , if null pageRedirectFrom
                    then Nothing
                    else Just ("redirectFrom" .= pageRedirectFrom)
                ]

data AppConfig = AppConfig
    { appPages :: Map.Map Text PageConfig
    } deriving (Generic)

instance FromJSON AppConfig
instance ToJSON AppConfig

-- | Main entry point for parsing configuration
-- parse :: BS.ByteString -> Either String Config
-- parse = eitherDecodeStrict
