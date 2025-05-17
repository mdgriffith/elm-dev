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
    , Theme(..)
    , GraphQLConfig(..)
    , DocsConfig(..)
    , defaultTheme
    , defaultDocs
    , elmSrc
    , effectTsSrc
    ) where

import qualified Data.ByteString as BS
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Aeson
import Data.Aeson.Types
import GHC.Generics
import Data.Maybe (catMaybes)
import Control.Applicative ((<|>))
import System.FilePath ((</>))


elmSrc :: String
elmSrc = "src" </> "app"

effectTsSrc :: String
effectTsSrc = "src" </> "js" </>"effect"

-- | Main configuration type
data Config = Config
    { configPackageManager :: Maybe PackageManager
    , configApp :: Maybe AppConfig
    , configAssets :: Maybe (Map.Map Text AssetConfig)
    , configTheme :: Maybe Theme
    , configGraphQL :: Maybe GraphQLConfig
    , configDocs :: Maybe DocsConfig
    } deriving (Generic)

instance FromJSON Config where
    parseJSON = withObject "Config" $ \v -> Config
        <$> v .:? "packageManager"
        <*> v .:? "app"
        <*> v .:? "assets"
        <*> v .:? "theme"
        <*> v .:? "graphql"
        <*> v .:? "docs"

instance ToJSON Config where
    toJSON Config{..} = object $ catMaybes
        [ ("packageManager",) . toJSON <$> configPackageManager
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

instance FromJSON AssetConfig where
    parseJSON = withObject "AssetConfig" $ \v -> AssetConfig
        <$> v .: "src"
        <*> v .: "onServer"

instance ToJSON AssetConfig where
    toJSON AssetConfig{..} = object
        [ "src" .= assetSrc
        , "onServer" .= assetOnServer
        ]

data Theme = Theme
    { themeTarget :: Maybe ThemeTarget
    , themeColors :: Maybe ColorsTheme
    , themeSpacing :: Maybe (Map.Map Text Int)
    , themeTypography :: Maybe [Font]
    , themeBorders :: Maybe BorderConfig
    } deriving (Generic)

instance FromJSON Theme where
    parseJSON = withObject "Theme" $ \v -> Theme
        <$> v .:? "target"
        <*> v .:? "colors"
        <*> v .:? "spacing"
        <*> v .:? "typography"
        <*> v .:? "borders"

instance ToJSON Theme where
    toJSON Theme{..} = object $ catMaybes
        [ ("target",) . toJSON <$> themeTarget
        , ("colors",) . toJSON <$> themeColors
        , ("spacing",) . toJSON <$> themeSpacing
        , ("typography",) . toJSON <$> themeTypography
        , ("borders",) . toJSON <$> themeBorders
        ]

data ColorsTheme = ColorsTheme
    { colorsPalette :: Maybe (Map.Map Text Text)
    , colorsAliases :: Maybe ColorAliasTheme
    , colorsText :: Maybe StyleSchema
    , colorsBackground :: Maybe StyleSchema
    , colorsBorder :: Maybe StyleSchema
    } deriving (Generic)

instance FromJSON ColorsTheme where
    parseJSON = withObject "ColorsTheme" $ \v -> ColorsTheme
        <$> v .:? "palette"
        <*> v .:? "aliases"
        <*> v .:? "text"
        <*> v .:? "background"
        <*> v .:? "border"

instance ToJSON ColorsTheme where
    toJSON ColorsTheme{..} = object $ catMaybes
        [ ("palette",) . toJSON <$> colorsPalette
        , ("aliases",) . toJSON <$> colorsAliases
        , ("text",) . toJSON <$> colorsText
        , ("background",) . toJSON <$> colorsBackground
        , ("border",) . toJSON <$> colorsBorder
        ]

data ColorAliasTheme = ColorAliasTheme
    { aliasNeutral :: Text
    , aliasPrimary :: Text
    , aliasFocus :: Maybe Text
    , aliasSuccess :: Maybe Text
    , aliasError :: Maybe Text
    } deriving (Generic)

instance FromJSON ColorAliasTheme where
    parseJSON = withObject "ColorAliasTheme" $ \v -> ColorAliasTheme
        <$> v .: "neutral"
        <*> v .: "primary"
        <*> v .:? "focus"
        <*> v .:? "success"
        <*> v .:? "error"

instance ToJSON ColorAliasTheme where
    toJSON ColorAliasTheme{..} = object $ catMaybes
        [ Just ("neutral", toJSON aliasNeutral)
        , Just ("primary", toJSON aliasPrimary)
        , ("focus",) . toJSON <$> aliasFocus
        , ("success",) . toJSON <$> aliasSuccess
        , ("error",) . toJSON <$> aliasError
        ]

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

instance FromJSON GraphQLConfig where
    parseJSON = withObject "GraphQLConfig" $ \v -> GraphQLConfig
        <$> v .: "schema"
        <*> v .:? "namespace"
        <*> v .: "header"
        <*> v .:? "generateMocks"
        <*> v .:? "queries"
        <*> v .:? "globalFragments"
        <*> v .:? "existingEnumDefinitions"

instance ToJSON GraphQLConfig where
    toJSON GraphQLConfig{..} = object $ catMaybes
        [ Just ("schema", toJSON graphqlSchema)
        , ("namespace",) . toJSON <$> graphqlNamespace
        , Just ("header", toJSON graphqlHeader)
        , ("generateMocks",) . toJSON <$> graphqlGenerateMocks
        , ("queries",) . toJSON <$> graphqlQueries
        , ("globalFragments",) . toJSON <$> graphqlGlobalFragments
        , ("existingEnumDefinitions",) . toJSON <$> graphqlExistingEnumDefinitions
        ]

data DocsConfig = DocsConfig
    { docsSrc :: Text
    , docsModules :: Maybe [Text]
    , docsGuides :: Maybe [Text]
    } deriving (Generic)

instance FromJSON DocsConfig
instance ToJSON DocsConfig


defaultDocs :: DocsConfig
defaultDocs = DocsConfig
    { docsSrc = "docs"
    , docsModules = Nothing
    , docsGuides = Nothing
    }



data PackageManager
    = NPM
    | Yarn
    | PNPM
    | Bun
    deriving (Generic)

instance FromJSON PackageManager where
    parseJSON = withText "PackageManager" $ \case
        "npm" -> pure NPM
        "yarn" -> pure Yarn
        "pnpm" -> pure PNPM
        "bun" -> pure Bun
        _ -> fail "Invalid package manager"

instance ToJSON PackageManager where
    toJSON NPM = String "npm"
    toJSON Yarn = String "yarn"
    toJSON PNPM = String "pnpm"
    toJSON Bun = String "bun"

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
        else if null pageRedirectFrom then
            String pageUrl
        else
            object $ catMaybes
                [ Just ("url" .= pageUrl)
                , Just ("redirectFrom" .= pageRedirectFrom)
                ]

data AppConfig = AppConfig
    { appPages :: Map.Map Text PageConfig
    } deriving (Generic)

instance FromJSON AppConfig where
    parseJSON = withObject "AppConfig" $ \v -> AppConfig
        <$> v .: "pages"

instance ToJSON AppConfig where
    toJSON AppConfig{..} = object
        [ "pages" .= appPages
        ]

-- | Main entry point for parsing configuration
-- parse :: BS.ByteString -> Either String Config
-- parse = eitherDecodeStrict

defaultTheme :: Theme
defaultTheme = Theme
    { themeTarget = Just ElmUI
    , themeColors = Just defaultColors
    , themeSpacing = Just $ Map.fromList
        [ ("xs", 4)
        , ("sm", 8)
        , ("md", 16)
        , ("lg", 32)
        , ("xl", 64)
        ]
    , themeTypography = Just [defaultFont]
    , themeBorders = Just defaultBorders
    }

defaultColors :: ColorsTheme
defaultColors = ColorsTheme
    { colorsPalette = Just $ Map.fromList
        [ ("black", "#000000")
        , ("white", "#FFFFFF")
        , ("gray-100", "#F3F4F6")
        , ("gray-500", "#6B7280")
        , ("gray-900", "#111827")
        , ("blue-500", "#3B82F6")
        ]
    , colorsAliases = Just defaultColorAliases
    , colorsText = Just $ StyleString "gray-900"
    , colorsBackground = Just $ StyleString "white"
    , colorsBorder = Just $ StyleString "gray-100"
    }

defaultColorAliases :: ColorAliasTheme
defaultColorAliases = ColorAliasTheme
    { aliasNeutral = "gray-500"
    , aliasPrimary = "blue-500"
    , aliasFocus = Just "blue-500"
    , aliasSuccess = Nothing
    , aliasError = Nothing
    }

defaultFont :: Font
defaultFont = Font
    { fontFamily = ["system-ui", "sans-serif"]
    , fontCapitalSizing = Nothing
    , fontSizes = Map.fromList
        [ ("sm", FontDetails 14 (Just 1.5) Nothing Nothing Nothing Nothing)
        , ("base", FontDetails 16 (Just 1.5) Nothing Nothing Nothing Nothing)
        , ("lg", FontDetails 18 (Just 1.5) Nothing Nothing Nothing Nothing)
        ]
    }

defaultBorders :: BorderConfig
defaultBorders = BorderConfig
    { borderRadius = Just $ Map.fromList
        [ ("sm", 4)
        , ("md", 6)
        , ("lg", 8)
        ]
    , borderWidth = Just $ Map.fromList
        [ ("thin", 1)
        , ("medium", 2)
        , ("thick", 4)
        ]
    }
