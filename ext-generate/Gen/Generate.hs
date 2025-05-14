{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Gen.Generate (readConfig, readConfigOrFail, generate, File(..)) where

import qualified Gen.Config as Config
import qualified Gen.RunConfig as RunConfig
import qualified Gen.Javascript as Javascript
import qualified Gen.Templates.Loader as Loader
import qualified Gen.Templates as Templates
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as Map
import qualified System.Directory as Dir
import qualified Data.Aeson.Encode.Pretty as Aeson
import System.FilePath ((</>), takeBaseName)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Maybe (fromMaybe)
import Control.Monad (forM_)
import Data.Function ((&))
import Data.Aeson (eitherDecodeStrict, object, (.=), FromJSON, ToJSON)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (object, (.=))
import GHC.Generics (Generic)


readConfig :: IO (Either String Config.Config)
readConfig = do
    config <- BSL.readFile "elm.generate.json"
    return $ eitherDecodeStrict (BSL.toStrict config)


readConfigOrFail :: IO Config.Config
readConfigOrFail = do
    configResult <- readConfig
    case configResult of
        Left err -> 
            fail $ "Failed to parse elm.generate.json: " ++ err
        Right config -> 
            return config


data File = File {
    outputDir :: String,
    path :: String,
    contents :: String  
} deriving (Show, Generic)

instance FromJSON File
instance ToJSON File

data GeneratedFiles = GeneratedFiles {
    generated :: [File]
} deriving (Show, Generic)

instance FromJSON GeneratedFiles
instance ToJSON GeneratedFiles


-- | Main generation function
generate :: Config.Config -> IO (Either String [File])
generate config = do
    cwd <- Dir.getCurrentDirectory

    -- Convert Config to RunConfig and then run Javascript generator
    runConfig <- RunConfig.toRunConfig cwd config
    let jsInput = object
            [ "outputDir" .= ("elm-stuff/generated" :: String)
            , "flags" .= runConfig
            ]

    -- putStrLn $ "Running Javascript generator with input: " ++ Text.unpack (Text.decodeUtf8 (BSL.toStrict (Aeson.encodePretty jsInput)))

    result <- Javascript.run Javascript.generatorJs (BS.toStrict (Aeson.encodePretty jsInput))
    case result of
        Left err -> return $ Left err
        Right output -> do
            case eitherDecodeStrict (Text.encodeUtf8 (Text.pack output)) of
                Left err -> do 
                    putStrLn output
                    return $ Left err
                Right (GeneratedFiles files) -> return $ Right files
    




-- | Synchronize pages between filesystem and config
syncPages :: Config.Config -> IO Config.Config
syncPages config = do
    -- Scan for existing page files
    existingPages <- findPageFiles
    
    -- Get configured pages from config
    let configuredPages = maybe Map.empty Config.appPages (Config.configApp config)
    
    -- Create missing page files using template
    
    let pageTemplate = findPageTemplate Templates.templates
    
    -- Create files for configured pages that don't exist
    forM_ (Map.toList configuredPages) $ \(pageId, _) -> do
        let filePath = Text.unpack Config.src </> "Page" </> Text.unpack pageId <> ".elm"
        fileExists <- Dir.doesFileExist filePath
        when (not fileExists) $ do
            -- Create directory if it doesn't exist
            Dir.createDirectoryIfMissing True (Text.unpack Config.src </> "Page")
            -- Create file from template
            case pageTemplate of
                Just tmpl -> do
                    let contents = Text.decodeUtf8 (Loader.content tmpl)
                    let pageName = pageId
                    let newContents = contents
                            & Text.replace "{{name}}" pageName
                            & Text.replace "{{name_underscored}}" (Text.replace "." "_" pageName)
                    BS.writeFile filePath (Text.encodeUtf8 newContents)
                Nothing -> putStrLn $ "Warning: No template found for page " <> Text.unpack pageId

    -- Add missing pages to config
    let newPages = foldr addToConfig configuredPages existingPages
    
    -- Return updated config
    return $ config { Config.configApp = Just $ Config.AppConfig newPages }
  where
    findPageFiles :: IO [FilePath]
    findPageFiles = do
        let pageDir = Text.unpack Config.src </> "Page"
        dirExists <- Dir.doesDirectoryExist pageDir
        if dirExists
            then do
                files <- Dir.listDirectory pageDir
                return $ filter (\f -> takeBaseName f /= "NotFound") 
                       $ filter (\f -> last (takeBaseName f) /= '_')
                       $ filter (\f -> takeExtension f == ".elm") files
            else return []

    findPageTemplate :: [Loader.Template] -> Maybe Loader.Template
    findPageTemplate = find (\t -> 
        Loader.target t == Loader.OneOff && 
        Loader.filename t == "Page.elm")

    addToConfig :: FilePath -> Map.Map Text Config.PageConfig -> Map.Map Text Config.PageConfig
    addToConfig file pages =
        let pageId = Text.pack $ takeBaseName file
            defaultPage = Config.PageConfig 
                { Config.pageUrl = "/" <> pageId
                , Config.pageRedirectFrom = []
                , Config.pageUrlOnly = False
                }
        in if Map.member pageId pages
           then pages
           else Map.insert pageId defaultPage pages

    when :: Bool -> IO () -> IO ()
    when True action = action
    when False _ = return ()

    find :: (a -> Bool) -> [a] -> Maybe a
    find _ [] = Nothing
    find pred (x:xs) = if pred x then Just x else find pred xs

    takeExtension :: FilePath -> String
    takeExtension = reverse . takeWhile (/= '.') . reverse
