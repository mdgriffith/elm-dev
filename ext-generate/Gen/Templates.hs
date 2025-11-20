{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Gen.Templates (templates, write, writeTs, writeGroup, writeGroupCustomizable) where


import qualified Gen.Templates.Loader
import qualified Language.Haskell.TH.Syntax as TH
-- Writing templates
import qualified Data.Text.IO as TIO
import qualified Data.Text as Text
import qualified System.Directory as Dir
import qualified Data.List as List
import qualified Data.Text.Encoding
import System.FilePath ((</>), (<.>))
import Data.Function ((&))
import Control.Monad (forM_, when)
import qualified Data.Char as Char


-- The actual templates list, populated at compile time
templates :: [Gen.Templates.Loader.Template]
templates = $(TH.runIO 
                (Gen.Templates.Loader.read
                    "ext-generate/generator" 
                    [ "app"
                    , "assets"
                    ]
                ) >>= TH.lift
             )


write :: String -> String -> String -> IO ()
write templateName src name = do
    let maybeTemplate = List.find (\t -> 
            Gen.Templates.Loader.target t == Gen.Templates.Loader.OneOff && 
            Gen.Templates.Loader.templateName t == templateName) Gen.Templates.templates
    
    case maybeTemplate of
        Nothing -> 
            fail "Could not find page template"

        Just template -> do
            -- Get template contents
            let contents = Data.Text.Encoding.decodeUtf8 (Gen.Templates.Loader.content template) 
            
            -- Do replacements
            let pageName = Text.pack name
            let pageNameUnderscored = Text.replace "." "_" pageName
            let newContents = contents
                    & Text.replace "{{name}}" pageName
                    & Text.replace "{{name_underscored}}" pageNameUnderscored
                    & Text.replace "{{name_decapitalized}}" (Text.pack $ decapitalize name)

            cwd <- Dir.getCurrentDirectory
            let targetPath = cwd </> src </> templateName </> Text.unpack pageName <.> "elm"

            Dir.createDirectoryIfMissing True (cwd </> src)
            Dir.createDirectoryIfMissing True (cwd </> src </> templateName)
            TIO.writeFile targetPath newContents


decapitalize :: String -> String
decapitalize (c:cs) = Char.toLower c : cs
decapitalize [] = []


-- | Convert CamelCase to kebab-case
toKebabCase :: String -> String
toKebabCase = 
    let go [] = []
        go (c:cs) = 
            if Char.isUpper c 
            then '-' : Char.toLower c : go cs
            else c : go cs
    in dropWhile (== '-') . go

writeTs :: String -> String -> String -> IO ()
writeTs templateName src name = do
    let maybeTemplate = List.find (\t -> 
            Gen.Templates.Loader.target t == Gen.Templates.Loader.OneOff && 
            Gen.Templates.Loader.templateName t == templateName) Gen.Templates.templates
    
    case maybeTemplate of
        Nothing -> 
            fail "Could not find page template"

        Just template -> do
            -- Contents
            let contents = Data.Text.Encoding.decodeUtf8 (Gen.Templates.Loader.content template)
            let nameKebab = toKebabCase name
            let newContents = contents
                    & Text.replace "{{name}}" (Text.pack name)
                    & Text.replace "{{name_decapitalized}}" (Text.pack $ decapitalize name)

            -- Write
            cwd <- Dir.getCurrentDirectory
            let targetPath = cwd </> src </> nameKebab <.> "ts"
            TIO.writeFile targetPath newContents


writeGroup :: Gen.Templates.Loader.Target -> String -> IO ()
writeGroup target src = do
    let matchingTemplates = List.filter (\t -> Gen.Templates.Loader.target t == target) Gen.Templates.templates
    forM_ matchingTemplates $ \template -> do
        -- Path
        let targetDir = src </> Gen.Templates.Loader.dir template
        Dir.createDirectoryIfMissing True targetDir

        -- Contents
        let contents = Data.Text.Encoding.decodeUtf8 (Gen.Templates.Loader.content template)

        -- Write
        TIO.writeFile (targetDir </> Gen.Templates.Loader.filename template) contents
    

writeGroupCustomizable :: Gen.Templates.Loader.Target -> String -> String -> IO ()
writeGroupCustomizable target src hiddenSrc = do
    let matchingTemplates = List.filter (\t -> Gen.Templates.Loader.target t == target) Gen.Templates.templates
    forM_ matchingTemplates $ \template -> do
        -- Paths
        let srcPath = src </> Gen.Templates.Loader.dir template </> Gen.Templates.Loader.filename template
        let hiddenPath = hiddenSrc </> Gen.Templates.Loader.dir template </> Gen.Templates.Loader.filename template

        -- Check if file exists in src
        srcExists <- Dir.doesFileExist srcPath
        
        if srcExists
            then do
                -- If file exists in src, check and delete from hiddenSrc if it exists
                hiddenExists <- Dir.doesFileExist hiddenPath
                when hiddenExists $ Dir.removeFile hiddenPath
            else do
                Dir.createDirectoryIfMissing True (hiddenSrc </> Gen.Templates.Loader.dir template)
                -- If file doesn't exist in src, write to hiddenSrc
                let contents = Data.Text.Encoding.decodeUtf8 (Gen.Templates.Loader.content template)
                TIO.writeFile hiddenPath contents
    

-- Version.  Iterate when we change the templates
version :: String
version = "d23d2ba8e5d3ae6d"