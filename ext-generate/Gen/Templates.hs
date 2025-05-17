{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Gen.Templates (templates, write, writeGroup, writeGroupCustomizable) where


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

            cwd <- Dir.getCurrentDirectory
            let targetPath = cwd </> src </> templateName </> Text.unpack pageName <.> "elm"

            Dir.createDirectoryIfMissing True (cwd </> src)
            Dir.createDirectoryIfMissing True (cwd </> src </> templateName)
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
version = "301ad885348added"