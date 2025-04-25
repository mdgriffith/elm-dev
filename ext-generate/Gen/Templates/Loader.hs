{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveLift #-}
module Gen.Templates.Loader 
    ( Gen.Templates.Loader.read
    , Template(..)
    , Target(..)
    ) where


import qualified Data.ByteString as BS
import System.FilePath (splitPath, (</>), dropExtension)
import qualified System.Directory as Dir
import Control.Monad (forM)
import Data.List (intercalate, isPrefixOf)
import Data.Char (toLower)
import Language.Haskell.TH.Syntax (Lift)

data Target = ToJs 
            | ToRoot 
            | ToHidden 
            | ToSrc 
            | Customizable 
            | OneOff
            deriving (Show, Eq, Lift)

toFilePath :: Target -> String
toFilePath ToJs = "js"
toFilePath ToRoot = ""
toFilePath ToHidden = ".hidden"
toFilePath ToSrc = "src"
toFilePath Customizable = "custom"
toFilePath OneOff = "src"

data Template = Template 
    { target :: Target
    , content :: BS.ByteString
    , plugin :: String
    , dir :: String
    , templateName :: String
    , filename :: String
    , elmModuleName :: String
    } deriving (Show, Lift)

-- Convert Target to directory name
targetToDir :: Target -> String
targetToDir = map toLower . show

-- Parse directory path into Template components
parseTemplatePath :: FilePath -> Maybe (String, Target, String, String, String)
parseTemplatePath path = case filter (not . null) $ splitPath path of
    (plugin':_:target':rest) -> do
        target <- parseTarget (init target') -- init removes trailing slash
        let (dirs, file) = splitDirAndFile rest
        let elmModuleName = intercalate "." $ map (filter (/= '/')) $ init rest ++ [dropExtension file]
        return (init plugin', target, dirs, file, elmModuleName)
    _ -> Nothing
  where
    parseTarget :: String -> Maybe Target
    parseTarget "toJs" = Just ToJs
    parseTarget "toRoot" = Just ToRoot
    parseTarget "toHidden" = Just ToHidden
    parseTarget "toSrc" = Just ToSrc
    parseTarget "customizable" = Just Customizable
    parseTarget "oneOff" = Just OneOff
    parseTarget _ = Nothing
    
    splitDirAndFile :: [String] -> (String, String)
    splitDirAndFile [] = ("", "")
    splitDirAndFile [file] = ("", init file)
    splitDirAndFile paths = (intercalate "/" $ map init $ init paths, init $ last paths)

-- Get all files recursively from a directory
getFilesRecursive :: FilePath -> IO [FilePath]
getFilesRecursive dir = do
    entries <- Dir.listDirectory dir
    paths <- forM entries $ \entry -> do
        let path = dir </> entry
        isFile <- Dir.doesFileExist path
        if isFile
            then return [path]
            else do
                isDir <- Dir.doesDirectoryExist path
                if isDir
                    then getFilesRecursive path
                    else return []
    return $ concat paths

-- Read templates from directory at compile time
readTemplates :: FilePath -> FilePath -> IO [Template]
readTemplates base targetDir = do
    files <- getFilesRecursive (base </> targetDir)
    forM files $ \path -> do
        let filePath = if base `isPrefixOf` path
                        then drop (length base + 1) path 
                        else error $ "Template path " ++ path ++ " is not under base directory " ++ base


        case parseTemplatePath filePath of
            Just (pluginName, targetType, dirPath, fname, elmModuleName) -> do
                content <- BS.readFile path
                return $ Template 
                    { target = targetType
                    , content = content
                    , plugin = pluginName
                    , dir = dirPath
                    , filename = fname
                    , templateName = dropExtension fname
                    , elmModuleName = elmModuleName
                    }
            Nothing -> error $ "Invalid template path structure: " ++ path ++ " for base " ++ base ++ " for filePath " ++ filePath

-- Read multiple template directories
read :: FilePath -> [FilePath] -> IO [Template]
read base paths = concat <$> mapM (readTemplates base) paths