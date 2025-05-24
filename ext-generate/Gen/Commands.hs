{-# LANGUAGE OverloadedStrings #-}
module Gen.Commands (initialize, make, addPage, addStore, addEffect, addDocs, addTheme, addListener, customize) where

import qualified CommandParser
import qualified Terminal
import qualified Data.Char as Char
import Terminal ((!), (?), (...))
import Data.Text (Text)
import qualified Text.PrettyPrint.ANSI.Leijen as P
import qualified Terminal.Helpers
import qualified Elm.ModuleName
import qualified Gen.Javascript
import qualified Gen.Generate
import qualified Gen.Config
import qualified Data.Name as Name
import qualified Data.Maybe as Maybe
import qualified Data.List as List
import qualified Gen.Config as Config
import qualified Gen.Commands.Init
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Lazy as BS
import qualified Make
import System.FilePath ((</>))
import Data.Text (pack)
import Data.Aeson (eitherDecodeStrict)
import qualified Data.Text as Text
import qualified Data.List (find)
import qualified Data.Text.IO as TIO
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import System.FilePath ((<.>))
import qualified Gen.Templates
import qualified Gen.Templates.Loader
import Data.Function ((&))
import qualified Data.Text.Encoding
import qualified System.Directory as Dir (doesFileExist, removeFile, getCurrentDirectory, createDirectoryIfMissing)
import qualified Reporting.Exit as Exit
import Control.Monad (when)
import qualified System.FilePath as FP
import qualified Terminal.Colors
import qualified Ext.Log
import qualified System.IO as IO



-- Flag types
-- data InitFlags = InitFlags
data MakeFlags = MakeFlags
data AddFlags = AddFlags
data CustomizeFlags = CustomizeFlags

-- INIT COMMAND
initialize :: CommandParser.Command
initialize =
    CommandParser.command ["init"] "Create a new Elm project" 
      Nothing
      Gen.Commands.Init.args
      Gen.Commands.Init.flags
      Gen.Commands.Init.run


-- MAKE COMMAND
make :: CommandParser.Command
make = CommandParser.command ["make"] "Build your Elm project" Nothing parseMakeArgs parseMakeFlags runMake
  where 
    parseMakeFlags = CommandParser.parseFlag3
                        (CommandParser.flag "debug" "Debug mode")
                        (CommandParser.flag "optimize" "Make the code smaller and faster")
                        (CommandParser.flagWithArg "output" "Output file path" Make.parseOutput)
    parseMakeArgs =
       CommandParser.parseArgList (CommandParser.arg "module")

runMake :: (String, [String]) -> (Maybe Bool, Maybe Bool, Maybe Make.Output) -> IO ()
runMake (fstModule, modules) (debug, optimize, output) = do
    configResult <- Gen.Generate.readConfig
    case configResult of
        Right (configLastModified, config) -> do
            Gen.Templates.writeGroupCustomizable Gen.Templates.Loader.Customizable "./src/app" "./elm-stuff/generated"
            Gen.Templates.writeGroup Gen.Templates.Loader.ToHidden "./elm-stuff/generated"

            generateResult <- Gen.Generate.generate config
            case generateResult of
                Right files -> do 
                    -- Write each file to disk
                    mapM_ (\file -> do
                              let fullPath = Gen.Generate.path file
                              Dir.createDirectoryIfMissing True (FP.takeDirectory fullPath)
                              TIO.writeFile fullPath (Text.pack $ Gen.Generate.contents file)
                          ) files
                    
                    Ext.Log.with [Ext.Log.ElmCompilerError] $ do
                      Make.run 
                          (fstModule : modules)
                            (Make.Flags
                                (fromMaybe False debug)
                                (fromMaybe False optimize)
                                output
                                Nothing
                                Nothing
                            )
                          
                Left err -> do
                    IO.hPutStrLn IO.stderr err
                    return ()
        Left _ -> do
            -- No generation config, so just run the compiler
            Ext.Log.with [Ext.Log.ElmCompilerError] $ do
                Make.run 
                    (fstModule : modules)
                      (Make.Flags
                          (fromMaybe False debug)
                          (fromMaybe False optimize)
                          output
                          Nothing
                          Nothing
                      )
            return ()


addGroup :: Maybe String
addGroup = Just "Add to your Elm app"

-- Add Page Command
addPage :: CommandParser.Command
addPage = CommandParser.command ["add", "page"] "Add a new page" addGroup parsePageArgs CommandParser.noFlag runPage
  where
    parsePageArgs =
       CommandParser.parseArg
            (CommandParser.arg "url") 

    runPage :: String -> () -> IO ()
    runPage url _ = do
        let name = urlToElmModuleName url
        configResult <- Gen.Generate.readConfigOrFail

        Gen.Templates.write "Page" (Config.elmSrc) name
        let urlText = Text.pack url

        -- Update config
        let updatedConfig = configResult {
            Config.configApp = Just $ case Config.configApp configResult of
                Nothing -> Config.AppConfig { 
                    Config.appPages = Map.singleton (Text.pack name) (Config.PageConfig urlText [] False)
                }
                Just appConfig -> appConfig {
                    Config.appPages = Map.insert (Text.pack name) (Config.PageConfig urlText [] False) (Config.appPages appConfig)
                }
        }
        BS.writeFile "elm.generate.json" (Aeson.encodePretty updatedConfig)
        
        -- putStrLn $ "Created new page: " ++ name

-- Add Store Command
addStore :: CommandParser.Command
addStore = CommandParser.command ["add", "store"] "Add a new store" addGroup CommandParser.elmModuleName CommandParser.noFlag runStore
  where

    runStore :: Elm.ModuleName.Raw -> () -> IO ()
    runStore modName _ = do
        configResult <- Gen.Generate.readConfigOrFail

        let storeName = Elm.ModuleName.toChars modName
        Gen.Templates.write "Store" Config.elmSrc storeName
           
        putStrLn $ "Created new store: " ++ storeName


-- Add Effect Command
addEffect :: CommandParser.Command
addEffect = CommandParser.command ["add", "effect"] "Add a new effect" addGroup CommandParser.elmModuleName CommandParser.noFlag runEffect
  where
   
    runEffect :: Elm.ModuleName.Raw -> () -> IO ()
    runEffect modName _ = do
        configResult <- Gen.Generate.readConfigOrFail
        let name = Elm.ModuleName.toChars modName

        Gen.Templates.write "Effect" Config.elmSrc name
        -- corresponding TS file for the effect
        Gen.Templates.writeTs "effect" Config.effectTsSrc name
         
        putStrLn $ "Created new effect: " ++ name


-- Add Listener Command
addListener :: CommandParser.Command
addListener = CommandParser.command ["add", "listener"] "Add a new listener" addGroup CommandParser.elmModuleName CommandParser.noFlag runListener
  where
   
    runListener :: Elm.ModuleName.Raw -> () -> IO ()
    runListener modName _ = do
        configResult <- Gen.Generate.readConfigOrFail
        let name = Elm.ModuleName.toChars modName

        Gen.Templates.write "Listen" Config.elmSrc name
        
        putStrLn $ "Created new listener: " ++ name



-- Add Docs Command
addDocs :: CommandParser.Command
addDocs = CommandParser.command ["add", "docs"] "Add docs site" addGroup CommandParser.noArg CommandParser.noFlag runDocs
  where

    runDocs :: () -> () -> IO ()
    runDocs _ _ = do
        configResult <- Gen.Generate.readConfigOrFail

        case Config.configDocs configResult of
            Just _ ->
                fail "Docs section already exists in config"
            Nothing -> do
                let newConfig = configResult { Config.configDocs = Just Config.defaultDocs }
                BS.writeFile "elm.generate.json" (Aeson.encodePretty newConfig)
                putStrLn "Added docs section to config"

-- Add Theme Command
addTheme :: CommandParser.Command
addTheme = CommandParser.command ["add", "theme"] "Add a theme" addGroup CommandParser.noArg CommandParser.noFlag runTheme
  where

    runTheme :: () -> () -> IO ()
    runTheme _ _ = do
        configResult <- Gen.Generate.readConfigOrFail

        case Config.configTheme configResult of
            Just _ ->
                fail "Theme section already exists in config"
            Nothing -> do
                let newConfig = configResult { Config.configTheme = Just Config.defaultTheme }
                BS.writeFile "elm.generate.json" (Aeson.encodePretty newConfig)
                putStrLn "Added theme section to config"

-- TYPES


data AddCommand
  = AddPage Terminal.Helpers.PageParams
  | AddStore Elm.ModuleName.Raw
  | AddEffect Elm.ModuleName.Raw
  | AddDocs
  | AddTheme 


exactly :: String -> Terminal.Parser ()
exactly str =
  Terminal.Parser
    { Terminal._singular = "type"
    , Terminal._plural = "types"
    , Terminal._parser = parseExact str
    , Terminal._suggest = \_ -> return [ str ]
    , Terminal._examples = \_ -> return [str]
    , Terminal._choices = Nothing
    }


parseExact :: String -> String -> Maybe ()
parseExact target found =
  if target == found then 
    Just ()
  else 
    Nothing





-- CUSTOMIZE COMMAND
customizeGroup :: Maybe String
customizeGroup = Just "Move an elm-dev-controlled file into your project"

customize :: CommandParser.Command
customize = CommandParser.command ["customize"] "Customize project components" customizeGroup (CommandParser.parseArg (CommandParser.arg "module"))  CommandParser.noFlag $ \moduleName () -> do
    -- Read config to get source directory
    configResult <- Gen.Generate.readConfigOrFail

    cwd <- Dir.getCurrentDirectory

    -- Convert moduleName to a file path
    let moduleFilePath = foldr (</>) "" $ words $ map (\c -> if c == '.' then ' ' else c) moduleName <.> "elm"

    -- Find matching customizable template
    let maybeTemplate = Data.List.find (\t -> 
            Gen.Templates.Loader.target t == Gen.Templates.Loader.Customizable &&
            moduleName == Gen.Templates.Loader.elmModuleName t)
            Gen.Templates.templates

    case maybeTemplate of
        Nothing -> do
            let customizableTemplates = List.sortBy (\a b -> compare (Gen.Templates.Loader.elmModuleName a) (Gen.Templates.Loader.elmModuleName b)) $ 
                                            filter (\t -> Gen.Templates.Loader.target t == Gen.Templates.Loader.Customizable) Gen.Templates.templates
            putStrLn $ "I wasn't able to find  " ++ Terminal.Colors.yellow moduleName
            putStrLn "Available customizable templates:\n"
            mapM_ (\t -> putStrLn $ "  " ++ Terminal.Colors.green (Gen.Templates.Loader.elmModuleName t)) customizableTemplates
            
        Just template -> do
            -- Write template to configSrc folder
            let contents = Data.Text.Encoding.decodeUtf8 (Gen.Templates.Loader.content template)
            let targetPath = cwd </> Config.elmSrc </> moduleFilePath
            let relativePath = Config.elmSrc </> moduleFilePath

            -- Check if file already exists
            targetExists <- Dir.doesFileExist targetPath
            when targetExists $ do
                putStrLn $ "\n  File already exists at " ++ Terminal.Colors.yellow relativePath
                putStrLn "  Are you already customizing this file?"
                putStrLn "  If you want it to be regenerated, delete it and run the command again."

            when (not targetExists) $ do
              -- Create the directory if it doesn't exist
              Dir.createDirectoryIfMissing True (FP.takeDirectory targetPath)
              TIO.writeFile targetPath contents
              
              -- Check and remove corresponding file in ToHidden directory
              let hiddenPath = cwd </> ".elm-generate" </> moduleFilePath
              hiddenExists <- Dir.doesFileExist hiddenPath
              when hiddenExists $ do
                  Dir.removeFile hiddenPath
              
              putStrLn ""
              putStrLn $ "  " ++ Terminal.Colors.green relativePath ++ " is now in your project!"
              putStrLn ""
              putStrLn "Customize away!"


reflow :: String -> P.Doc
reflow string =
  P.fillSep $ map P.text $ words string


-- URL to Elm Module Name conversion
urlToElmModuleName :: String -> String
urlToElmModuleName url = 
    let -- Remove everything after ?
        baseUrl = takeWhile (/= '?') url
        -- Split on / and filter out empty strings
        parts = filter (not . null) $ splitOn '/' baseUrl
        -- Filter out parts starting with : and capitalize
        validParts = map capitalize $ filter (not . isParam) parts
    in concat validParts
    where
        splitOn :: Char -> String -> [String]
        splitOn c = words . map (\x -> if x == c then ' ' else x)
        
        isParam :: String -> Bool
        isParam [] = False
        isParam (x:_) = x == ':'
        
        capitalize :: String -> String
        capitalize [] = []
        capitalize (x:xs) = Char.toUpper x : xs