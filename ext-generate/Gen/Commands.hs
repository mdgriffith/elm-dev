{-# LANGUAGE OverloadedStrings #-}
module Gen.Commands (initialize, make, add, customize) where

import qualified Terminal
import Terminal ((!), (?), (...))
import Data.Text (Text)
import qualified Text.PrettyPrint.ANSI.Leijen as P
import qualified Terminal.Helpers
import qualified Elm.ModuleName
import qualified Gen.Javascript
import qualified Gen.Config as Config
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString
import System.FilePath ((</>))
import Data.Text (pack)
import Data.Aeson (eitherDecodeStrict)
import qualified System.Directory (getCurrentDirectory)
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
import System.Directory (doesFileExist, removeFile)
import Control.Monad (when)
import qualified System.FilePath as FP


-- INIT COMMAND
initialize :: Terminal.Command
initialize =
  let
    summary =
      "Create a new Elm project"

    details =
      "The `init` command sets up a new Elm project with the recommended structure."

    example =
      reflow
        "Run this command in an empty directory to create a new Elm project."

    initFlags = Terminal.noFlags
    initArgs = Terminal.noArgs
  in
  Terminal.Command "init" (Terminal.Common summary) details example initArgs initFlags runInit


-- MAKE COMMAND
make :: Terminal.Command
make =
  let
    summary =
      "Build and compile your Elm project"

    details =
      "The `make` command compiles your Elm project."

    example =
      reflow
        "Run this command to compile your Elm project."

    makeFlags = Terminal.noFlags
    makeArgs = Terminal.noArgs
  in
  Terminal.Command "make" (Terminal.Common summary) details example makeArgs makeFlags runMake


-- ADD COMMAND
add :: Terminal.Command
add =
  let
    summary =
      "Add new components to your project"

    details =
      "The `add` command helps you add new pages, stores, effects, docs, or themes."

    example =
      reflow
        "Use this command with one of: page, store, effect, docs, theme"

    addFlags = Terminal.noFlags
    addArgs = Terminal.oneOf
      [ Terminal.require2 (\_ -> AddPage) (exactly "page") Terminal.Helpers.urlPattern
      , Terminal.require2 (\_ -> AddStore) (exactly "store") Terminal.Helpers.elmModule
      , Terminal.require2 (\_ -> AddEffect) (exactly "effect") Terminal.Helpers.elmModule
      , Terminal.require1 (\_ -> AddDocs) (exactly "docs")
      , Terminal.require1 (\_ -> AddTheme) (exactly "theme")
      ]
  in
  Terminal.Command "add" (Terminal.Common summary) details example addArgs addFlags runAdd

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
customize :: Terminal.Command
customize =
  let
    summary =
      "Customize project components"

    details =
      "The `customize` command allows you to customize components given an Elm module name."

    example =
      reflow
        "Run this command with an optional Elm module name (e.g., 'My.Module')"

    customizeFlags = Terminal.noFlags
    customizeArgs = Terminal.oneOrMoreWith Customize (Terminal.Helpers.elmModuleOrPath)
  in
  Terminal.Command "customize" (Terminal.Common summary) details example customizeArgs customizeFlags runCustomize


data CustomizeCommand
  = Customize Terminal.Helpers.ElmModuleOrFilePath [Terminal.Helpers.ElmModuleOrFilePath]


-- RUNNERS (These need to be implemented)
runInit :: () -> () -> IO ()
runInit () () = 
    do  defaultConfig <- return $ Config.Config
            { Config.configPackageManager = Just Config.Bun
            , Config.configApp = Nothing
            , Config.configAssets = Nothing
            , Config.configTheme = Nothing
            , Config.configGraphQL = Nothing
            , Config.configDocs = Nothing
            }
        
        BS.writeFile "elm.generate.json" (Aeson.encodePretty defaultConfig)
        putStrLn "Created elm.generate.json with default configuration"

runMake :: () -> () -> IO ()
runMake () () =
    do  putStrLn "Initializing project..."

runAdd :: AddCommand -> () -> IO ()
runAdd cmd () = do
    configResult <- BS.readFile "elm.generate.json" >>= \contents ->
        case eitherDecodeStrict (BS.toStrict contents) :: Either String Config.Config of
            Left err -> 
                fail $ "Failed to parse elm.generate.json: " ++ err
            Right config -> 
                return config

    case cmd of
        AddPage pageParams -> do
            -- Find the matching page template
            let maybeTemplate = Data.List.find (\t -> 
                    Gen.Templates.Loader.target t == Gen.Templates.Loader.OneOff && 
                    Gen.Templates.Loader.plugin t == "app" && 
                    Gen.Templates.Loader.filename t == "Page.elm") Gen.Templates.templates
            
            case maybeTemplate of
                Nothing -> 
                    fail "Could not find page template"
                Just template -> do
                    -- Get template contents
                    let contents = Data.Text.Encoding.decodeUtf8 (Gen.Templates.Loader.content template)
                    let url = Text.pack (Terminal.Helpers.url pageParams)
                    let fullModuleName = Text.pack ("Page." ++ Terminal.Helpers.elmModuleName pageParams)
                    
                    -- Do replacements
                    let pageName = Text.pack (Terminal.Helpers.elmModuleName pageParams)
                    let pageNameUnderscored = Text.replace "." "_" pageName
                    let newContents = contents
                            & Text.replace "{{name}}" pageName
                            & Text.replace "{{name_underscored}}" pageNameUnderscored
                    
                    -- Write to file
                    cwd <- System.Directory.getCurrentDirectory
                    let targetPath = cwd </> Text.unpack Config.src </> "Page" </> Text.unpack pageName <.> "elm"
                    TIO.writeFile targetPath newContents
                    
                    -- Update config
                    let updatedConfig = configResult {
                            Config.configApp = Just $ case Config.configApp configResult of
                                Nothing -> Config.AppConfig { 
                                    Config.appPages = Map.singleton url (Config.PageConfig fullModuleName [] False)
                                }
                                Just appConfig -> appConfig {
                                    Config.appPages = Map.insert url (Config.PageConfig fullModuleName [] False) (Config.appPages appConfig)
                                }
                        }
                    BS.writeFile "elm.generate.json" (Aeson.encodePretty updatedConfig)
                    
                    putStrLn $ "Created new page: " ++ Text.unpack pageName

        AddStore moduleName ->
            do
                -- Find the matching store template
                let maybeTemplate = Data.List.find (\t -> 
                        Gen.Templates.Loader.target t == Gen.Templates.Loader.OneOff && 
                        Gen.Templates.Loader.plugin t == "app" && 
                        Gen.Templates.Loader.filename t == "Store.elm") Gen.Templates.templates
                
                case maybeTemplate of
                    Nothing -> 
                        fail "Could not find store template"
                    Just template -> do
                        -- Get template contents
                        let contents = Data.Text.Encoding.decodeUtf8 (Gen.Templates.Loader.content template)
                        
                        -- Do replacement
                        let storeName = Text.pack (Elm.ModuleName.toChars moduleName)
                        let newContents = contents & Text.replace "{{name}}" storeName
                        
                        -- Write to file
                        cwd <- System.Directory.getCurrentDirectory
                        let targetPath = cwd </> Text.unpack Config.src </> "Store" </> Text.unpack storeName <.> "elm"
                        TIO.writeFile targetPath newContents
                        
                        putStrLn $ "Created new store: " ++ Text.unpack storeName

        AddEffect moduleName ->
            do
                -- Find the matching effect template
                let maybeTemplate = Data.List.find (\t -> 
                        Gen.Templates.Loader.target t == Gen.Templates.Loader.OneOff && 
                        Gen.Templates.Loader.plugin t == "app" && 
                        Gen.Templates.Loader.filename t == "Effect.elm") Gen.Templates.templates
                
                case maybeTemplate of
                    Nothing -> 
                        fail "Could not find effect template"
                    Just template -> do
                        -- Get template contents
                        let contents = Data.Text.Encoding.decodeUtf8 (Gen.Templates.Loader.content template)
                        
                        -- Do replacement
                        let effectName = Text.pack (Elm.ModuleName.toChars moduleName)
                        let newContents = contents & Text.replace "{{name}}" effectName
                        
                        -- Write to file
                        cwd <- System.Directory.getCurrentDirectory
                        let targetPath = cwd </> Text.unpack Config.src </> "Effect" </> Text.unpack effectName <.> "elm"
                        TIO.writeFile targetPath newContents
                        
                        putStrLn $ "Created new effect: " ++ Text.unpack effectName

        AddDocs ->
            case Config.configDocs configResult of
                Just _ ->
                    fail "Docs section already exists in config"
                Nothing -> do
                    let newConfig = configResult { Config.configDocs = Just Config.defaultDocs }
                    -- Config.writeConfig newConfig
                    BS.writeFile "elm.generate.json" (Aeson.encodePretty newConfig)
                    putStrLn "Added docs section to config"

        AddTheme ->
            case Config.configTheme configResult of
                Just _ ->
                    fail "Theme section already exists in config"

                Nothing -> do
                    let newConfig = configResult { Config.configTheme = Just Config.defaultTheme }
                    -- Config.writeConfig newConfig
                    BS.writeFile "elm.generate.json" (Aeson.encodePretty newConfig)
                    putStrLn "Added theme section to config"

runCustomize :: CustomizeCommand -> () -> IO ()
runCustomize (Customize firstModule restModules) () = do
    -- Read config to get source directory
    configResult <- BS.readFile "elm.generate.json" >>= \contents ->
        case eitherDecodeStrict (BS.toStrict contents) :: Either String Config.Config of
            Left err -> 
                fail $ "Failed to parse elm.generate.json: " ++ err
            Right config -> 
                return config

    let modules = firstModule : restModules
    cwd <- System.Directory.getCurrentDirectory

    -- Process each module
    mapM_ (\moduleOrPath -> do
        -- Convert moduleOrPath to a file path
        let moduleFilePath = case moduleOrPath of
                Terminal.Helpers.ElmModule moduleName ->
                    foldr (</>) "" $ words $ map (\c -> if c == '.' then ' ' else c) (Elm.ModuleName.toChars moduleName)  <.> "elm"
                    
                Terminal.Helpers.FilePath path ->
                    path

        -- Find matching customizable template
        let maybeTemplate = Data.List.find (\t -> 
                Gen.Templates.Loader.target t == Gen.Templates.Loader.Customizable &&
                Text.pack moduleFilePath == Text.pack (Gen.Templates.Loader.filename t)) 
                Gen.Templates.templates

        case maybeTemplate of
            Nothing -> 
                putStrLn $ "No customizable : " ++ show moduleOrPath
            Just template -> do
                -- Write template to configSrc folder
                let contents = Data.Text.Encoding.decodeUtf8 (Gen.Templates.Loader.content template)
                let targetPath = cwd </> Text.unpack Config.src </> moduleFilePath
                TIO.writeFile targetPath contents
                
                -- Check and remove corresponding file in ToHidden directory
                let hiddenPath = cwd </> ".elm-generate" </> moduleFilePath
                hiddenExists <- System.Directory.doesFileExist hiddenPath
                when hiddenExists $ do
                    System.Directory.removeFile hiddenPath
                    putStrLn $ "Removed hidden template: " ++ hiddenPath
                
                putStrLn $ "Customized template written to: " ++ targetPath
        ) modules


reflow :: String -> P.Doc
reflow string =
  P.fillSep $ map P.text $ words string


