{-# LANGUAGE OverloadedStrings #-}

module Gen.Commands (initialize, addPage, addStore, addEffect, addDocs, addTheme, addUi, addListener, customize) where

import qualified CommandParser
import Control.Monad (forM, when)
import Data.Aeson (Value, eitherDecodeStrict, object, (.=))
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Lazy as BS
import qualified Data.Char as Char
import Data.Function ((&))
import qualified Data.List (find)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Maybe as Maybe
import qualified Data.Name as Name
import Data.Text (Text, pack)
import qualified Data.Text as Text
import qualified Data.Text.Encoding
import qualified Data.Text.IO as TIO
import qualified Elm.ModuleName
import qualified Ext.Log
import qualified Gen.Commands.Init
import qualified Gen.Config
import qualified Gen.Config as Config
import qualified Gen.Generate
import qualified Gen.Javascript
import qualified Gen.Templates
import qualified Gen.Templates.Loader
import qualified Make
import qualified Reporting.Exit as Exit
import qualified System.Directory as Dir (createDirectoryIfMissing, doesFileExist, getCurrentDirectory, removeFile)
import System.FilePath ((<.>), (</>))
import qualified System.FilePath as FP
import qualified System.IO as IO
import Terminal ((!), (...), (?))
import qualified Terminal
import qualified Terminal.Colors
import qualified Terminal.Helpers
import qualified Text.PrettyPrint.ANSI.Leijen as P

-- Flag types
-- data InitFlags = InitFlags
data MakeFlags = MakeFlags

data AddFlags = AddFlags

data CustomizeFlags = CustomizeFlags

-- INIT COMMAND
initialize :: CommandParser.Command
initialize =
  CommandParser.command
    ["init"]
    "Create a new Elm project"
    Nothing
    Gen.Commands.Init.args
    Gen.Commands.Init.flags
    (\_ _ -> do
      cwd <- Dir.getCurrentDirectory
      Gen.Commands.Init.run cwd
    )

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
      cwd <- Dir.getCurrentDirectory
      configResult <- Gen.Generate.readConfigOrFail cwd

      Gen.Templates.write "Page" Config.elmSrc name
      let urlText = Text.pack url

      -- Update config
      let updatedConfig =
            configResult
              { Config.configPages = Just $ case Config.configPages configResult of
                  Nothing -> Map.singleton (Text.pack name) (Config.PageConfig urlText [] False)
                  Just pagesMap -> Map.insert (Text.pack name) (Config.PageConfig urlText [] False) pagesMap
              }
      BS.writeFile "elm.dev.json" (Aeson.encodePretty updatedConfig)

-- putStrLn $ "Created new page: " ++ name

-- Add Store Command
addStore :: CommandParser.Command
addStore = CommandParser.command ["add", "store"] "Add a new store" addGroup CommandParser.elmModuleName CommandParser.noFlag runStore
  where
    runStore :: Elm.ModuleName.Raw -> () -> IO ()
    runStore modName _ = do
      cwd <- Dir.getCurrentDirectory
      configResult <- Gen.Generate.readConfigOrFail cwd

      let storeName = Elm.ModuleName.toChars modName
      Gen.Templates.write "Store" Config.elmSrc storeName

      putStrLn $ "Created new store: " ++ storeName

-- Add Effect Command
addEffect :: CommandParser.Command
addEffect = CommandParser.command ["add", "effect"] "Add a new effect" addGroup CommandParser.elmModuleName CommandParser.noFlag runEffect
  where
    runEffect :: Elm.ModuleName.Raw -> () -> IO ()
    runEffect modName _ = do
      cwd <- Dir.getCurrentDirectory
      configResult <- Gen.Generate.readConfigOrFail cwd
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
      cwd <- Dir.getCurrentDirectory
      configResult <- Gen.Generate.readConfigOrFail cwd
      let name = Elm.ModuleName.toChars modName

      Gen.Templates.write "Listen" Config.elmSrc name

      putStrLn $ "Created new listener: " ++ name

-- Add UI Command
addUi :: CommandParser.Command
addUi = CommandParser.command ["add", "ui"] "Add built-in UI modules" addGroup CommandParser.noArg CommandParser.noFlag runUi
  where
    runUi :: () -> () -> IO ()
    runUi _ _ = do
      cwd <- Dir.getCurrentDirectory
      configResult <- Gen.Generate.readConfigOrFail cwd

      case Config.configTheme configResult of
        Nothing -> do
          let newConfig = configResult {Config.configTheme = Just defaultTheme}
          BS.writeFile "elm.dev.json" (Aeson.encodePretty newConfig)
          putStrLn "Added theme section to config"
        Just _ ->
          return ()

      let uiTemplates =
            List.sortBy
              (\a b -> compare (Gen.Templates.Loader.elmModuleName a) (Gen.Templates.Loader.elmModuleName b))
              ( filter
                  (\t ->
                      Gen.Templates.Loader.target t == Gen.Templates.Loader.Customizable
                        && Gen.Templates.Loader.plugin t == "theme"
                        && isUiPath (Gen.Templates.Loader.dir t)
                  )
                  Gen.Templates.templates
              )

      if List.null uiTemplates
        then fail "Could not find built-in UI templates"
        else do
          results <- forM uiTemplates $ \template -> do
            let relativePath = Config.elmSrc </> Gen.Templates.Loader.dir template </> Gen.Templates.Loader.filename template
            let targetPath = cwd </> relativePath
            targetExists <- Dir.doesFileExist targetPath

            if targetExists
              then return ([], [relativePath])
              else do
                Dir.createDirectoryIfMissing True (FP.takeDirectory targetPath)
                TIO.writeFile targetPath (Data.Text.Encoding.decodeUtf8 (Gen.Templates.Loader.content template))

                let hiddenPath = cwd </> "elm-stuff/generated" </> Gen.Templates.Loader.dir template </> Gen.Templates.Loader.filename template
                hiddenExists <- Dir.doesFileExist hiddenPath
                when hiddenExists (Dir.removeFile hiddenPath)

                return ([relativePath], [])

          let created = List.concatMap fst results
          let skipped = List.concatMap snd results

          putStrLn ""
          mapM_ (\path -> putStrLn $ "  + " ++ Terminal.Colors.green path) created
          mapM_ (\path -> putStrLn $ "  = " ++ Terminal.Colors.yellow path ++ " (already exists)") skipped
          putStrLn ""
          putStrLn $ "Added " ++ show (length created) ++ " UI module(s)."

          codegenResult <- Gen.Generate.run cwd
          case codegenResult of
            Left err -> do
              putStrLn ""
              putStrLn "UI modules were added, but code generation failed:"
              putStrLn err
              fail "Unable to regenerate generated modules after `add ui`."

            Right () -> do
              putStrLn "Regenerated generated modules."

    isUiPath :: String -> Bool
    isUiPath dirPath =
      dirPath == "Ui" || List.isPrefixOf "Ui/" dirPath

-- Add Docs Command
addDocs :: CommandParser.Command
addDocs = CommandParser.command ["add", "docs"] "Add docs site" addGroup CommandParser.noArg CommandParser.noFlag runDocs
  where
    runDocs :: () -> () -> IO ()
    runDocs _ _ = do
      cwd <- Dir.getCurrentDirectory
      configResult <- Gen.Generate.readConfigOrFail cwd

      case Config.configDocs configResult of
        Just _ ->
          fail "Docs section already exists in config"
        Nothing -> do
          let newConfig = configResult {Config.configDocs = Just Config.defaultDocs}
          BS.writeFile "elm.dev.json" (Aeson.encodePretty newConfig)
          putStrLn "Added docs section to config"

-- Add Theme Command
addTheme :: CommandParser.Command
addTheme = CommandParser.command ["add", "theme"] "Add a theme" addGroup CommandParser.noArg CommandParser.noFlag runTheme
  where
    runTheme :: () -> () -> IO ()
    runTheme _ _ = do
      cwd <- Dir.getCurrentDirectory
      configResult <- Gen.Generate.readConfigOrFail cwd

      case Config.configTheme configResult of
        Just _ ->
          fail "Theme section already exists in config"
        Nothing -> do
          let newConfig = configResult {Config.configTheme = Just defaultTheme}
          BS.writeFile "elm.dev.json" (Aeson.encodePretty newConfig)
          putStrLn "Added theme section to config"

defaultTheme :: Value
defaultTheme =
  object
    [ "target" .= ("elm-ui" :: Text),
      "scale" .= (4 :: Int),
      "borderWidthScale" .= (1 :: Int),
      "colors"
        .= object
          [ "neutral" .= object ["swatchFrom" .= ("#6B7280" :: Text)],
            "brand" .= object ["swatchFrom" .= ("#2563EB" :: Text)],
            "white" .= ("#FFFFFF" :: Text),
            "black" .= ("#111111" :: Text)
          ],
      "colorRoles"
        .= object
          [ "text"
              .= object
                [ "default" .= ("neutral20" :: Text),
                  "muted" .= ("neutral40" :: Text),
                  "onBrand" .= ("white50" :: Text),
                  "@dark"
                    .= object
                      [ "default" .= ("neutral90" :: Text),
                        "muted" .= ("neutral70" :: Text),
                        "onBrand" .= ("white95" :: Text)
                      ]
                ],
            "background"
              .= object
                [ "canvas" .= ("white50" :: Text),
                  "surface" .= ("neutral95" :: Text),
                  "primary" .= ("brand40" :: Text),
                  "@dark"
                    .= object
                      [ "canvas" .= ("neutral10" :: Text),
                        "surface" .= ("neutral20" :: Text),
                        "primary" .= ("brand80" :: Text)
                      ]
                ],
            "border"
              .= object
                [ "default" .= ("neutral80" :: Text),
                  "focus" .= ("brand50" :: Text),
                  "@dark"
                    .= object
                      [ "default" .= ("neutral30" :: Text),
                        "focus" .= ("brand80" :: Text)
                      ]
                ]
          ],
      "typography"
        .= object
          [ "families"
              .= object
                [ "uiSans" .= [("Inter" :: Text), ("sans-serif" :: Text)],
                  "serifDisplay" .= [("EB Garamond" :: Text), ("serif" :: Text)]
                ],
            "instances"
              .= object
                [ "body"
                    .= object
                      [ "family" .= ("uiSans" :: Text),
                        "size" .= (16 :: Int),
                        "weight" .= (400 :: Int),
                        "lineHeight" .= (1.5 :: Double)
                      ],
                  "title"
                    .= object
                      [ "family" .= ("serifDisplay" :: Text),
                        "size" .= (32 :: Int),
                        "weight" .= (700 :: Int),
                        "lineHeight" .= (1.2 :: Double)
                      ]
                ]
          ],
      "borders"
        .= object
          [ "radius"
              .= object
                [ "sm" .= (4 :: Int),
                  "md" .= (8 :: Int)
                ]
          ]
    ]

-- TYPES

data AddCommand
  = AddPage Terminal.Helpers.PageParams
  | AddStore Elm.ModuleName.Raw
  | AddEffect Elm.ModuleName.Raw
  | AddUi
  | AddDocs
  | AddTheme

exactly :: String -> Terminal.Parser ()
exactly str =
  Terminal.Parser
    { Terminal._singular = "type",
      Terminal._plural = "types",
      Terminal._parser = parseExact str,
      Terminal._suggest = \_ -> return [str],
      Terminal._examples = \_ -> return [str],
      Terminal._choices = Nothing
    }

parseExact :: String -> String -> Maybe ()
parseExact target found =
  if target == found
    then Just ()
    else Nothing

-- CUSTOMIZE COMMAND
customizeGroup :: Maybe String
customizeGroup = Just "Move an elm-dev-generated file into your project"

customize :: CommandParser.Command
customize = CommandParser.command ["customize"] "Customize project components" customizeGroup (CommandParser.parseOptionalArg (CommandParser.arg "module")) customizeFlags $ \maybeModuleName flags -> do
  case (flags, maybeModuleName) of
    (True, _) -> printCustomizableTemplates
    (_, Nothing) -> printCustomizableTemplates
    (_, Just moduleName) -> customizeTemplate moduleName

customizeFlags :: CommandParser.ParsedArgs -> Either String (Bool, CommandParser.ParsedArgs)
customizeFlags parsed =
  case CommandParser.parseFlag CommandParser.helpFlag parsed of
    Left err -> Left err
    Right (maybeHelp, parsedRest) -> Right (Maybe.fromMaybe False maybeHelp, parsedRest)

customizeTemplate :: String -> IO ()
customizeTemplate moduleName = do
  -- Read config to get source directory
  cwd <- Dir.getCurrentDirectory
  configResult <- Gen.Generate.readConfigOrFail cwd

  -- Convert moduleName to a file path
  let moduleFilePath = foldr (</>) "" $ words $ map (\c -> if c == '.' then ' ' else c) moduleName <.> "elm"

  -- Find matching customizable template
  let maybeTemplate =
        Data.List.find
          ( \t ->
              Gen.Templates.Loader.target t == Gen.Templates.Loader.Customizable
                && moduleName == Gen.Templates.Loader.elmModuleName t
          )
          Gen.Templates.templates

  case maybeTemplate of
    Nothing -> do
      putStrLn $ "I wasn't able to find  " ++ Terminal.Colors.yellow moduleName
      printCustomizableTemplates
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
        let hiddenPath = cwd </> "elm-stuff/generated" </> moduleFilePath
        hiddenExists <- Dir.doesFileExist hiddenPath
        when hiddenExists $ do
          Dir.removeFile hiddenPath

        putStrLn ""
        putStrLn $ "  " ++ Terminal.Colors.green relativePath ++ " is now in your project!"
        putStrLn ""
        putStrLn "Customize away!"

printCustomizableTemplates :: IO ()
printCustomizableTemplates = do
  let customizableTemplates =
        List.sortBy (\a b -> compare (Gen.Templates.Loader.elmModuleName a) (Gen.Templates.Loader.elmModuleName b)) $
          filter (\t -> Gen.Templates.Loader.target t == Gen.Templates.Loader.Customizable) Gen.Templates.templates
  putStrLn "Available customizable templates:\n"
  mapM_ (\t -> putStrLn $ "  " ++ Terminal.Colors.green (Gen.Templates.Loader.elmModuleName t)) customizableTemplates

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
    isParam (x : _) = x == ':'

    capitalize :: String -> String
    capitalize [] = []
    capitalize (x : xs) = Char.toUpper x : xs
