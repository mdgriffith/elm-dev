{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Ext.Dev.Project
  ( importersOf,
    defaultImports,
    lookupModulePath,
    lookupModuleName,
    getRoot,
    contains,
    discover,
    decodeProject,
    Project (..),
    encodeProjectJson,
    equal,
  )
where

import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import Data.Function ((&))
import qualified Data.List as List
import qualified Data.NonEmptyList as NE
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Elm.Details
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Outline
import qualified Elm.Package
import qualified Ext.Log
import qualified Json.Decode
import Json.Encode ((==>))
import qualified Json.Encode
import qualified Json.String
import qualified Reporting.Exit as Exit
import qualified System.Directory as Dir
import System.FilePath as FP ((</>))
import Prelude hiding (lookup)
import qualified StandaloneInstances

defaultImports :: Set.Set ModuleName.Raw
defaultImports =
  Set.fromList
    [ "Platform.Sub",
      "Platform.Cmd",
      "Platform",
      "Tuple",
      "Char",
      "String",
      "Result",
      "Maybe",
      "List",
      "Debug",
      "Basics"
    ]

lookupModulePath :: Elm.Details.Details -> ModuleName.Raw -> Maybe FilePath
lookupModulePath details canModuleName =
  details
    & Elm.Details._locals
    & Map.lookup canModuleName
    & fmap Elm.Details._path

lookupModuleName :: Elm.Details.Details -> FilePath -> Maybe ModuleName.Raw
lookupModuleName details filepath =
  let locals = Elm.Details._locals details
   in Map.foldrWithKey
        ( \localModuleName localDetails found ->
            case found of
              Just _ ->
                found
              Nothing ->
                if Elm.Details._path localDetails == filepath
                  then Just localModuleName
                  else Nothing
        )
        Nothing
        locals

importersOf :: Elm.Details.Details -> ModuleName.Raw -> Set.Set ModuleName.Raw
importersOf details targetModule =
  let locals = Elm.Details._locals details
   in Map.foldrWithKey
        ( \localModuleName localDetails found ->
            if List.elem targetModule (Elm.Details._deps localDetails)
              then Set.insert localModuleName found
              else found
        )
        Set.empty
        locals

{-
    A project is an instance of `elm.json` that lives in _root as well

    as any known Elm file that is the entrypoint to compile.

Entrypoints:

    Based on elm-tooling:
        Discover currently looks for an `elm-tooling.json` file: https://elm-tooling.github.io/elm-tooling-cli/spec/

        And take the entrypoints listed there.

    Based on elm.json and smart searching. (not currently implemented)
        1. require that elm.json is present
        2. gather all source directories

        Applications ->
            - Find all .elm files in all source files.
            - Run compilation with all of them listed at once (does this work with a huge number of files?)
            - Crawl the dependency graph and take all files that are imported by no one.

        Packages ->
            - Add all exposed modules
-}
data Project = Project
  { _root :: FilePath,
    _projectRoot :: FilePath,
    _entrypoints :: NE.List FilePath,
    _srcDirs :: [FilePath]
  }
  deriving (Show)



equal :: Project -> Project -> Bool
equal (Project root1 _ _ _) (Project root2 _ _ _) =
  root1 == root2

getRoot :: Project -> FilePath
getRoot (Project root _ _ _) =
  root

contains :: FilePath -> Project -> Bool
contains path (Project root _ _ _) =
  root `List.isPrefixOf` path

{- Recursively find files named elm.json.

Skip node_modules, elm_stuff and anythign that starts with '.'
    (I wonder if we can just use gitignore to know what to skip)

When elm.json is found.

Ultimately we want to:
    read the elm.json

    If it's an app, look in the source directories for a `Main.elm`
    Is there a better way to determine the endpoints?

    If it's a package, add all exposed files.

-}
discover :: FilePath -> IO [Project]
discover projectBase =
  searchProjectHelp projectBase [] projectBase

shouldSkip :: FilePath -> Bool
shouldSkip path =
  List.isInfixOf "elm-stuff" path
    || List.isInfixOf "node_modules" path
    || ( case path of
           '.' : _ ->
             True
           _ ->
             False
       )

listDirectoriesSafe :: FilePath -> IO (Either Exception.SomeException [FilePath])
listDirectoriesSafe path = Exception.try $ do
  isDir <- Dir.doesDirectoryExist path
  if isDir
    then Dir.listDirectory path
    else return []

searchProjectHelp :: FilePath -> [Project] -> FilePath -> IO [Project]
searchProjectHelp projectRoot projs root = do
  if shouldSkip root
    then pure projs
    else do
      elmJsonExists <- Dir.doesFileExist (root </> "elm.json")
      newProjects <-
        if elmJsonExists
          then do
            maybeProject <- captureProjectIfEntrypoints projectRoot root
            case maybeProject of
              Nothing -> pure projs
              Just project -> pure (project : projs)
          else pure projs

      dirResult <- listDirectoriesSafe root
      case dirResult of
        Right dirNames -> do
          let paths = map (root </>) dirNames
          dirs <- Monad.filterM Dir.doesDirectoryExist paths
          Monad.foldM (searchProjectHelp projectRoot) newProjects dirs
        Left err -> do
          _ <- Ext.Log.log Ext.Log.Live ("Error " <> show err)
          pure newProjects

{-

 If this is a package, we use all the exposed modules as entrypoints.

 Otherwise, if this is an application, we look for a `Main.elm`

-}
captureProjectIfEntrypoints :: FilePath -> FilePath -> IO (Maybe Project)
captureProjectIfEntrypoints projectRoot elmJsonRoot = do
  outlineResult <- Elm.Outline.read elmJsonRoot
  case outlineResult of
    Right (Elm.Outline.App app) -> do
      Ext.Log.log Ext.Log.Live ("Found App: " <> elmJsonRoot)
      maybeElmMain <- findFirstFileNamed "Main.elm" elmJsonRoot
      case maybeElmMain of
        Nothing ->
          pure Nothing
        Just main -> do
          let srcDirsList = NE.toList (Elm.Outline._app_source_dirs app)
          let absoluteSrcDirsList = map (Elm.Outline.toAbsolute elmJsonRoot) srcDirsList
          pure (Just (Project elmJsonRoot projectRoot (NE.List main []) absoluteSrcDirsList))
    Right (Elm.Outline.Pkg pkg) -> do
      Ext.Log.log Ext.Log.Live ("Found package: " <> Elm.Package.toChars (Elm.Outline._pkg_name pkg) <> " at " <> elmJsonRoot)
      case Elm.Outline._pkg_exposed pkg of
        Elm.Outline.ExposedList rawModNameList ->
          let paths = rawModuleNameToPackagePath elmJsonRoot <$> rawModNameList
              srcDirs = [elmJsonRoot </> "src"]
          in case paths of
               [] -> pure Nothing
               (x:xs) -> pure (Just (Project elmJsonRoot projectRoot (NE.List x xs) srcDirs))
        Elm.Outline.ExposedDict dict ->
          let paths = concatMap (\(_, modList) -> rawModuleNameToPackagePath elmJsonRoot <$> modList) dict
              srcDirs = [elmJsonRoot </> "src"]
          in case paths of
               [] -> pure Nothing
               (x:xs) -> pure (Just (Project elmJsonRoot projectRoot (NE.List x xs) srcDirs))
    Left err -> do
      Ext.Log.log
          Ext.Log.Live
          ("Skipping: " <> elmJsonRoot <> " Elm Outline Error: " <> Exit.toString (Exit.toOutlineReport err)
          )
      pure Nothing


rawModuleNameToPackagePath :: FilePath -> ModuleName.Raw -> FilePath
rawModuleNameToPackagePath root modul =
  root </> "src" </> (ModuleName.toFilePath modul <> ".elm")

findFirstFileNamed :: String -> FilePath -> IO (Maybe FilePath)
findFirstFileNamed named dir =
  if shouldSkip dir
    then pure Nothing
    else do
      fileExists <- Dir.doesFileExist (dir </> named)

      if fileExists
        then pure (Just (dir </> named))
        else do
          subdirs <- Dir.listDirectory dir
          let paths = map (dir </>) subdirs
          dirs <- Monad.filterM Dir.doesDirectoryExist paths
          Monad.foldM
            ( \found subdir ->
                case found of
                  Nothing ->
                    findFirstFileNamed named subdir
                  Just _ ->
                    pure found
            )
            Nothing
            dirs

decodeProject :: Json.Decode.Decoder String Project
decodeProject =
  Project
    <$> Json.Decode.field "root" decodeFilePath
    <*> Json.Decode.field "projectRoot" decodeFilePath
    <*> Json.Decode.field "entrypoints" (Json.Decode.nonEmptyList decodeFilePath "No entrypoints")
    <*> Json.Decode.field "srcDirs" (Json.Decode.list decodeFilePath)

decodeFilePath :: Json.Decode.Decoder x FilePath
decodeFilePath =
  ( \str ->
      case str of
        [] ->
          str
        '.' : '/' : remain ->
          remain
        _ ->
          str
  )
    <$> (Json.String.toChars <$> Json.Decode.string)

encodeProjectJson :: Project -> Json.Encode.Value
encodeProjectJson (Project elmJson projectRoot entrypoints srcDirs) =
  Json.Encode.object
    [ "root" ==> Json.Encode.string (Json.String.fromChars elmJson),
      "projectRoot" ==> Json.Encode.string (Json.String.fromChars projectRoot),
      "entrypoints"
        ==> Json.Encode.list
          (Json.Encode.string . Json.String.fromChars)
          (NE.toList entrypoints),
      "srcDirs"
        ==> Json.Encode.list
          (Json.Encode.string . Json.String.fromChars)
          srcDirs
    ]
