{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall #-}

module Ext.Dev.Project
  ( importersOf
  , defaultImports
  , lookupModulePath
  , lookupModuleName
  , getRoot
  , contains
  , discover, decodeProject, Project (..), encodeProjectJson, equal
  ) where


import Prelude hiding (lookup)
import qualified System.Directory as Dir
import System.FilePath as FP ((</>))
import Data.Function ((&))

import qualified Control.Monad as Monad
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T

import qualified Ext.Common

import qualified Json.Decode
import Json.Encode ((==>))
import qualified Json.Encode
import qualified Json.String

import qualified Elm.Outline
import qualified Elm.Details
import qualified Elm.ModuleName as ModuleName


defaultImports :: Set.Set ModuleName.Raw
defaultImports =
  Set.fromList
    [ "Platform.Sub"
    , "Platform.Cmd"
    , "Platform"
    , "Tuple"
    , "Char"
    , "String"
    , "Result"
    , "Maybe"
    , "List"
    , "Debug"
    , "Basics"
    ]


{-|
  ModuelName -> Path
-}
lookupModulePath :: Elm.Details.Details -> ModuleName.Raw -> Maybe FilePath
lookupModulePath details canModuleName =
  details
    & Elm.Details._locals
    & Map.lookup canModuleName
    & fmap Elm.Details._path


{-|
  Path -> ModuleName
-}
lookupModuleName :: Elm.Details.Details -> FilePath -> Maybe ModuleName.Raw
lookupModuleName details filepath =
  let 
      locals = Elm.Details._locals details
  in
  Map.foldrWithKey
    (\localModuleName localDetails found ->
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
  let 
      locals = Elm.Details._locals details
  in
  Map.foldrWithKey
    (\localModuleName localDetails found ->
        if List.elem targetModule (Elm.Details._deps localDetails) then 
            Set.insert localModuleName found
        else
            found
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
    _entrypoints :: [FilePath]
  }
  deriving (Show)


equal :: Project -> Project -> Bool
equal (Project root1 _) (Project root2 _) =
    root1 == root2

getRoot :: Project -> FilePath
getRoot (Project root _) =
    root

contains :: FilePath -> Project -> Bool
contains path (Project root entries) =
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
discover root =
  searchProjectHelp [] root

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

searchProjectHelp :: [Project] -> FilePath -> IO [Project]
searchProjectHelp projs root =
  if shouldSkip root
    then pure projs
    else do
      elmJsonExists <- Dir.doesFileExist (root </> "elm.json")
      newProjects <-
        if elmJsonExists
          then (\proj -> proj : projs) <$> createProject root
          else pure projs

      names <- Dir.listDirectory root
      let paths = map (root </>) names
      dirs <- Monad.filterM Dir.doesDirectoryExist paths
      Monad.foldM searchProjectHelp newProjects dirs



createProject :: FilePath -> IO Project
createProject root =
  do
    maybeElmMain <- findFirstFileNamed "Main.elm" root
    case maybeElmMain of
      Nothing ->
        pure (Project root [])
      Just main ->
        do
          pure (Project root [main])


findFirstFileNamed :: String -> FilePath -> IO (Maybe FilePath)
findFirstFileNamed named dir =
  if shouldSkip dir
    then pure Nothing
    else do
      fileExists <- Dir.doesFileExist (dir </> named)

      if fileExists
        then pure (Just (dir </> named))
        else
          do
            subdirs <- Dir.listDirectory dir
            let paths = map (dir </>) subdirs
            dirs <- Monad.filterM Dir.doesDirectoryExist paths
            Monad.foldM
               (\found subdir ->
                  case found of
                    Nothing ->
                      findFirstFileNamed named subdir
                    Just _ ->
                      pure found
               )
               Nothing
               dirs




decodeProject :: Json.Decode.Decoder x Project
decodeProject =
  Project
    <$> Json.Decode.field "root" filepath
    <*> Json.Decode.field "entrypoints" (Json.Decode.list filepath)

filepath :: Json.Decode.Decoder x FilePath
filepath =
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
encodeProjectJson (Project elmJson entrypoints) =
  Json.Encode.object
    [ "root" ==> Json.Encode.string (Json.String.fromChars elmJson),
      "entrypoints"
        ==> Json.Encode.list
          (\point -> Json.Encode.string (Json.String.fromChars point))
          entrypoints
    ]