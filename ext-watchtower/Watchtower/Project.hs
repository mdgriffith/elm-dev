{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall #-}

module Watchtower.Project (contains, discover, decodeProject, Project (..), encodeProjectJson) where

import qualified Control.Monad as Monad
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Elm.Outline
import qualified File
import qualified Json.Decode
import Json.Encode ((==>))
import qualified Json.Encode
import qualified Json.String
import qualified System.Directory as Dir
import System.FilePath as FP ((</>))
import Prelude hiding (lookup)

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

contains :: FilePath -> Project -> Bool
contains path (Project root entries) =
  List.isPrefixOf root path

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




{--}
createProject :: FilePath -> IO Project
createProject root =
  do
    elmMainExists <- Dir.doesFileExist (root </> "src" </> "Main.elm")
    if elmMainExists
      then pure (Project root [(root </> "src" </> "Main.elm")])
      else pure (Project root [])
     

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