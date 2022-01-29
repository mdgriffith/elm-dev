{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall #-}

module Watchtower.Project (contains, discover, decodeProject, Project (..), encodeProjectJson) where

import Control.Concurrent.MVar
import qualified Control.Monad as Monad
import qualified Data.ByteString as BS
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Ext.Common
import qualified File
import qualified Json.Decode
import Json.Encode ((==>))
import qualified Json.Encode
import qualified Json.String
import qualified System.Directory as Dir
import qualified System.Environment as Env
import qualified System.Exit
import System.FilePath as FP (joinPath, splitDirectories, takeDirectory, (</>))
import System.IO (hClose, hFlush, hPutStr, hPutStrLn, openTempFile, stderr, stdout)
import System.IO.Unsafe (unsafePerformIO)
import qualified System.Process
import qualified Text.Show.Unicode
import Prelude hiding (lookup)

{-
    A project is an instance of `elm.json` that lives in _root as well

    as any known Elm file that is the entrypoint to compile.

    Discover currently looks for an `elm-tooling.json` file: https://elm-tooling.github.io/elm-tooling-cli/spec/

    In order to discover the entrypoint to compile.

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

When elm.json is found, look for elm-tooling.  Parse elm-tooling for entrypoints.

otherwise, create a new project with no entrypoints.

-}
discover :: FilePath -> IO [Project]
discover root =
  searchProjectHelp [] root

shouldSkip :: FilePath -> Bool
shouldSkip path =
  if (List.isInfixOf "elm-stuff" path) || (List.isInfixOf "node_modules" path)
    then True
    else case path of
      '.' : _ ->
        True
      _ ->
        False

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

-- look for elm-tooling for entrypoints
createProject :: FilePath -> IO Project
createProject root =
  do
    maybeTooling <- readElmTooling (root </> "elm-tooling.json")
    case maybeTooling of
      Nothing ->
        pure (Project root [])
      Just tooling ->
        pure (Project root (_entries tooling))

readElmTooling :: FilePath -> IO (Maybe Tooling)
readElmTooling path =
  do
    exists <- Dir.doesFileExist path

    if exists
      then do
        byteString <- File.readUtf8 path
        let parsed = Json.Decode.fromByteString decodeElmTooling byteString
        case parsed of
          Left err ->
            pure Nothing
          Right tooling ->
            pure (Just tooling)
      else pure Nothing

data Tooling = Tooling
  { _entries :: [FilePath]
  }

decodeElmTooling :: Json.Decode.Decoder x Tooling
decodeElmTooling =
  Tooling <$> (Json.Decode.field "entrypoints" (Json.Decode.list filepath))

decodeProject :: Json.Decode.Decoder x Project
decodeProject =
  Project
    <$> (Json.Decode.field "root" filepath)
    <*> (Json.Decode.field "entrypoints" (Json.Decode.list filepath))

filepath :: Json.Decode.Decoder x FilePath
filepath =
  ( ( \str ->
        case str of
          [] ->
            str
          '.' : '/' : remain ->
            remain
          _ ->
            str
    )
      <$> (Json.String.toChars <$> Json.Decode.string)
  )

encodeProjectJson :: Project -> Json.Encode.Value
encodeProjectJson (Project elmJson entrypoints) =
  Json.Encode.object
    [ "root" ==> Json.Encode.string (Json.String.fromChars elmJson),
      "entrypoints"
        ==> Json.Encode.list
          (\point -> Json.Encode.string (Json.String.fromChars point))
          entrypoints
    ]