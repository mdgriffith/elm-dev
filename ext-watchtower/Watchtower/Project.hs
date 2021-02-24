{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Watchtower.Project (contains, discover, decodeProject, Project(..)) where

import qualified Data.List as List
import qualified System.Directory as Dir
import qualified System.Environment as Env
import System.FilePath as FP ((</>), joinPath, splitDirectories, takeDirectory)
import System.IO (hFlush, hPutStr, hPutStrLn, stderr, stdout, hClose, openTempFile)
import Prelude hiding (lookup)


import qualified Data.ByteString as BS
import qualified Data.Map as Map
import System.IO.Unsafe (unsafePerformIO)
import Control.Concurrent.MVar
import qualified Control.Monad as Monad
import qualified System.Environment as Env
import qualified System.Process
import qualified Text.Show.Unicode
import qualified Data.Text as T
import Data.Text (Text)

import qualified System.Exit
import qualified Json.Decode
import qualified Json.String

import qualified File
import qualified Ext.Common

{-
    A project is an instance of `elm.json` that lives in _root as well

    as any known Elm file that is the entrypoint to compile.


    Discover currently looks for an `elm-tooling.json` file: https://elm-tooling.github.io/elm-tooling-cli/spec/

    In order to discover the entrypoint to compile.


-}
data Project =
    Project
        { _root :: FilePath
        , _entrypoints :: [ FilePath]
        }
        deriving (Show)




{-|
-}
contains :: FilePath -> Project -> Bool
contains path (Project root entries) =
    List.isPrefixOf root path


{- Recursively find files named elm.json.

Skip node_modules, elm_stuff and anythign that starts with '.'
    (I wonder if we can just use gitignore to know what to skip)

When elm.json is found, look for elm-tooling.  Parse elm-tooling for entrypoints.

otherwise, create a new project with no entrypoints.


-}
discover :: FilePath -> IO [ Project ]
discover root =
    searchProjectHelp [] root


shouldSkip :: FilePath -> Bool
shouldSkip path =
    if path == "elm-stuff" || path == "node_modules" then
        True
    else
        case path of
            '.':_ ->
                True
            _ ->
                False


searchProjectHelp :: [Project] -> FilePath ->  IO [ Project ]
searchProjectHelp projs root =
    if shouldSkip root then
        pure projs
    else
        do
            elmJsonExists <- Dir.doesFileExist (root </> "elm.json")
            newProjects <-
                if elmJsonExists then
                    (\proj -> proj : projs) <$> createProject root
                else
                    pure projs

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
        if exists then
            do
                byteString <- File.readUtf8 path
                let parsed = Json.Decode.fromByteString decodeElmTooling byteString
                case parsed of
                    Left err ->
                        pure Nothing
                    Right tooling ->
                        pure (Just tooling)

        else
            pure Nothing






data Tooling =
    Tooling
        { _entries :: [ FilePath ]

        }

decodeElmTooling :: Json.Decode.Decoder x Tooling
decodeElmTooling =
    Tooling <$>  (Json.Decode.field "entrypoints" (Json.Decode.list filepath))


decodeProject :: Json.Decode.Decoder x Project
decodeProject =
    Project
        <$> (Json.Decode.field "root" filepath)
        <*> (Json.Decode.field "entrypoints" (Json.Decode.list filepath))


filepath :: Json.Decode.Decoder x FilePath
filepath =
    (Json.String.toChars <$> Json.Decode.string)