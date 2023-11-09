{-# LANGUAGE OverloadedStrings #-}

module Ext.Dev.EntryPoints
    ( entrypoints
    , encode
    , EntryPoint
    )
    where


import qualified Data.Map as Map
import qualified Data.Name as Name
import qualified Data.Maybe as Maybe
import qualified Elm.Docs
import qualified Reporting.Exit as Exit
import qualified Reporting.Warning as Warning
import qualified Data.NonEmptyList as NonEmpty
import qualified AST.Source as Src

import qualified Json.Encode
import Json.Encode ((==>))

import qualified Ext.CompileProxy
import qualified Ext.Dev.Docs
import qualified Ext.Dev.Warnings
import qualified Elm.Details
import qualified Elm.ModuleName as ModuleName



{- Approach 



    Ext.CompileProxy.loadProject root 
        -> Details.Details 
        -> locals ->

            Local
                { _path :: FilePath
                , _time :: File.Time
                , _deps :: [ModuleName.Raw]
                , _main :: Bool
                , _lastChange :: BuildID
                , _lastCompile :: BuildID
                }

    -- Finding ports
    -- Present in AST.Source 
    -- But more convenient in Can.AST
    -> Effects


-}

encode :: EntryPoint -> Json.Encode.Value
encode (EntryPoint base path moduleName) =
    Json.Encode.object
        [ "base" ==> Json.Encode.chars dir
        , "filepath" ==> Json.Encode.chars path
        , "module" ==> Json.Encode.chars (Name.toChars moduleName)
        ]

data EntryPoint =
    EntryPoint
        { _baseDir :: FilePath
        , _filepath :: FilePath
        , _moduleName :: Name.Name
        -- , _flags :: Maybe Bool
        
        -- Port definitions
        -- , _incoming :: Maybe Elm.Docs.Module
        }

entrypoints :: FilePath -> IO (Either Exit.Reactor [Ext.Dev.EntryPoints.EntryPoint])
entrypoints root = do 
    details <- Ext.CompileProxy.loadProject root
    let locals = Elm.Details._locals details  

    let entry =  Map.foldrWithKey (toEntryPoint root) [] locals 
    
    pure (Right entry)



toEntryPoint :: FilePath -> ModuleName.Raw -> Elm.Details.Local ->  [EntryPoint] -> [EntryPoint]
toEntryPoint root  moduleName local entries =
    if Elm.Details._main local then
        let
            path = Elm.Details._path local
        in
        (EntryPoint root path moduleName) : entries
    else
        entries
    


