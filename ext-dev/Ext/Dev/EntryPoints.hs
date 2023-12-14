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
import qualified AST.Canonical as Can
import qualified AST.Optimized as Opt


import qualified Json.Encode
import Json.Encode ((==>))

import qualified Ext.CompileHelpers.Generic
import qualified Ext.CompileProxy
import qualified Ext.Dev.Docs
import qualified Ext.Dev.Warnings
import qualified Elm.Details
import qualified Elm.Outline
import qualified Elm.ModuleName as ModuleName
import qualified Ext.Dev.Project.Ports
import qualified Reporting.Render.Type
import qualified Reporting.Doc
import qualified Reporting.Render.Type.Localizer

import qualified Ext.CompileHelpers.Disk
import qualified Build


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
encode (EntryPoint base path moduleName incoming outgoing) =
    Json.Encode.object
        [ "base" ==> Json.Encode.chars base
        , "filepath" ==> Json.Encode.chars path
        , "module" ==> Json.Encode.name moduleName

        -- Still need to make this correct
        -- , "incoming" ==> encodePorts incoming
        -- , "outgoing" ==> encodePorts outgoing
        ]

encodePorts :: Ext.Dev.Project.Ports.PortGroup -> Json.Encode.Value
encodePorts ports =
    Json.Encode.object
        (Map.foldrWithKey
            (\(moduleName, name) type_ entries ->
                let 
                    field = 
                        (Name.toChars name) ==> 
                            (Json.Encode.object 
                                [ "module" ==> Json.Encode.name moduleName
                                , "type" ==> Json.Encode.chars (renderType type_)
                                ]
                            )
                in
                field : entries
            )
            []
            ports
        )

renderType :: Can.Type -> String
renderType type_ =
    Reporting.Doc.toString
        (Reporting.Render.Type.canToDoc 
            (Reporting.Render.Type.Localizer.fromNames Map.empty) 
            Reporting.Render.Type.None 
            type_
        )


data EntryPoint =
    EntryPoint
        { _baseDir :: FilePath
        , _filepath :: FilePath
        , _moduleName :: Name.Name
        -- , _flags :: Maybe Bool
        
        -- Port definitions
        , _incoming :: Ext.Dev.Project.Ports.PortGroup
        , _outgoing :: Ext.Dev.Project.Ports.PortGroup
        }

entrypoints :: FilePath -> IO (Either Ext.CompileProxy.CompilationError [Ext.Dev.EntryPoints.EntryPoint])
entrypoints root = do 
    detailsResult <- Ext.CompileProxy.ensureModulesAreCompiled root Nothing
    case detailsResult of
        Left err -> do
            pure (Left err)

        Right details -> do
            let locals = Elm.Details._locals details

            let entry = Map.foldrWithKey (toEntryPoint root Ext.Dev.Project.Ports.empty) [] locals 
            
            pure (Right entry)



toEntryPoint :: FilePath -> Ext.Dev.Project.Ports.Ports -> ModuleName.Raw -> Elm.Details.Local ->  [EntryPoint] -> [EntryPoint]
toEntryPoint root ports moduleName local entries =
    if Elm.Details._main local then
        let
            path = Elm.Details._path local

            incoming = Ext.Dev.Project.Ports._incoming ports

            outgoing = Ext.Dev.Project.Ports._outgoing ports
        in
        (EntryPoint root path moduleName incoming outgoing) : entries
    else
        entries
    
