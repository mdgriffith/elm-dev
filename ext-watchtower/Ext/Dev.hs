{-# LANGUAGE OverloadedStrings #-}

module Ext.Dev 
    ( docs
    , info, Info(..)
    )
    where


import qualified Data.Maybe as Maybe
import qualified Elm.Docs
import qualified Reporting.Exit as Exit
import qualified Reporting.Warning as Warning

import qualified AST.Source as Src

import qualified Ext.CompileProxy
import qualified Ext.Dev.Docs
import qualified Ext.Dev.Warnings


-- warnings :: FilePath -> FilePath -> IO (Either () (Src.Module, [ Warning.Warning ]))
-- warnings root path =
--     pure (Left ())



data Info =
    Info 
        { _warnings :: Maybe (Src.Module, [ Warning.Warning ])
        , _docs :: Maybe Elm.Docs.Module
        }

info :: FilePath -> FilePath -> IO Info
info root path = do
    loaded <- Ext.CompileProxy.loadSingle root path
    let (Ext.CompileProxy.Single source maybeWarnings canonical compiled) = Ext.Dev.Warnings.addUnusedImports loaded
    let docs = case compiled of
                    Just (Right artifacts) ->
                        case  Ext.Dev.Docs.fromArtifacts artifacts of
                            Left err ->
                                Nothing
                            
                            Right docs ->
                                Just docs

                    _ ->
                        Nothing

    let warnings = case source of
                        Right sourceMod ->
                            Just (sourceMod, Maybe.fromMaybe [] maybeWarnings)
                        
                        Left _ ->
                            Nothing
   
    pure 
        (Info
            warnings
            docs
        )
    

docs :: FilePath -> FilePath -> IO (Maybe Elm.Docs.Module)
docs root path = do
    (Ext.CompileProxy.Single source warnings canonical compiled) <- Ext.CompileProxy.loadSingle root path
    case compiled of
        Just (Right artifacts) ->
            case  Ext.Dev.Docs.fromArtifacts artifacts of
                Left err ->
                    pure Nothing
                
                Right docs ->
                    pure (Just docs)

        _ ->
            pure Nothing

