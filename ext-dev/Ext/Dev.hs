{-# LANGUAGE OverloadedStrings #-}

module Ext.Dev 
    ( docs, docsForProject
    , info, Info(..)
    , warnings
    , entrypoints
    )
    where


import qualified Data.Maybe as Maybe
import qualified Elm.Docs
import qualified Reporting.Exit as Exit
import qualified Reporting.Warning as Warning
import qualified Data.NonEmptyList as NonEmpty
import qualified AST.Source as Src

import qualified Ext.CompileProxy
import qualified Ext.Dev.Docs
import qualified Ext.Dev.Warnings
import qualified Ext.Dev.EntryPoints


warnings :: FilePath -> FilePath -> IO (Either () (Src.Module, [ Warning.Warning ]))
warnings root path = do
    loaded <- Ext.CompileProxy.loadSingle root path
    let processed = Ext.Dev.Warnings.addAliasOptionsToWarnings
                        (Ext.Dev.Warnings.addUnusedDeclarations
                            (Ext.Dev.Warnings.addUnusedImports loaded)
                        )
    let (Ext.CompileProxy.Single source maybeWarnings interfaces canonical compiled) = processed
    case source of
        Right sourceMod ->
            pure (Right (sourceMod, Maybe.fromMaybe [] maybeWarnings))
        
        Left _ ->
            pure (Left ())



data Info =
    Info 
        { _warnings :: Maybe (Src.Module, [ Warning.Warning ])
        , _docs :: Maybe Elm.Docs.Module
        }

info :: FilePath -> FilePath -> IO Info
info root path = do
    loaded <- Ext.CompileProxy.loadSingle root path
    let (Ext.CompileProxy.Single source maybeWarnings interfaces canonical compiled) = Ext.Dev.Warnings.addUnusedImports loaded
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
    (Ext.CompileProxy.Single source warnings interfaces canonical compiled) <- Ext.CompileProxy.loadSingle root path
    case compiled of
        Just (Right artifacts) ->
            case  Ext.Dev.Docs.fromArtifacts artifacts of
                Left err ->
                    pure Nothing
                
                Right docs ->
                    pure (Just docs)

        _ ->
            pure Nothing



docsForProject :: FilePath -> FilePath -> IO (Either Exit.Reactor Elm.Docs.Documentation)
docsForProject root path = do
    Ext.CompileProxy.compileToDocs root (NonEmpty.singleton path)
        
   

entrypoints :: FilePath -> IO (Either Ext.CompileProxy.CompilationError [Ext.Dev.EntryPoints.EntryPoint])
entrypoints root =
    Ext.Dev.EntryPoints.entrypoints root