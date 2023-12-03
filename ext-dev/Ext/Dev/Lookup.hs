{-# LANGUAGE OverloadedStrings #-}

module Ext.Dev.Lookup
  ( Ext.Dev.Lookup.lookupDefinition
  , lookupDefinitionMany
  , LookupResult(..)
  )
where


{-|

Find is finding stuff by source position.
Lookup is for when you know the name of the thing, you just want to get it. 

-}

import qualified AST.Source as Src
import qualified AST.Canonical as Can
import qualified Reporting.Annotation as A
import qualified Parse.Primitives as P
import qualified Watchtower.Editor
import qualified Data.List as List

import qualified Elm.ModuleName as ModuleName
import qualified Elm.Details
import qualified Ext.Dev.Project

import qualified Ext.CompileProxy

import Data.Name (Name)
import qualified Data.Map as Map
import Data.Function ((&))



data LookupResult
    = Union (Maybe Src.Comment) Can.Union
    | Alias (Maybe Src.Comment) Can.Alias
    | Def Src.Comment
    deriving (Show)

{-|

-}
lookupDefinition :: String -> ModuleName.Raw -> Name -> IO (Maybe LookupResult)
lookupDefinition root modName name =
    do
        project <- Ext.CompileProxy.loadProject root
        case Ext.Dev.Project.lookupModulePath project modName of 
            Nothing ->
                pure Nothing
            
            Just path -> do
                (Ext.CompileProxy.Single source warnings maybeInterfaces maybeCanonical compiled) <- Ext.CompileProxy.loadSingle root path
                
                case maybeCanonical of
                    Nothing -> pure Nothing

                    Just canonical -> do
                        let (Can.Module canModName exports docs decls unions aliases binops effects) = canonical
                        
                        let maybeDocs = (case docs of
                                            Src.NoDocs _ ->
                                                Nothing
                                            Src.YesDocs comm docComments ->
                                                Map.lookup name (Map.fromList docComments)
                                        )

                        case Map.lookup name unions of
                            Nothing ->
                                case Map.lookup name aliases of
                                    Nothing ->
                                        pure (fmap Def maybeDocs)

                                    Just alias ->
                                        pure (Just (Alias maybeDocs alias))
                                    

                            Just union ->
                                pure (Just (Union maybeDocs union))



lookupDefinitionMany :: String -> ModuleName.Raw -> [ Name ] -> IO (Map.Map Name LookupResult)
lookupDefinitionMany root modName names = do
    project <- Ext.CompileProxy.loadProject root
    case Ext.Dev.Project.lookupModulePath project modName of 
        Nothing ->
            pure Map.empty 
        
        Just path -> do
            (Ext.CompileProxy.Single source warnings interfaces maybeCanonical compiled) <- Ext.CompileProxy.loadSingle root path
            case maybeCanonical of
                Nothing -> pure Map.empty 

                Just canonical -> 
                    pure (List.foldl (getType canonical) Map.empty names)


getType (Can.Module canModName exports docs decls unions aliases binops effects) foundNames name =
    let 
        maybeDocs =
            case docs of
                Src.NoDocs _ ->
                    Nothing
                Src.YesDocs comm docComments ->
                    Map.lookup name (Map.fromList docComments) 
                        
    in
    case Map.lookup name unions of
        Nothing ->
            case Map.lookup name aliases of
                Nothing ->
                    foundNames

                Just alias_ ->
                    Map.insert name (Alias maybeDocs alias_) foundNames
        
        Just union ->
            Map.insert name (Union maybeDocs union) foundNames


