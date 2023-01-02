{-# LANGUAGE OverloadedStrings #-}

module Ext.Dev.Lookup
  ( Ext.Dev.Lookup.lookup
  , lookupMany
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
import qualified Watchtower.Editor
import qualified Data.List as List

import qualified Elm.ModuleName as ModuleName
import qualified Elm.Details

import qualified Ext.CompileProxy

import Data.Name (Name)
import qualified Data.Map as Map
import Data.Function ((&))




data LookupResult
    = Union Can.Union
    | Alias Can.Alias
    deriving (Show)

lookup :: String -> ModuleName.Canonical -> Name -> IO (Maybe LookupResult)
lookup root mod name =
    do
        project <- Ext.CompileProxy.loadProject root
        case lookupModulePath project mod of 
            Nothing ->
                pure Nothing
            
            Just path -> do
                (Ext.CompileProxy.Single source warnings maybeCanonical compiled) <- Ext.CompileProxy.loadSingle root path
                case maybeCanonical of
                    Nothing -> pure Nothing

                    Just canonical -> do
                        let (Can.Module canModName exports docs decls unions aliases binops effects) = canonical
                        case Map.lookup name unions of
                            Nothing ->
                                pure (fmap Alias (Map.lookup name aliases))

                            Just union ->
                                pure (Just (Union union))



lookupMany :: String -> ModuleName.Canonical -> [ Name ] -> IO (Map.Map Name LookupResult)
lookupMany root mod names =
    do
        project <- Ext.CompileProxy.loadProject root
        case lookupModulePath project mod of 
            Nothing ->
                pure Map.empty 
            
            Just path -> do
                (Ext.CompileProxy.Single source warnings maybeCanonical compiled) <- Ext.CompileProxy.loadSingle root path
                case maybeCanonical of
                    Nothing -> pure Map.empty 

                    Just canonical -> 
                        pure (List.foldl (getType canonical) Map.empty names)


getType (Can.Module canModName exports docs decls unions aliases binops effects) foundNames name =
    case Map.lookup name unions of
        Nothing ->
            case Map.lookup name aliases of
                Nothing ->
                    foundNames

                Just alias_ ->
                    Map.insert name (Alias alias_) foundNames
        

        Just union ->
            Map.insert name (Union union) foundNames

lookupModulePath :: Elm.Details.Details -> ModuleName.Canonical -> Maybe FilePath
lookupModulePath details canModuleName =
  details
    & Elm.Details._locals
    & Map.lookup (ModuleName._module canModuleName)
    & fmap Elm.Details._path