{-# LANGUAGE OverloadedStrings #-}
module Ext.Dev.Package (getCurrentlyUsedOrLatestVersion) where

import qualified Data.Map as Map
import Control.Applicative ((<|>))


import qualified Elm.Package
import qualified Elm.Version
import qualified Elm.Outline

import qualified Stuff
import qualified Deps.Registry
import qualified Ext.Common

getCurrentlyUsedOrLatestVersion :: FilePath -> Elm.Package.Name -> IO (Maybe Elm.Version.Version)
getCurrentlyUsedOrLatestVersion rootDir packageName = do
    eitherOutline <- Elm.Outline.read rootDir
    case eitherOutline of
      Left err ->
        -- Otherwise get the latest version number
        getPackageNewestPackageVersionFromRegistry packageName
      
      Right (Elm.Outline.App appOutline) ->
        let 
            maybeLocal =
                  Map.lookup packageName (Elm.Outline._app_deps_direct appOutline) 
                    <|> Map.lookup packageName (Elm.Outline._app_deps_indirect appOutline)
                    <|> Map.lookup packageName (Elm.Outline._app_test_direct appOutline)
                    <|> Map.lookup packageName (Elm.Outline._app_test_indirect appOutline)
        in
        case maybeLocal of
            Nothing ->
                getPackageNewestPackageVersionFromRegistry packageName

            Just found ->
                pure maybeLocal
        
      Right (Elm.Outline.Pkg _) ->
        getPackageNewestPackageVersionFromRegistry packageName


getPackageNewestPackageVersionFromRegistry packageName = do
    packageCache <- Stuff.getPackageCache
    maybeRegistry <- Deps.Registry.read packageCache
    case maybeRegistry of
        Nothing ->
            -- Otherwise get the latest version number
            -- Download the package if it's not available
            pure Nothing
        
        Just registry ->
            case Map.lookup packageName (Deps.Registry._versions registry) of
                Nothing ->
                    -- Otherwise get the latest version number
                    -- Download the package if it's not available
                    pure Nothing
                
                Just knownVersions ->
                    -- Otherwise get the latest version number
                    -- Download the package if it's not available
                    pure (Just (Deps.Registry._newest knownVersions))
