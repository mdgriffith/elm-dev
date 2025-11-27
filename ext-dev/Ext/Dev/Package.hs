{-# LANGUAGE OverloadedStrings #-}
module Ext.Dev.Package (getDocs, getElmJsonVersion, getCurrentlyUsedOrLatestVersion, getPackageNewestPackageVersionFromRegistry) where

import qualified Data.Map as Map
import Control.Applicative ((<|>))
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, readMVar)

import qualified Elm.Package
import qualified Elm.Version
import qualified Elm.Outline
import qualified Elm.Docs

import qualified Stuff
import qualified Deps.Diff
import qualified Deps.Registry
import qualified Ext.Common
import qualified Http
import qualified Reporting.Exit
import qualified Elm.Constraint as Con


getDocs :: Elm.Package.Name -> Elm.Version.Version -> IO (Either Reporting.Exit.DocsProblem Elm.Docs.Documentation)
getDocs packageName version = do
    mvar  <- newEmptyMVar
    _     <- forkIO $ putMVar mvar =<< Http.getManager
    packageCache <- Stuff.getPackageCache
    Stuff.withRegistryLock packageCache $ do
        manager       <- readMVar mvar
        Deps.Diff.getDocs packageCache manager packageName version

    

getCurrentlyUsedOrLatestVersion :: FilePath -> Elm.Package.Name -> IO (Maybe Elm.Version.Version)
getCurrentlyUsedOrLatestVersion rootDir packageName = do
    fromOutline <- getElmJsonVersion rootDir packageName
    case fromOutline of
      Just v ->
        pure (Just v)
      Nothing ->
        getPackageNewestPackageVersionFromRegistry packageName

-- | Read the project's elm.json outline (if present) and return the package version
-- directly from the outline. Returns Nothing if the outline cannot be read or the
-- package is not present in any dependency set.
getElmJsonVersion :: FilePath -> Elm.Package.Name -> IO (Maybe Elm.Version.Version)
getElmJsonVersion rootDir packageName = do
    eitherOutline <- Elm.Outline.read rootDir
    case eitherOutline of
      Left _ ->
        pure Nothing
      
      Right (Elm.Outline.App appOutline) ->
        let 
            maybeLocal =
                  Map.lookup packageName (Elm.Outline._app_deps_direct appOutline) 
                    <|> Map.lookup packageName (Elm.Outline._app_deps_indirect appOutline)
                    <|> Map.lookup packageName (Elm.Outline._app_test_direct appOutline)
                    <|> Map.lookup packageName (Elm.Outline._app_test_indirect appOutline)
        in
        pure maybeLocal
        
      Right (Elm.Outline.Pkg _) ->
        -- For package projects, dependencies are constraints; choose newest version satisfying it.
        case eitherOutline of
          Right (Elm.Outline.Pkg pkgOutline) -> do
            let mConstraint =
                  Map.lookup packageName (Elm.Outline._pkg_deps pkgOutline)
                    <|> Map.lookup packageName (Elm.Outline._pkg_test_deps pkgOutline)
            case mConstraint of
              Nothing ->
                pure Nothing
              Just constraint -> do
                packageCache <- Stuff.getPackageCache
                maybeRegistry <- Deps.Registry.read packageCache
                case maybeRegistry >>= Deps.Registry.getVersions packageName of
                  Nothing ->
                    pure Nothing
                  Just known ->
                    let versions = Deps.Registry._newest known : Deps.Registry._previous known
                        pick = Prelude.filter (Con.satisfies constraint) versions
                    in
                    case pick of
                      v : _ -> pure (Just v)
                      [] -> pure Nothing



getPackageNewestPackageVersionFromRegistry :: Elm.Package.Name -> IO (Maybe Elm.Version.Version)
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
