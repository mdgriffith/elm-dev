{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ext.Dev.Warnings
  ( addUnusedImports
  )
where

import qualified Data.Set as Set
import Data.Name (Name)

import qualified Ext.CompileProxy
import qualified Ext.Dev.Find.Canonical

import qualified Elm.ModuleName
import qualified Reporting.Annotation as A
import qualified AST.Source as Src
import qualified Reporting.Warning as Warning



addUnusedImports :: Ext.CompileProxy.SingleFileResult -> Ext.CompileProxy.SingleFileResult 
addUnusedImports untouched@(Ext.CompileProxy.Single source warnings canonical compiled) =
    case source of
        Left _ -> untouched

        Right srcModule ->
            case fmap Ext.Dev.Find.Canonical.used canonical of
                Nothing -> untouched

                Just usedModules -> do
                    let (Src.Module _ _ _ imports _ _ _ _ _) = srcModule
                    let filteredImports = filterOutDefaultImports imports
                    let importNames = Set.fromList $ fmap Src.getImportName filteredImports
                    let usedModuleNames = Set.map canModuleName usedModules
                    let unusedImports = Set.difference importNames usedModuleNames
                    let unusedImportWarnings = importsToWarnings (Set.toList unusedImports) filteredImports

                    Ext.CompileProxy.Single source (addUnused unusedImportWarnings warnings) canonical compiled

canModuleName :: Elm.ModuleName.Canonical -> Name
canModuleName (Elm.ModuleName.Canonical pkg modName) =
    modName

addUnused :: [Warning.Warning] -> Maybe [Warning.Warning] -> Maybe [Warning.Warning]
addUnused newWarnings maybeExisting =
    case maybeExisting of
        Nothing ->
            Just newWarnings
        Just old ->
            Just (old <> newWarnings)

importsToWarnings :: [Name] -> [Src.Import] -> [Warning.Warning]
importsToWarnings unusedNames imports =
  importsToWarningsHelper unusedNames imports []


importsToWarningsHelper :: [Name] -> [Src.Import] -> [Warning.Warning] -> [Warning.Warning]
importsToWarningsHelper unusedNames imports warnings =
  case imports of
    [] -> warnings
    (Src.Import (A.At region name) _ _) : remainingImports ->
      if any (\unusedName -> unusedName == name) unusedNames
        then importsToWarningsHelper unusedNames remainingImports (Warning.UnusedImport region name : warnings)
        else importsToWarningsHelper unusedNames remainingImports warnings




-- By default every Elm module has these modules imported with these region pairings.
-- If they add a manual import of, e.g. `import Maybe`, then we'll get the same name
-- but with a non-zero based region
filterOutDefaultImports :: [Src.Import] -> [Src.Import]
filterOutDefaultImports imports =
    filter
      (\(Src.Import (A.At region name) _ _) ->
        not $ any (\defaultImport -> defaultImport == (name,region)) defaultImports
      )
      imports


defaultImports :: [(Name, A.Region)]
defaultImports =
  [ ("Platform.Sub", A.Region (A.Position 0 0) (A.Position 0 0))
  , ("Platform.Cmd", A.Region (A.Position 0 0) (A.Position 0 0))
  , ("Platform", A.Region (A.Position 0 0) (A.Position 0 0))
  , ("Tuple", A.Region (A.Position 0 0) (A.Position 0 0))
  , ("Char", A.Region (A.Position 0 0) (A.Position 0 0))
  , ("String", A.Region (A.Position 0 0) (A.Position 0 0))
  , ("Result", A.Region (A.Position 0 0) (A.Position 0 0))
  , ("Maybe", A.Region (A.Position 0 0) (A.Position 0 0))
  , ("List", A.Region (A.Position 0 0) (A.Position 0 0))
  , ("Debug", A.Region (A.Position 0 0) (A.Position 0 0))
  , ("Basics", A.Region (A.Position 0 0) (A.Position 0 0))
  ]