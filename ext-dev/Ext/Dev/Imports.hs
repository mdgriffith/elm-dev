{-# LANGUAGE OverloadedStrings #-}

module Ext.Dev.Imports
  ( getImportSummary
  , ImportSummary(..)
  , Import(..)
  , encode
  , encodeSummary
  )
where


{-|

given a module name, report:



    - [ ]  All direct imports
    - [ ]  All indirect imports
    - [ ]  All external packages that are referenced in any of the above imports
- [ ]  For every imported thing, also report:
    - [ ]  The literal filename of that file
    - [x]  The module name
    - [ ]  Values used
-}

import qualified AST.Source as Src
import qualified AST.Canonical as Can
import qualified Reporting.Annotation as A



import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Name as Name

import qualified Elm.ModuleName as ModuleName
import qualified Elm.Details
import qualified Elm.Package

import qualified Ext.CompileProxy

import qualified Data.Name as Name (Name)
import qualified Data.Map as Map
import Data.Function ((&))

import qualified Json.Encode
import Json.Encode ((==>))
import qualified System.IO
import qualified Ext.Dev.Project



data ImportSummary =
  ImportSummary
    { _packagesImported :: [PackageImport]
    , _importGraph :: [Import]
    }

data Import =
  Import
    { _path :: FilePath
    , _moduleName :: ModuleName.Raw
    , _imports :: [ModuleName.Raw]
    }

data PackageImport =
  PackageImport
    {  _name :: Elm.Package.Name
    , _usedModules :: [ModuleName.Raw]
    }

encodeSummary :: ImportSummary -> Json.Encode.Value
encodeSummary (ImportSummary packages imports) =
    Json.Encode.object
        [ "packages" ==> Json.Encode.list encodePackageImport packages
        , "imports" ==> Json.Encode.list encode imports
        ]

encodePackageImport :: PackageImport -> Json.Encode.Value
encodePackageImport (PackageImport name usedModules) =
    Json.Encode.object
        [ "name" ==> Elm.Package.encode name
        , "usedModules" ==> Json.Encode.list Json.Encode.name usedModules
        ]

getImportSummary :: Elm.Details.Details -> ModuleName.Raw -> ImportSummary
getImportSummary details moduleName =
    let 
        -- Get local imports
        localImports = getImportsForModule details moduleName

        -- Get used packages
        localImportNames = List.concatMap _imports localImports
        packageImports = List.foldl (gatherPackageImports details) Map.empty (moduleName : localImportNames)

    in
    ImportSummary (toPackageList packageImports) localImports


toPackageList :: Map.Map Elm.Package.Name (Set.Set ModuleName.Raw) -> [PackageImport]
toPackageList packageImports =
    Map.foldrWithKey (\k v acc -> PackageImport k (Set.toList v) : acc) [] packageImports


gatherPackageImports :: Elm.Details.Details ->  Map.Map Elm.Package.Name (Set.Set ModuleName.Raw) -> ModuleName.Raw -> Map.Map Elm.Package.Name (Set.Set ModuleName.Raw)
gatherPackageImports details acc modName =
    let 
        foreigns = Elm.Details._foreigns details
    in
    case Map.lookup modName foreigns of 
        Nothing ->
            acc
        
        Just (Elm.Details.Foreign packageName indirectDeps) ->
            case Map.lookup packageName acc of
                Nothing ->
                    Map.insert packageName (Set.singleton modName) acc
                Just usedModules ->
                    Map.insert packageName (Set.insert modName usedModules) acc

getPackageImports :: Elm.Details.Details -> ModuleName.Raw -> [PackageImport]
getPackageImports details modName =
    let 
        foreigns = Elm.Details._foreigns details
    in
    case Map.lookup modName foreigns of 
        Nothing ->
            []
        
        Just (Elm.Details.Foreign packageName indirectDeps) ->
            let 
                importDetails = PackageImport packageName [modName]
            in
            [importDetails]


getImportsForModule :: Elm.Details.Details -> ModuleName.Raw -> [Import]
getImportsForModule details modName =
    let 
        locals = Elm.Details._locals details
    in
    case Map.lookup modName locals of 
        Nothing ->
            []
        
        Just importedLocalFound ->
            let 
                importDetails = localToImportDetails modName importedLocalFound
            
                indirectDeps = 
                    (List.concatMap (getImportsForModule details) 
                        (Elm.Details._deps importedLocalFound)
                    )
            in
            importDetails : indirectDeps
            
            
localToImportDetails :: ModuleName.Raw -> Elm.Details.Local -> Import
localToImportDetails modName local =
    Import
        { _path = Elm.Details._path local
        , _moduleName = modName
        , _imports = 
            List.filter 
                (\imp -> 
                    not (Set.member imp Ext.Dev.Project.defaultImports)
                )
                (Elm.Details._deps local)
        }

encode :: Import -> Json.Encode.Value
encode (Import path moduleName imports) =
    Json.Encode.object
        [ "path" ==> Json.Encode.chars path
        , "module" ==> Json.Encode.name moduleName
        , "imports" ==> Json.Encode.list Json.Encode.name imports
        ]