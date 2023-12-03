{-# LANGUAGE OverloadedStrings #-}

module Ext.Dev.Usage
  ( usageOfModule
  , UsageSummary(..)
  , encode
  -- of type
  , usageOfType
  , encodeUsageOfType
  )
where


{-|

Given a module name, report all usages of exposed values from that module


-}

import qualified System.IO

import qualified Control.Monad
import qualified AST.Source as Src
import qualified AST.Canonical as Can
import qualified Reporting.Annotation as A
import qualified Parse.Primitives as P
import qualified Watchtower.Editor
import qualified Data.List as List
import qualified Terminal.Dev.Error

import qualified Reporting.Warning as Warning
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Details
import qualified Ext.Dev.Project
import qualified Ext.Dev.Lookup
import qualified Ext.Dev.Explain
import qualified Ext.Dev.Find.Canonical

import qualified Ext.CompileProxy

import qualified Json.Encode
import Json.Encode ((==>))
import qualified Ext.Dev.Json.Encode

import Data.Name (Name)
import qualified Data.Name as Name
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Ext.Dev.Help

import Data.Function ((&))

data UsageSummary =
    UsageSummary
        { _path :: FilePath
        , _usages :: [ExposedValue]
        }

data ExposedValue =
    ExposedValue
        { _name :: Name
        , _usedBy :: [ ModuleName.Raw ]
        }



usageOfModule :: String -> Elm.Details.Details -> ModuleName.Raw -> IO (Maybe UsageSummary)
usageOfModule root project mod =
  case Ext.Dev.Project.lookupModulePath project mod of
    Nothing -> pure Nothing
    Just path -> do
      let importers = Ext.Dev.Project.importersOf project mod
      usages <- Control.Monad.foldM (lookupUsage root project mod) Map.empty importers
      pure (Just (UsageSummary path (toExposedValues usages)))


toExposedValues :: Map.Map Name (Set.Set ModuleName.Raw) -> [ExposedValue]
toExposedValues foundMap =
    Map.foldrWithKey
        (\name usedBy exposedValues ->
            ExposedValue name (Set.toList usedBy) : exposedValues
        )
        []
        foundMap




{--}

lookupUsage :: String -> Elm.Details.Details -> ModuleName.Raw -> Map.Map Name (Set.Set ModuleName.Raw) ->  ModuleName.Raw -> IO (Map.Map Name (Set.Set ModuleName.Raw))
lookupUsage root details originalModule foundMap importer = do
    case Ext.Dev.Project.lookupModulePath details importer of 
        Nothing ->
            pure foundMap
        
        Just path -> do
            (Ext.CompileProxy.Single source warnings maybeInterfaces maybeCanonical compiled) <- Ext.CompileProxy.loadSingle root path
            
            case maybeCanonical of
                Nothing -> pure foundMap

                Just canonical ->
                    let 
                        usedValues = Ext.Dev.Find.Canonical.usedValues canonical
                            
                    in
                    Set.fold
                        (\(canMod, valName) innerMap ->
                            if ModuleName._module canMod == originalModule then
                                case Map.lookup valName innerMap of
                                  Nothing ->
                                    Map.insert valName (Set.singleton importer) innerMap

                                  Just existing ->
                                    Map.insert valName (Set.insert importer existing) innerMap
                            else
                                innerMap
                          

                        ) 
                        foundMap
                        usedValues
                        & pure

                 
                 
                    
                    
                    

encode :: UsageSummary -> Json.Encode.Value
encode summary =
    Json.Encode.object
        [ ( "path", Json.Encode.chars (_path summary) )
        , ( "usages", Json.Encode.list encodeExposedValue (_usages summary)) 
        ]

encodeExposedValue :: ExposedValue -> Json.Encode.Value
encodeExposedValue exposedValue =
    Json.Encode.object
        [ ( "name", Json.Encode.name (_name exposedValue))
        , ( "usedBy", Json.Encode.list Json.Encode.name (_usedBy exposedValue))
        ]



{- Usage of Type -}


data UsageOfType =
    UsageOfType 
        { _typeUsedBy :: Map.Map ModuleName.Raw [TypeUsage]
        }

 
data TypeUsage = 
    TypeUsage 
        { _usageName :: A.Located Name
        , _usageDef :: Can.Def
        , _usageType :: Can.Type
        , _usageExplanation :: Maybe Ext.Dev.Explain.Explanation
        , _isExposed :: Bool
        }
        deriving (Show)

encodeUsageOfType :: UsageOfType -> Json.Encode.Value
encodeUsageOfType summary =
    Json.Encode.object
        [ ( "usages", Json.Encode.list encodeModuleTypeUsage (Map.toList (_typeUsedBy summary))) 
        ]

encodeModuleTypeUsage :: ( ModuleName.Raw, [ TypeUsage ] ) -> Json.Encode.Value
encodeModuleTypeUsage (moduleName, usages) =
    Json.Encode.object
        [ ( "module", Json.Encode.name moduleName)
        , ( "usedBy", Json.Encode.list encodeTypeUsage usages)
        ]


encodeTypeUsage :: TypeUsage -> Json.Encode.Value
encodeTypeUsage (TypeUsage locatedName canDef canType explanation isExposed) =
    Json.Encode.object
        [ ( "region", Ext.Dev.Json.Encode.region (A.toRegion locatedName))
        , ( "name", Json.Encode.name (A.toValue locatedName) )
        , ( "explanation",  Ext.Dev.Json.Encode.maybe Ext.Dev.Explain.encode explanation)
        ]


usageOfType ::
    String 
    -> Elm.Details.Details 
    -> ModuleName.Raw 
    -> Name.Name 
    -> IO (Either Terminal.Dev.Error.Error UsageOfType)
usageOfType root details mod valueName =
  case Ext.Dev.Project.lookupModulePath details mod of
    Nothing -> pure (Left Terminal.Dev.Error.CouldNotFindModule)
    Just path -> do
      let importers = Ext.Dev.Project.importersOf details mod
      usages <- Control.Monad.foldM (lookupUsageOfType root details mod valueName) Map.empty importers
      withExplanations <- Control.Monad.foldM (addExplanationToModule root details) Map.empty (Map.toList usages)
      
      pure (Right (UsageOfType withExplanations))


addExplanationToModule :: String -> Elm.Details.Details -> Map.Map ModuleName.Raw [TypeUsage] -> ( ModuleName.Raw, [TypeUsage] ) -> IO (Map.Map ModuleName.Raw [TypeUsage])
addExplanationToModule root details gathered (moduleName, typeUsages) = do
    case Ext.Dev.Project.lookupModulePath details moduleName of
        Nothing -> pure (Map.insert moduleName typeUsages gathered)
        Just modulePath -> do
            single <- Ext.CompileProxy.loadSingle root modulePath
            usagesWithExplanations <- Control.Monad.mapM (addExplanation root single) typeUsages
            pure (Map.insert moduleName usagesWithExplanations gathered) 

addExplanation :: String -> Ext.CompileProxy.SingleFileResult -> TypeUsage -> IO TypeUsage
addExplanation root single (TypeUsage name def tipe _ isExposed) = do
    maybeExplanation <- Ext.Dev.Explain.explainFromFileResult root (A.toValue name) single
    pure (TypeUsage name def tipe maybeExplanation isExposed)

lookupUsageOfType :: 
    String 
    -> Elm.Details.Details 
    -> ModuleName.Raw 
    -> Name.Name
    -> Map.Map ModuleName.Raw [TypeUsage]
    -> ModuleName.Raw 
    -> IO (Map.Map ModuleName.Raw [TypeUsage])
lookupUsageOfType root details originalModule targetTypeName foundMap importerModuleName = do
    case Ext.Dev.Project.lookupModulePath details importerModuleName of 
        Nothing -> do
             -- This should probably be logged
            pure foundMap
        
        Just path -> do
            single <- Ext.CompileProxy.loadSingle root path
            let (Ext.CompileProxy.Single source warnings maybeInterfaces maybeCanonical compiled) = single
            
            case maybeCanonical of
                Nothing -> do
                     -- This should probably be logged
                    pure foundMap

                Just (Can.Module name exports docs decls unions aliases binops effects) -> do
                    let thereIsAnExposedAlias = hasExposedAlias originalModule targetTypeName exports aliases
                    foundMapWithAliases <- if thereIsAnExposedAlias then do 
                                                let importers = Ext.Dev.Project.importersOf details (ModuleName._module name)
                                                Control.Monad.foldM (lookupUsageOfType root details originalModule targetTypeName) foundMap importers
                                            else do
                                                pure foundMap

                    let missingTypes = Ext.Dev.Help.toMissingTypeLookup single
                    let foundTypeUsages = usedValueInDecls exports missingTypes originalModule targetTypeName decls []
                    case foundTypeUsages of
                        [] ->
                            pure foundMapWithAliases

                        _ ->
                            if List.any _isExposed foundTypeUsages && not thereIsAnExposedAlias then do
                                -- If there was an exposed alias, we're already searching downstream modules
                                let importers = Ext.Dev.Project.importersOf details (ModuleName._module name)
                                finalFound <- Control.Monad.foldM (lookupUsageOfType root details originalModule targetTypeName) foundMapWithAliases importers
                                pure (Map.insert importerModuleName foundTypeUsages finalFound)
                                

                            else
                                pure (Map.insert importerModuleName foundTypeUsages foundMapWithAliases)


hasExposedAlias originalMpodule targetTypeName exports aliases =
    Map.foldrWithKey
        (\aliasName (Can.Alias names aliasType) found ->
            if found then
                found
            else 
                typeIsUsed originalMpodule targetTypeName aliasType
                    && Ext.Dev.Help.isExposed aliasName exports
        ) 
        False 
        aliases



usedValueInDecls ::
    Can.Exports 
    -> Map.Map Name.Name Can.Type
    -> ModuleName.Raw 
    -> Name.Name
    -> Can.Decls 
    -> [ TypeUsage ]
    -> [ TypeUsage ]
usedValueInDecls exports missingTypes moduleName targetTypeName decls found =
    case decls of
        Can.Declare def moarDecls ->
            (usedTypeInDef exports missingTypes moduleName targetTypeName def found)
                & usedValueInDecls exports missingTypes moduleName targetTypeName moarDecls
        
        Can.DeclareRec def defs moarDecls ->
            List.foldl (\gathered innerDef -> usedTypeInDef exports missingTypes moduleName targetTypeName innerDef gathered) found (def : defs)
                & usedValueInDecls exports missingTypes moduleName targetTypeName moarDecls

        Can.SaveTheEnvironment ->
            found
                        
                    

usedTypeInDef ::
    Can.Exports 
    -> Map.Map Name.Name Can.Type
    -> ModuleName.Raw 
    -> Name.Name
    -> Can.Def
    -> [ TypeUsage ]
    -> [ TypeUsage ]
usedTypeInDef exports missingTypes moduleName targetTypeName def found =
    case def of
        Can.Def locatedName patterns expr ->
            case Map.lookup (A.toValue locatedName) missingTypes of
                Nothing -> found

                Just tipe ->
                    if typeIsUsed moduleName targetTypeName tipe then 
                        TypeUsage locatedName def tipe Nothing 
                            (Ext.Dev.Help.isExposed (A.toValue locatedName) exports) 
                            : found
                    else
                        found
            
            
        
        Can.TypedDef locatedName freeVars patternTypes expr tipe ->
            if typeIsUsed moduleName targetTypeName tipe then 
                TypeUsage locatedName def tipe Nothing 
                    (Ext.Dev.Help.isExposed (A.toValue locatedName) exports)  
                    : found
            else
                found

typeIsUsed ::
    ModuleName.Raw 
    -> Name.Name
    -> Can.Type
    -> Bool
typeIsUsed targetModuleName targetTypeName canType =
    case canType of 
        Can.TLambda one two ->
            typeIsUsed targetModuleName targetTypeName one 
                || typeIsUsed targetModuleName targetTypeName two

        Can.TVar _ ->
            False

        Can.TType moduleName name children ->
            if ModuleName._module moduleName == targetModuleName 
                && name == targetTypeName 
            then 
                True
            else 
                List.any (typeIsUsed targetModuleName targetTypeName) children


        Can.TRecord fieldMap maybeName ->
            Map.foldr
                (\(Can.FieldType _ fieldType) isFound ->
                    if isFound then 
                        isFound 
                    else 
                        typeIsUsed targetModuleName targetTypeName fieldType
                
                ) False fieldMap 
            

        Can.TUnit ->
            False

        Can.TTuple one two Nothing ->
           typeIsUsed targetModuleName targetTypeName one 
                || typeIsUsed targetModuleName targetTypeName two

        Can.TTuple one two (Just three) ->
            typeIsUsed targetModuleName targetTypeName one 
                || typeIsUsed targetModuleName targetTypeName two
                || typeIsUsed targetModuleName targetTypeName three

        Can.TAlias moduleName name vars (Can.Holey holeyType) ->
             if ModuleName._module moduleName == targetModuleName 
                && name == targetTypeName 
            then 
                True
            else 
                typeIsUsed targetModuleName targetTypeName holeyType
                
        
        Can.TAlias moduleName name vars (Can.Filled holeyType) ->
            if ModuleName._module moduleName == targetModuleName 
                && name == targetTypeName 
            then 
                True
            else 
                typeIsUsed targetModuleName targetTypeName holeyType
