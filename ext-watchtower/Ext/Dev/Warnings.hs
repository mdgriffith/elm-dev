{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ext.Dev.Warnings
  ( addUnusedImports
  , addUnusedDeclarations
  , addAliasOptionsToWarnings
  )
where

import qualified Data.Set as Set
import Data.Name (Name)
import qualified Data.Map

import qualified Ext.CompileProxy
import qualified Ext.Dev.Find.Canonical

import qualified Elm.Interface
import qualified Elm.ModuleName
import qualified Reporting.Annotation as A
import qualified AST.Source as Src
import qualified AST.Canonical as Can
import qualified Reporting.Warning as Warning
import qualified Debug.Trace



addAliasOptionsToWarnings :: Ext.CompileProxy.SingleFileResult -> Ext.CompileProxy.SingleFileResult 
addAliasOptionsToWarnings untouched@(Ext.CompileProxy.Single source maybeWarnings maybeInterfaces canonical compiled) =
    case (canonical, maybeInterfaces, maybeWarnings) of
        (Just canModule, Just interfaces, Just warnings) ->
            let 
                newWarnings = fmap (addAliasesHelper canModule interfaces) warnings
            in
            Ext.CompileProxy.Single 
              source (Just newWarnings) maybeInterfaces canonical compiled

        _ ->
            untouched

addAliasesHelper :: Can.Module -> Data.Map.Map Elm.ModuleName.Raw Elm.Interface.Interface -> Warning.Warning -> Warning.Warning
addAliasesHelper canModule interfaces warning =
    case warning of
        Warning.UnusedImport _ _ ->
            warning 
        
        Warning.UnusedVariable _ _ _ ->
            warning
          
        Warning.MissingTypeAnnotation region name canType ->
            Warning.MissingTypeAnnotation region name (addAliasesToType canModule interfaces canType)

addAliasesToType :: Can.Module -> Data.Map.Map Elm.ModuleName.Raw Elm.Interface.Interface -> Can.Type -> Can.Type
addAliasesToType canModule interfaces canType =
    case canType of 
        Can.TLambda one two ->
            Can.TLambda
            (addAliasesToType canModule interfaces one)
            (addAliasesToType canModule interfaces two)

        Can.TVar _ ->
            canType

        Can.TType moduleName name children ->
            Can.TType moduleName name
            (fmap (addAliasesToType canModule interfaces) children)

        Can.TRecord fieldMap maybeName ->
            case getAliasForRecord fieldMap canModule interfaces of
                Nothing ->
                    canType
                
                Just aliasFound ->
                    aliasFound

        Can.TUnit ->
            canType

        Can.TTuple one two Nothing ->
            Can.TTuple 
            (addAliasesToType canModule interfaces one) 
            (addAliasesToType canModule interfaces two)
            Nothing

        Can.TTuple one two (Just three) ->
            Can.TTuple 
            (addAliasesToType canModule interfaces one) 
            (addAliasesToType canModule interfaces two)
            (Just ((addAliasesToType canModule interfaces three)))

        Can.TAlias moduleName name vars (Can.Holey holeyType) ->
            canType
        
        Can.TAlias moduleName name vars (Can.Filled holeyType) ->
            canType


getAliasForRecord :: Data.Map.Map Name Can.FieldType -> Can.Module -> Data.Map.Map Elm.ModuleName.Raw Elm.Interface.Interface -> Maybe Can.Type
getAliasForRecord fields canModule interfaces =
    let 
        (Can.Module _name _exports _docs _decls _unions aliases _binops _effects) = canModule

        matches =
            Data.Map.foldrWithKey 
              (getMatchingAliases canModule fields)
              []
              aliases
                          
    in
    case matches of
      [] ->
        Nothing

      (top : _) ->
          Just top


getMatchingAliases :: Can.Module -> Data.Map.Map Name Can.FieldType -> Name -> Can.Alias -> [Can.Type] -> [Can.Type]
getMatchingAliases canModule fields aliasName (Can.Alias vars aliasType) gathered =
    case aliasType of
      Can.TRecord aliasFieldMap maybeName ->
          -- We only care about matching aliases for records
          -- Everything else can contribte to obfuscation
          case fulfillAliasVars fields vars aliasFieldMap of
            Nothing -> gathered

            Just unifiedVars ->
              let 
                  (Can.Module moduleName _ _ _ _ _ _ _) = canModule

              in
              Can.TAlias moduleName aliasName unifiedVars (Can.Filled aliasType)
                : gathered

      Can.TLambda one two ->
          gathered

      Can.TVar _ ->
          gathered

      Can.TType moduleName name children ->
          gathered

      Can.TUnit ->
          gathered

      Can.TTuple one two Nothing ->
          gathered

      Can.TTuple one two (Just three) ->
          gathered

      Can.TAlias moduleName name myAliasVars (Can.Holey holeyType) ->
          gathered
      
      Can.TAlias moduleName name myAliasVars (Can.Filled holeyType) ->
          gathered

{-|
  Make sure oneFields is a subrecord of twoFields.

  And fill in what the vars should be for the alias.

-}
fulfillAliasVars :: Data.Map.Map Name Can.FieldType -> [Name] -> Data.Map.Map Name Can.FieldType -> Maybe [(Name, Can.Type)]
fulfillAliasVars oneFields aliasVars twoFields =
    let 

        aliasVarMap =
            Data.Map.fromList $ fmap (\name -> (name, Can.TVar name)) aliasVars

        unificationResult =
            Data.Map.foldrWithKey
                (\key value maybeVars ->
                  case maybeVars of
                    Nothing ->
                      -- something failed somewhere
                        Nothing   
                    Just vars ->
                        case Data.Map.lookup key twoFields of
                            Nothing ->
                                Nothing
                            
                            Just twoValue ->
                                case unifyFieldType value twoValue of 
                                  Nothing ->
                                    Nothing 
                                  
                                  Just unifiedVars ->
                                      Just (vars ++ unifiedVars)
                )
                (Just [])
                oneFields
    in
    case unificationResult of 
      Nothing ->
          Nothing

      Just varsResolved ->
          Just varsResolved


unifyFieldType :: Can.FieldType -> Can.FieldType -> Maybe [(Name, Can.Type)]
unifyFieldType (Can.FieldType _ one) (Can.FieldType _ two) =
    unifyType one two


{-| -}
unifyType :: Can.Type -> Can.Type -> Maybe [(Name, Can.Type)]
unifyType one two =
    case (one, two) of 
      (firstType, Can.TVar twoVarName) ->
          Just [(twoVarName, firstType)]

      (Can.TRecord oneFields maybeName, Can.TRecord twoFields twoMaybeName) ->
            let 
                (newVars, finalRemainingFields) =
                    Data.Map.foldrWithKey
                        (\key value (maybeVars, remainingTwoFields) ->
                            case maybeVars of
                                Nothing ->
                                    -- something failed somewhere
                                    ( Nothing
                                    , remainingTwoFields
                                    )

                                Just vars ->
                                    case Data.Map.lookup key remainingTwoFields of
                                        Nothing ->
                                            ( Nothing
                                            , remainingTwoFields
                                            )
                                        
                                        Just twoValue ->
                                            case unifyFieldType value twoValue of 
                                                Nothing ->
                                                    ( Nothing 
                                                    , Data.Map.delete key remainingTwoFields
                                                    )
                                                
                                                Just unifiedVars ->
                                                    ( Just (vars ++ unifiedVars)
                                                    , Data.Map.delete key remainingTwoFields
                                                    
                                                    )
                        )
                        (Just [], twoFields)
                        oneFields
            in
            if Data.Map.size finalRemainingFields > 0 then
                Nothing
            else
                newVars

      (Can.TLambda oneOne oneTwo, Can.TLambda twoOne twoTwo) ->
          (++)  
            <$> unifyType oneOne twoOne
            <*> unifyType oneTwo twoTwo

    
      (Can.TType oneModuleName oneName oneVars, Can.TType twoModuleName twoName twoVars) ->
          if oneModuleName == twoModuleName && oneName == twoName then 
              -- Wrong
              Just []
          else 
              Nothing

      (Can.TUnit, Can.TUnit) ->
          Just []

      (Can.TTuple oneOne oneTwo Nothing, Can.TTuple twoOne twoTwo Nothing) ->
          (++)  
            <$> unifyType oneOne twoOne
            <*> unifyType oneTwo twoTwo

      (Can.TTuple oneOne oneTwo (Just oneThree), Can.TTuple twoOne twoTwo (Just twoThree)) ->
          (\a b c ->
            a ++ b ++ b
          )
            <$> unifyType oneOne twoOne
            <*> unifyType oneTwo twoTwo
            <*> unifyType oneThree twoThree

      (Can.TAlias oneModuleName oneName oneVars _, Can.TAlias twoModuleName twoName twoVars _) ->
          if oneModuleName == twoModuleName && oneName == twoName then
              Just oneVars

          else 
              Nothing
      
      _ ->
          Nothing



addUnusedDeclarations :: Ext.CompileProxy.SingleFileResult -> Ext.CompileProxy.SingleFileResult 
addUnusedDeclarations untouched@(Ext.CompileProxy.Single source warnings interfaces canonical compiled) =
    case canonical of
        Nothing -> untouched

        Just canModule -> 
            let 
                (Can.Module _ exports _ decls _ _ _ _) = canModule
            in
            case exports of
                Can.ExportEverything _ ->
                    untouched 

                Can.Export exportMap -> do
                    let usedValues = Ext.Dev.Find.Canonical.usedValues canModule 
                    let (Can.Module _ _ _ decls _ _ _ _) = canModule
                    let unusedDecls = filterOutUsedDecls (Can._name canModule) exportMap usedValues decls
                    let unusedDeclWarnings = fmap declsToWarning unusedDecls
                    Ext.CompileProxy.Single source (addUnused unusedDeclWarnings warnings) interfaces canonical compiled


declsToWarning :: Can.Def -> Warning.Warning
declsToWarning unusedDef =
    case unusedDef of
        Can.Def locatedName pattern expr ->
            Warning.UnusedVariable (A.toRegion locatedName) Warning.Def (A.toValue locatedName)

        Can.TypedDef locatedName frevars pattern expr type_ ->
            Warning.UnusedVariable (A.toRegion locatedName) Warning.Def (A.toValue locatedName)


getDefIdentifier :: Can.Def -> Name
getDefIdentifier def =
    case def of
        Can.Def locatedName _ _ ->
            (A.toValue locatedName)

        Can.TypedDef locatedName _ _ _ _ ->
            (A.toValue locatedName)

filterOutUsedDecls :: Elm.ModuleName.Canonical -> Data.Map.Map Name (A.Located Can.Export) ->  Set.Set (Elm.ModuleName.Canonical, Name) -> Can.Decls -> [Can.Def]
filterOutUsedDecls modName exportMap used decls =
    case decls of 
        Can.SaveTheEnvironment ->
            []
        
        Can.Declare def moarDecls ->
            let 
                name = getDefIdentifier def
                identifier = (modName, name)
            in
            if Set.member identifier used || Data.Map.member name exportMap then
                filterOutUsedDecls modName exportMap used moarDecls
            else
                [def] <> filterOutUsedDecls modName exportMap used moarDecls
        
        Can.DeclareRec def defs moarDecls ->
            let 
                name = getDefIdentifier def
                identifier = (modName, name)
            in
            if Set.member identifier used || Data.Map.member name exportMap then
                filterOutUsedDecls modName exportMap used moarDecls
            else
                [def] <> filterOutUsedDecls modName exportMap used moarDecls


addUnusedImports :: Ext.CompileProxy.SingleFileResult -> Ext.CompileProxy.SingleFileResult 
addUnusedImports untouched@(Ext.CompileProxy.Single source warnings interfaces canonical compiled) =
    case source of
        Left _ -> untouched

        Right srcModule ->
            case fmap Ext.Dev.Find.Canonical.usedModules canonical of
                Nothing -> untouched

                Just usedModules -> do
                    let (Src.Module _ _ _ imports _ _ _ _ _) = srcModule
                    let filteredImports = filterOutDefaultImports imports
                    let importNames = Set.fromList $ fmap Src.getImportName filteredImports
                    let usedModuleNames = Set.map canModuleName usedModules
                    let unusedImports = Set.difference importNames usedModuleNames
                    let unusedImportWarnings = importsToWarnings (Set.toList unusedImports) filteredImports

                    Ext.CompileProxy.Single source (addUnused unusedImportWarnings warnings) interfaces canonical compiled

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