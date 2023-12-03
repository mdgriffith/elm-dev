{-# LANGUAGE OverloadedStrings #-}

module Ext.Dev.Explain
  ( explain
  , explainFromFileResult
  , explainAtLocation
  , Explanation
  , encode
  , encodeInlineDefinition
  )
where

{-|
Given a point location, return all the Types and values that are used 
in the definition at that point.



-}

import qualified AST.Source as Src
import qualified AST.Canonical as Can

import qualified System.IO
import qualified Json.String
import qualified Elm.Package as Package
import qualified Elm.ModuleName as ModuleName
import qualified Reporting.Annotation as A
import qualified Reporting.Render.Type
import qualified Reporting.Render.Type.Localizer
import qualified Control.Monad as Monad

import qualified Reporting.Doc

import qualified Data.Maybe as Maybe
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Name as Name
import qualified Data.Utf8 as Utf8
import Data.Name (Name)

import Data.Function ((&))
import qualified Json.Encode
import Json.Encode ((==>))

import qualified Ext.CompileProxy
import qualified Ext.Dev.Find.Source
import qualified Ext.Dev.Lookup
import qualified Ext.Dev.Json.Encode
import qualified Ext.Dev.Project
import qualified Ext.Dev.Help

import qualified Elm.Details


import qualified Watchtower.Editor
import qualified Ext.Log



data Explanation =
    Explanation
        { _localizer :: Reporting.Render.Type.Localizer.Localizer
        , _modulename :: Name
        , _declaration :: DeclarationMetadata
        , _references :: [ Reference ]
        }
        deriving (Show)

data DeclarationMetadata =
    DeclarationMetadata
        { _declarationName :: Name
        , _declarationType :: Maybe Can.Type
        , _declarationTypeComponents :: [ TypeComponentDefinition ]
        , _declarationRange :: A.Region
        , _declarationRecursive :: Bool
        }
        deriving (Show)


data TypeComponentDefinition
    = TypeFromDef TypeUsage Ext.Dev.Lookup.LookupResult
    | TypeInline 
        { _typeInline :: Can.Type
        , _typeReferences :: [ (TypeUsage, Ext.Dev.Lookup.LookupResult) ]
        , _typeDefinition :: TypeDefinition
        }
    deriving (Show)


data TypeDefinition
    = Defined TypeUsage Ext.Dev.Lookup.LookupResult
    | Inline Can.Type
    deriving (Show)


data Reference
    = ValueReference   
        { _source :: Source
        , _name :: Name
        , _type :: Can.Type
        }
    | TypeReference TypeUsage Ext.Dev.Lookup.LookupResult
    deriving (Show)


data TypeUsage
    = TypeUsage ModuleName.Canonical Name [ Can.Type ]
    | AliasUsage ModuleName.Canonical Name [(Name, Can.Type)] Can.AliasType
    | Definition  ModuleName.Canonical Name Can.Type
    | UnionVariantUsage ModuleName.Canonical Name (Set.Set Name)
    deriving (Eq, Show)

data Source 
    = External ModuleName.Canonical
    | TopLevelDeclaration
    | LetValue
    deriving (Eq, Ord, Show)
  

explain :: Elm.Details.Details -> String -> ModuleName.Raw -> Name -> IO (Maybe Explanation)
explain details root modulename valueName = do
    case Ext.Dev.Project.lookupModulePath details modulename of
        Nothing ->
            pure Nothing
        
        Just modulePathName ->
            explainAtModulePath root modulePathName valueName


explainAtModulePath :: String -> FilePath -> Name -> IO (Maybe Explanation)
explainAtModulePath root modulePath valuename = do
    single <- Ext.CompileProxy.loadSingle root modulePath
    explainFromFileResult root valuename single
   

explainFromFileResult :: String -> Name -> Ext.CompileProxy.SingleFileResult -> IO (Maybe Explanation)
explainFromFileResult root valueName (single@(Ext.CompileProxy.Single source warnings interfaces canonical compiled)) =
    case (source, canonical) of
        (Right srcModule, Just canMod) -> do
            let missingTypeLookup = Ext.Dev.Help.toMissingTypeLookup single
            let localizer = Reporting.Render.Type.Localizer.fromModule srcModule
            let found = fmap (Ext.Dev.Find.Source.withCanonical canMod) (Ext.Dev.Find.Source.definitionNamed valueName srcModule)
            case found of
                Nothing -> do
                    pure Nothing

                Just (Ext.Dev.Find.Source.FoundValue (Just def) (A.At pos val)) ->  do
                    let (refCollection@(ReferenceCollection refMap usedTypes refUnions usedModules)) = getFoundDefTypes missingTypeLookup def emptyRefCollection
                    
                    -- System.IO.hPutStrLn System.IO.stdout "Explaining : "
                    -- System.IO.hPutStrLn System.IO.stdout (show ((Src.getName srcModule), valueName))
                    -- System.IO.hPutStrLn System.IO.stdout (show refCollection)
                    foundTypeRefs <- lookUpTypesByFragments root 
                                        (toLookupCollection refCollection)

                    Ext.Log.log Ext.Log.Misc (show (List.length foundTypeRefs))
                    let refs = Map.elems refMap
                    metadata <- getDeclarationMetadata root missingTypeLookup pos def
                    pure 
                        (Just 
                            (Explanation
                                localizer
                                (Src.getName srcModule)
                                metadata
                                (refs <> fmap pairToReference foundTypeRefs)
                            )
                        )
                
                Just (Ext.Dev.Find.Source.FoundUnion (Just union) (A.At pos srcUnion)) -> do
                    let moduleName = Can._name canMod
                    let usage = (UnionVariantUsage moduleName valueName Set.empty)
                    let lookupCollection = unionToLookupCollection moduleName valueName union

                    typeComponents <- lookUpTypesByFragments root lookupCollection
                    
                    let metadata = DeclarationMetadata valueName (Just (Can.TVar valueName)) (fmap toTypeComponentDefinition typeComponents) pos False
                    
                    pure 
                        (Just 
                            (Explanation
                                localizer
                                (Src.getName srcModule)
                                metadata
                                []
                            )
                        )

                Just (Ext.Dev.Find.Source.FoundAlias (Just canAlias) (A.At pos alias_)) -> do
                    let (Can.Alias aliasVars aliasedType) = canAlias
                    let placeholderAliasType = Can.Holey aliasedType

                    let moduleName = Can._name canMod
                    let usage = AliasUsage moduleName valueName [] placeholderAliasType

                    let lookupCollection = LookupCollection (Map.singleton moduleName (Map.singleton valueName usage) )
                                                 & getExternalTypeUsages aliasedType

                    typeComponents <- lookUpTypesByFragments root lookupCollection
                    -- let typeComponents = []
                    let metadata = DeclarationMetadata valueName (Just aliasedType) (fmap toTypeComponentDefinition typeComponents) pos False
                    
                    pure 
                        (Just 
                            (Explanation
                                localizer
                                (Src.getName srcModule)
                                metadata
                                []
                            )
                        )

                Just val -> do
                    pure Nothing

        (_, _) ->
             pure Nothing


toTypeComponentDefinition :: (TypeUsage, Ext.Dev.Lookup.LookupResult) -> TypeComponentDefinition
toTypeComponentDefinition (usage, lookup) =
    TypeFromDef usage lookup


explainAtLocation :: String -> Watchtower.Editor.PointLocation -> IO (Maybe Explanation)
explainAtLocation root location@(Watchtower.Editor.PointLocation path _) = do
    single <- Ext.CompileProxy.loadSingle root path
    let (Ext.CompileProxy.Single source warnings interfaces canonical compiled) = single
    case (source, canonical) of
        (Right srcModule, Just canMod) -> do
            let missingTypeLookup = Ext.Dev.Help.toMissingTypeLookup single
            let localizer = Reporting.Render.Type.Localizer.fromModule srcModule
            let found = fmap (Ext.Dev.Find.Source.withCanonical canMod) (Ext.Dev.Find.Source.definitionAtPoint location srcModule)
            case found of
                Nothing ->
                    pure Nothing

                Just (Ext.Dev.Find.Source.FoundValue (Just def) (A.At pos val)) -> 
                    do
                        let (refCollection@(ReferenceCollection refMap usedTypes refUnions usedModules)) = getFoundDefTypes missingTypeLookup def emptyRefCollection
                        
                       
                        foundTypeRefs <- lookUpTypesByFragments root 
                                            (toLookupCollection refCollection)

                        Ext.Log.log Ext.Log.Misc (show (List.length foundTypeRefs))
            
                        let refs = Map.elems refMap 

                        metadata <- getDeclarationMetadata root missingTypeLookup pos def

                        pure 
                            (Just 
                                (Explanation
                                    localizer
                                    (Src.getName srcModule)
                                    metadata
                                    (refs <>  fmap pairToReference foundTypeRefs)
                                )
                            )
                
                Just (Ext.Dev.Find.Source.FoundUnion (Just union) (A.At pos srcUnion)) ->
                    pure Nothing

                Just (Ext.Dev.Find.Source.FoundAlias (Just canAlias) (A.At pos alias_)) ->
                    pure Nothing
                
                _ ->
                    pure Nothing

        (_, _) ->
             pure Nothing




toLookupCollection :: ReferenceCollection -> LookupCollection
toLookupCollection (ReferenceCollection refMap types refUnions usedMods) =
    let
        -- This needs to come first because we want other instances 
        -- to overwrite it if available
        -- This is because UnionVariantUsage does not have type variable defs
        lookupCollectionWithUnions = 
            Map.foldrWithKey
                (\(canMod, unionName) (unionDef, usages) found -> 
                    let
                        new = UnionVariantUsage canMod unionName usages
                    in
                    addFragment new found
                )
                emptyLookup
                refUnions

        lookup =
            List.foldr getExternalTypeUsages lookupCollectionWithUnions types
    in
    Map.elems refMap
        & List.foldr (flip getTypeLookups) lookup

unionToLookupCollection :: ModuleName.Canonical -> Name.Name -> Can.Union -> LookupCollection
unionToLookupCollection moduleName unionName union =
    let
        variantList = Can._u_alts union
        externalTypes = 
            List.concatMap 
                (\(Can.Ctor _ _ _ types) -> types)
                variantList

        toUsage (Can.Ctor name _ _ types) =
            UnionVariantUsage moduleName unionName (Set.singleton name)

        variants = Map.fromList 
                    (List.map 
                        (\ctor ->
                            (unionName, toUsage ctor)
                        ) 
                        variantList
                    )

        baseCollection =  LookupCollection (Map.singleton moduleName variants)
    in
    List.foldr 
        getExternalTypeUsages
        baseCollection
        externalTypes


getTypeLookups :: LookupCollection -> Reference -> LookupCollection
getTypeLookups found ref =
   case ref of
      ValueReference source name type_ ->
            case source of
                External canModName ->
                    found
                        & addFragment (Definition canModName name type_)
                        & getExternalTypeUsages type_ 

                LetValue ->
                    getExternalTypeUsages type_ found

                TopLevelDeclaration ->
                    getExternalTypeUsages type_ found
      
      TypeReference fragment lookup ->
            found



getFragmentCanonicalName :: TypeUsage -> ModuleName.Canonical 
getFragmentCanonicalName frag =
    case frag of
        TypeUsage can name types ->
            can

        AliasUsage can name vars aliasType ->
            can

        Definition can name type_ ->
            can
        
        UnionVariantUsage can name usagse ->
            can
        


getFragmentName :: TypeUsage -> Name
getFragmentName frag =
    case frag of
        TypeUsage can name types ->
            name

        AliasUsage can name vars aliasType ->
            name

        Definition can name type_ ->
            name

        UnionVariantUsage can name usages ->
            name


data LookupCollection =
    LookupCollection
        { _fragments :: Map.Map ModuleName.Canonical (Map.Map Name TypeUsage)
        }
        deriving (Eq, Show)


emptyLookup :: LookupCollection
emptyLookup =
    LookupCollection (Map.empty)


addFragment :: TypeUsage -> LookupCollection -> LookupCollection
addFragment fragment (LookupCollection fragments) =
    LookupCollection 
        (Map.alter 
            (\maybeValue ->
                case maybeValue of
                    Nothing ->
                        Just (Map.singleton (getFragmentName fragment) fragment)
                    
                    Just set ->
                        Just (Map.insert (getFragmentName fragment) fragment set)
            )
            (getFragmentCanonicalName fragment)
            fragments
        )

getExternalTypeUsages :: Can.Type -> LookupCollection -> LookupCollection
getExternalTypeUsages canType collection =
    case canType of
        Can.TLambda one two ->
            collection
                & getExternalTypeUsages one
                & getExternalTypeUsages two

        Can.TVar name ->
            collection

        Can.TType canMod name varTypes ->
            let 
                newCollections = 
                        addFragment (TypeUsage canMod name varTypes)
                            collection
            in
            List.foldr
                getExternalTypeUsages
                newCollections
                varTypes

        Can.TRecord fields extensibleName ->
            Map.foldr
                (\(Can.FieldType _ fieldType) innerCollection ->
                    getExternalTypeUsages fieldType innerCollection
                )
                collection
                fields

        Can.TUnit ->
            collection

        Can.TTuple one two Nothing ->
              collection 
                & getExternalTypeUsages one 
                & getExternalTypeUsages two

        Can.TTuple one two (Just three) ->
            collection
                & getExternalTypeUsages one 
                & getExternalTypeUsages two 
                & getExternalTypeUsages three
        
        Can.TAlias canMod name varTypes aliasType ->
            addFragment (AliasUsage canMod name varTypes aliasType)
                collection





getTypeComponents :: Bool -> String -> Can.Type -> IO [ TypeComponentDefinition ]
getTypeComponents isTopLevel root tipe = do
    let lookupCollection = getExternalTypeUsages tipe emptyLookup
    refs <- lookUpTypesByFragments root lookupCollection
    case tipe of
        Can.TLambda one two ->
            pure [ TypeInline tipe refs (Inline tipe) ]

        Can.TVar name ->
            pure [ TypeInline tipe refs (Inline tipe) ]

        Can.TType canMod name varTypes -> do
            list <- Monad.mapM (getTypeComponents False root) varTypes
           
            maybeDefinition <- Ext.Dev.Lookup.lookupDefinition root (ModuleName._module canMod) name
            let definition = case maybeDefinition of
                                Nothing -> Inline tipe
                                Just lookupResult -> 
                                    let 
                                        usage = TypeUsage canMod name varTypes
                                    in
                                    Defined usage lookupResult

            pure (TypeInline tipe refs definition : List.concat list)

        Can.TRecord fields extensibleName ->
            pure [ TypeInline tipe refs (Inline tipe) ]

        Can.TUnit ->
            pure [ TypeInline tipe refs (Inline tipe) ]

        Can.TTuple one two Nothing -> do
            oneComp <- getTypeComponents False root one
            twoComp <- getTypeComponents False root two
            pure (oneComp ++ twoComp)

        Can.TTuple one two (Just three) -> do
            oneComp <- getTypeComponents False root one
            twoComp <- getTypeComponents False root two
            threeComp <- getTypeComponents False root three
            pure (oneComp ++ twoComp ++ threeComp)
        
        Can.TAlias canMod name varTypes aliasType -> do
            -- TODO: do we keep track of this alias at all?
            getTypeComponents isTopLevel root (getTypeFromAlias aliasType)
            

getTypeFromAlias aliasType =
    case aliasType of
        Can.Holey canType ->
            canType

        Can.Filled canType ->
            canType

lookUpTypesByFragments :: String -> LookupCollection -> IO [ (TypeUsage, Ext.Dev.Lookup.LookupResult) ]
lookUpTypesByFragments root (LookupCollection fragments) =
    List.concat <$> Monad.mapM (findFragment root) (Map.toList fragments)
    

pairToReference :: (TypeUsage, Ext.Dev.Lookup.LookupResult) -> Reference
pairToReference (fragment, lookup) =
    TypeReference fragment lookup

findFragment :: String -> ( ModuleName.Canonical, Map.Map Name TypeUsage) -> IO [  (TypeUsage, Ext.Dev.Lookup.LookupResult) ]      
findFragment root (canMod, fragmentMap) =
    if isSkippable canMod then 
        pure []

    else do 
        definitions <- Ext.Dev.Lookup.lookupDefinitionMany root (ModuleName._module canMod) (Map.keys fragmentMap)
        pure 
            (List.foldr 
                (\(fragname, frag) gathered ->
                    case Map.lookup fragname definitions of
                        Nothing ->
                            gathered

                        Just fragDef ->
                            (frag, fragDef) : gathered
                ) 
                []
                (Map.toList fragmentMap)
            )

   
{-| Skip internalmodules

-}
isSkippable :: ModuleName.Canonical -> Bool
isSkippable (ModuleName.Canonical (Package.Name author project) modName) =
    let 
        projectString = Utf8.toChars project
    in
    Utf8.toChars author == "elm"
        && (projectString == "core"
             || projectString == "browser"
           )
    



getDeclarationMetadata root missingTypeLookup pos foundDef =
    case foundDef of
        Ext.Dev.Find.Source.Def def ->
            getCanDeclarationMetadata root missingTypeLookup pos def False

        Ext.Dev.Find.Source.DefRecursive def otherDefs ->
            getCanDeclarationMetadata root missingTypeLookup pos def True
            

getCanDeclarationMetadata root missingTypeLookup pos def recursive =
    case def of
        Can.Def (A.At _ name) patterns expr -> do 
             case Map.lookup name missingTypeLookup of
                Nothing ->
                    pure (DeclarationMetadata name Nothing [] pos recursive)

                Just type_ -> do
                    typeComponents <- getTypeComponents True root type_
                    pure (DeclarationMetadata name (Just type_) typeComponents pos recursive)
        
        Can.TypedDef (A.At _ name) freevars patternTypes expr returnType -> do
            let tipe = Ext.Dev.Help.toFunctionType (fmap snd patternTypes) returnType
            typeComponents <- getTypeComponents True root tipe
            pure (DeclarationMetadata name (Just tipe) typeComponents pos recursive)


{-| REFERENCE COLLECTION -}

data ReferenceCollection =
    ReferenceCollection 
        { _refs :: Map.Map (Source, Name) Reference
        , _types :: [ Can.Type ]
        , _unionRef ::  Map.Map (ModuleName.Canonical, Name) (Can.Union, Set.Set Name)
        , _used :: Set.Set (ModuleName.Canonical, Name)
        }
        deriving (Show)


emptyRefCollection :: ReferenceCollection
emptyRefCollection =
    ReferenceCollection Map.empty [] Map.empty Set.empty


addRef :: Reference -> ReferenceCollection -> ReferenceCollection
addRef ref (ReferenceCollection refs usedTypes unions usedModules) =
    case ref of
        ValueReference mod name type_ ->
            ReferenceCollection 
                (Map.insert (mod, name) ref refs)
                usedTypes
                unions
                usedModules
        
        TypeReference typeUsage lookup ->
            let 
                mod = getFragmentCanonicalName typeUsage

                name = getFragmentName typeUsage
            in
            ReferenceCollection 
                (Map.insert (External mod, name) ref refs)
                usedTypes
                unions
                usedModules


addType :: Can.Type -> ReferenceCollection -> ReferenceCollection
addType type_ (ReferenceCollection refs usedTypes unions usedModules) =
    ReferenceCollection 
        refs
        (type_ : usedTypes)
        unions
        usedModules


addUnion :: ModuleName.Canonical -> Name -> Can.Union -> Name -> ReferenceCollection -> ReferenceCollection
addUnion canName typeName unionDef variantName (ReferenceCollection refs usedTypes unions usedModules) =
    ReferenceCollection 
        refs
        usedTypes
        (Map.alter 
            (\maybeValue -> 
             case maybeValue of
                    Nothing ->
                        Just (unionDef, Set.singleton variantName)
                    
                    Just (existingUnionDef, set) ->
                        Just (existingUnionDef, Set.insert variantName set)
                
            )
            (canName, typeName)
            unions
        )
        usedModules





{-| Traverse and build reference collection
-}



getFoundDefTypes :: Map.Map Name.Name Can.Type -> Ext.Dev.Find.Source.Def -> ReferenceCollection -> ReferenceCollection
getFoundDefTypes missingTypeLookup foundDef collec =
    case foundDef of
        Ext.Dev.Find.Source.Def def ->
            getDefTypes missingTypeLookup True def collec

        Ext.Dev.Find.Source.DefRecursive def otherDefs ->
            getDefTypes missingTypeLookup True def collec


getDefTypes :: Map.Map Name.Name Can.Type -> Bool -> Can.Def ->  ReferenceCollection ->  ReferenceCollection
getDefTypes missingTypeLookup isTopLevel foundDef collec =
    case foundDef of
        Can.Def  (A.At _ name) patterns expr ->
            case Map.lookup name missingTypeLookup of
                Nothing ->
                    patterns
                        & List.foldr (getPatternTypes) collec 
                        & getExprTypes missingTypeLookup expr

                Just type_ ->
                     patterns
                        & List.foldr 
                            (\pattern innerCollec ->  
                                innerCollec
                                    & getPatternTypes pattern
                            ) 
                            collec
                        & getExprTypes missingTypeLookup expr
                        & addRef (ValueReference (if isTopLevel then TopLevelDeclaration else LetValue) name type_)
            
        Can.TypedDef (A.At _ name) freeVars patterns expr type_ ->
            patterns
                & List.foldr
                    (\(pattern, patternType) innerCollec ->
                        innerCollec
                            & addType patternType
                            & getPatternTypes pattern
                    ) 
                    collec
                & getExprTypes missingTypeLookup expr
                & addRef (ValueReference (if isTopLevel then TopLevelDeclaration else LetValue) name (gatherFunctionType patterns type_))


gatherFunctionType :: [ (Can.Pattern, Can.Type) ] -> Can.Type -> Can.Type
gatherFunctionType patterns resultType  =
    case patterns of
        [] -> resultType

        (pattern, type_) : remain ->
            Can.TLambda type_ 
                (gatherFunctionType remain resultType)
            

getPatternTypes :: Can.Pattern -> ReferenceCollection -> ReferenceCollection
getPatternTypes pattern collec =
    case A.toValue pattern of
        Can.PAnything ->
            collec

        Can.PVar name ->
            collec

        Can.PRecord names ->
            collec

        Can.PAlias innerPattern name ->
            getPatternTypes innerPattern collec

        Can.PUnit ->
            collec

        Can.PTuple onePattern twoPattern Nothing ->
             collec
                & getPatternTypes onePattern 
                & getPatternTypes twoPattern
        
        Can.PTuple onePattern twoPattern (Just threePattern) ->
            collec
                & getPatternTypes onePattern 
                & getPatternTypes twoPattern
                & getPatternTypes threePattern 

        Can.PList patterns ->
            patterns
                & List.foldr getPatternTypes collec 

        Can.PCons onePattern twoPattern ->
              collec
                & getPatternTypes onePattern 
                & getPatternTypes twoPattern

        Can.PBool union bool ->
            collec

        Can.PChr str ->
            collec

        Can.PStr str ->
            collec

        Can.PInt int ->
            collec

        Can.PCtor canName typeName union name index args ->
            -- We may want to return `Union` from ctor!
            collec
                & addUnion canName typeName union name



getExprTypes :: Map.Map Name.Name Can.Type -> Can.Expr -> ReferenceCollection -> ReferenceCollection
getExprTypes missingTypeLookup expr collec =
    case A.toValue expr of
        Can.VarLocal name -> 
            collec

        Can.VarTopLevel modName name -> 
            collec

        Can.VarKernel internalName name -> 
            collec

        Can.VarForeign modName name annotation -> 
            collec
                & annotationToType modName name annotation

        Can.VarCtor opts modName name zeroBasedIndex annotation -> 
            collec
                & annotationToType modName name annotation

        Can.VarDebug modName name annotation -> 
            collec
                & annotationToType modName name annotation

        Can.VarOperator opName modName name annotation  -> 
            collec
                & annotationToType modName name annotation

        Can.Chr str -> 
            collec

        Can.Str str -> 
            collec

        Can.Int int -> 
            collec

        Can.Float float -> 
            collec

        Can.List innerExprs -> 
            List.foldr (getExprTypes missingTypeLookup) collec innerExprs

        Can.Negate inner -> 
            getExprTypes missingTypeLookup inner collec

        Can.Binop binName modName name annotation one two  -> 
            collec
                & annotationToType modName name annotation 
                & getExprTypes missingTypeLookup one 
                & getExprTypes missingTypeLookup two 

        Can.Lambda patterns inner -> 
            List.foldr getPatternTypes collec patterns
                & getExprTypes missingTypeLookup inner

        Can.Call inner argExprs -> 
            List.foldr (getExprTypes missingTypeLookup) collec argExprs
                & getExprTypes missingTypeLookup inner 

        Can.If ifExprs inner -> 
            List.foldr
                (\(oneExpr, twoExpr) innerCollec ->
                    innerCollec
                        & getExprTypes missingTypeLookup oneExpr 
                        & getExprTypes missingTypeLookup twoExpr 
                )
                collec
                ifExprs
                & getExprTypes missingTypeLookup inner

        Can.Let innerDef inner -> 
            collec 
                & getDefTypes missingTypeLookup False innerDef
                & getExprTypes missingTypeLookup inner

        Can.LetRec innerDefs inner -> 
            List.foldr
                (\letDef innerCollec -> 
                    innerCollec
                        & getDefTypes missingTypeLookup False letDef
                )
                collec
                innerDefs
                & getExprTypes missingTypeLookup inner

        Can.LetDestruct pattern one two -> 
            collec 
                & getPatternTypes pattern 
                & getExprTypes missingTypeLookup one 
                & getExprTypes missingTypeLookup two

        Can.Case inner branches -> 
            List.foldr
                (\(Can.CaseBranch casePattern caseExpr) innerCollec -> 
                    innerCollec
                        & getPatternTypes casePattern 
                        & getExprTypes missingTypeLookup caseExpr
                )
                collec
                branches 
                & getExprTypes missingTypeLookup inner
                 
        Can.Accessor name -> 
            collec

        Can.Access inner locatedName -> 
            getExprTypes missingTypeLookup inner collec

        Can.Update name inner fields -> 
            List.foldr
                (\(Can.FieldUpdate region fieldExpr) innerCollec -> 
                    getExprTypes missingTypeLookup fieldExpr innerCollec
                )
                collec
                (Map.elems fields) 
                & getExprTypes missingTypeLookup inner

        Can.Record fields -> 
            Map.elems fields
                & List.foldr (getExprTypes missingTypeLookup) collec 

        Can.Unit -> 
            collec

        Can.Tuple one two Nothing -> 
            collec
                & getExprTypes missingTypeLookup one 
                & getExprTypes missingTypeLookup two 
        
        Can.Tuple one two (Just three) -> 
           collec
                & getExprTypes missingTypeLookup one 
                & getExprTypes missingTypeLookup two 
                & getExprTypes missingTypeLookup three


        Can.Shader source types -> 
            collec


annotationToType modName name (Can.Forall vars type_) collec =
    addRef (ValueReference (External modName) name type_ ) collec
          


{- JSON ENCODING -}

encode :: Explanation -> Json.Encode.Value
encode (Explanation localizer moduleName def references) =
    Json.Encode.object
        [ "moduleName" ==> Json.Encode.name moduleName
        , "definition" ==> encodeDefinitionMetadata localizer def
        -- , "facts" ==>
        --     Json.Encode.list
        --         (encodeFact localizer)
        --         references
        ]


{-| This is used by `Usage` to embed an explanation in another piece of JSON which already has information like locaiton and name and stuff.

So, this is just the definition explanation

-}
encodeInlineDefinition :: Explanation -> Json.Encode.Value
encodeInlineDefinition (Explanation localizer moduleName def references) =
    encodeDefinitionType localizer def
    
encodeDefinitionMetadata :: Reporting.Render.Type.Localizer.Localizer ->  DeclarationMetadata -> Json.Encode.Value
encodeDefinitionMetadata localizer (def@(DeclarationMetadata name maybeType typeComponents region recursive)) =
    Json.Encode.object 
        [ "name" ==> Json.Encode.chars (Name.toChars name)
        , "type" ==> encodeDefinitionType localizer def
        , "region" ==> Watchtower.Editor.encodeRegion region

        -- Don't know if the below is useful
        -- , "recursive" ==>  Json.Encode.bool recursive
        ]   

encodeDefinitionType :: Reporting.Render.Type.Localizer.Localizer -> DeclarationMetadata -> Json.Encode.Value
encodeDefinitionType localizer (DeclarationMetadata name maybeType typeComponents region recursive) =
    Json.Encode.object 
        [ "signature" ==>
                case maybeType of
                Nothing -> Json.Encode.null
                Just canType ->
                    encodeCanType localizer canType

        , "components" ==>
            (Json.Encode.list 
                (\component ->  
                    case component of
                        TypeFromDef typeUsage typeLookup ->
                            Json.Encode.object (lookupToJsonFields localizer typeUsage typeLookup)
                        
                        TypeInline canType refs definition ->
                            Json.Encode.object 
                                [ "signature" ==> encodeCanType localizer canType
                                , "definition" ==> case definition of
                                                    Inline tipe -> encodeTypeDefinition localizer tipe
                                                    
                                                    Defined usage defined -> encodeLookupResult localizer usage defined

                                -- , "references" ==>
                                --     Json.Encode.list 
                                --         (\(typeUsage, typeLookup)-> 
                                --             Json.Encode.object (lookupToJsonFields localizer typeUsage typeLookup)    
                                --         )
                                --         refs 
                                ]

                        
                
                )
                typeComponents
            )

        ]

toVars :: Can.Type -> [ Name ]
toVars canType =
    case canType of
        Can.TLambda one two ->
            toVars one <> toVars two

        Can.TVar name ->
            [ name ]

        Can.TType canMod name varTypes ->
            [ name ] ++ List.concatMap toVars varTypes

        Can.TRecord fields extensibleName ->
            List.concatMap (\(Can.FieldType _ fieldType) -> toVars fieldType) (Map.elems fields)

        Can.TUnit ->
            []

        Can.TTuple one two Nothing ->
            toVars one <> toVars two

        Can.TTuple one two (Just three) ->
            toVars one <> toVars two <> toVars three

        Can.TAlias canMod name varTypes aliasType ->
            [ name ]


lookupToJsonFields ::
    Reporting.Render.Type.Localizer.Localizer 
        -> TypeUsage 
        -> Ext.Dev.Lookup.LookupResult 
        -> [ (Json.String.String, Json.Encode.Value) ]
lookupToJsonFields localizer usage lookup =
    [ "source" ==> Ext.Dev.Json.Encode.moduleName (getFragmentCanonicalName usage)
    , "definition" ==>  encodeLookupResult localizer usage lookup
    ]

encodeLookupResult localizer usage lookup =
    case lookup of
        Ext.Dev.Lookup.Union maybeComment canUnion ->
            encodeCanUnion localizer usage maybeComment canUnion

        Ext.Dev.Lookup.Alias maybeComment alias ->
            encodeAlias localizer usage maybeComment alias

        Ext.Dev.Lookup.Def comment ->
            encodeDefinition localizer usage comment

encodeFact :: Reporting.Render.Type.Localizer.Localizer -> Reference -> Json.Encode.Value
encodeFact localizer ref =
    case ref of
        ValueReference source name canType ->
            Json.Encode.object 
                [ "source" ==> 
                    case source of
                        External modName ->
                            Ext.Dev.Json.Encode.moduleName modName

                        LetValue ->
                            Json.Encode.chars "let"

                        TopLevelDeclaration ->
                            Json.Encode.chars "declaration"
                , "name" ==> Json.Encode.chars (Name.toChars name)
                , "type" ==> encodeCanType localizer canType
                ]

        TypeReference fragment lookup ->
            Json.Encode.object (lookupToJsonFields localizer fragment lookup)


encodeComment (Src.Comment docComment) =
    Json.Encode.string 
        (Json.String.fromComment docComment)


encodeMaybe encoder maybe =
    case maybe of
        Nothing -> Json.Encode.null
        Just val ->
            encoder val


encodeDefinition :: Reporting.Render.Type.Localizer.Localizer -> TypeUsage ->  Src.Comment -> Json.Encode.Value
encodeDefinition localizer fragment comment  =
    Json.Encode.object 
        [ "type" ==> Json.Encode.chars "definition"
        , "comment" ==> encodeComment comment
        , "signature" ==> (case fragment of
                            Definition canName name type_ ->
                                encodeCanType localizer type_
                            _ -> 
                                Json.Encode.null
                    )
        ] 



encodeAlias :: Reporting.Render.Type.Localizer.Localizer -> TypeUsage ->  Maybe Src.Comment -> Can.Alias -> Json.Encode.Value
encodeAlias localizer fragment maybeComment (Can.Alias vars type_) =
    Json.Encode.object 
        [ "type" ==> Json.Encode.chars "alias"
        , "comment" ==> encodeMaybe encodeComment maybeComment
        , "module" ==> Json.Encode.name (ModuleName._module (getFragmentCanonicalName fragment))
        , "name" ==> Json.Encode.name (getFragmentName fragment)
        , "args" ==> encodeVarList localizer vars fragment
        , "signature" ==> encodeCanType localizer type_
        , "fields" ==> 
            case type_ of
                Can.TRecord fields extensibleName ->
                    encodeTypeDefinition localizer type_
                _ ->
                    Json.Encode.null
        ] 


encodeTypeDefinition :: Reporting.Render.Type.Localizer.Localizer -> Can.Type -> Json.Encode.Value
encodeTypeDefinition localizer type_ =
    case type_ of
        Can.TLambda one two ->
            encodeCanType localizer type_
                
        Can.TVar name ->
            encodeCanType localizer type_

        Can.TType canMod name varTypes ->
            encodeCanType localizer type_

        Can.TRecord fields extensibleName ->
            Json.Encode.dict
                Json.String.fromName
                (\(Can.FieldType _ field) -> encodeTypeDefinition localizer field)
                fields

        Can.TUnit ->
            encodeCanType localizer type_

        Can.TTuple one two Nothing ->
            encodeCanType localizer type_

        Can.TTuple one two (Just three) ->
            encodeCanType localizer type_

        Can.TAlias canMod name varTypes aliasType ->
           encodeCanType localizer type_



encodeCanUnion :: Reporting.Render.Type.Localizer.Localizer -> TypeUsage -> Maybe Src.Comment -> Can.Union -> Json.Encode.Value
encodeCanUnion localizer fragment maybeComment (Can.Union vars ctors i opts) =
    Json.Encode.object 
        [ "type" ==> Json.Encode.chars "union"
        , "comment" ==> encodeMaybe encodeComment maybeComment
        , "module" ==> Json.Encode.name (ModuleName._module (getFragmentCanonicalName fragment))
        , "name" ==> Json.Encode.name (getFragmentName fragment)
        , "args" ==> encodeVarList localizer vars fragment
        , "variants" ==> Json.Encode.list (encodeUnionVariant localizer) ctors
        ] 


encodeVarList :: Reporting.Render.Type.Localizer.Localizer -> [ Name ] -> TypeUsage -> Json.Encode.Value
encodeVarList localizer varNames fragment =
    case fragment of
        TypeUsage canName name varTypes ->
            Json.Encode.list
                (encodeVar localizer)
                (List.zip varNames varTypes)

        AliasUsage canName name varNameTypes aliasType ->
            let 
                varTypes = fmap snd varNameTypes
            in
            Json.Encode.list
                (encodeVar localizer)
                (List.zip varNames varTypes)

        UnionVariantUsage canName name usages ->
            Json.Encode.list id []

        Definition canName name type_ ->
            Json.Encode.list id []



encodeVar :: Reporting.Render.Type.Localizer.Localizer -> (Name, Can.Type) -> Json.Encode.Value
encodeVar localizer (name, type_) =
     Json.Encode.object 
        [ "name" ==> Json.Encode.name name
        , "type" ==> encodeCanType localizer type_
        ]


encodeUnionVariant :: Reporting.Render.Type.Localizer.Localizer -> Can.Ctor  -> Json.Encode.Value
encodeUnionVariant localizer (Can.Ctor name _ _ args) =
    Json.Encode.object
        [ "name" ==> Json.Encode.name name
        , "args" ==> Json.Encode.list (encodeCanType localizer) args
        ]

         
    

encodeCanType :: Reporting.Render.Type.Localizer.Localizer -> Can.Type -> Json.Encode.Value
encodeCanType localizer type_ =
    Json.Encode.chars
        (Reporting.Doc.toLine
            (Reporting.Render.Type.canToDoc localizer Reporting.Render.Type.None type_)
        )
    
    