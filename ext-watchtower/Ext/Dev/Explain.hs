{-# LANGUAGE OverloadedStrings #-}

module Ext.Dev.Explain
  ( explain
  , encode
  )
where


import qualified AST.Source as Src
import qualified AST.Canonical as Can

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

import qualified Watchtower.Editor
import qualified Ext.Log



data Explanation =
    Explanation
        { _localizer :: Reporting.Render.Type.Localizer.Localizer
        , _declaration :: DeclarationMetadata
        , _references :: [ Reference ]
        }

data DeclarationMetadata =
    DeclarationMetadata
        { _declarationName :: Name
        , _declarationType :: Maybe Can.Type
        , _declarationRange :: A.Region
        , _declarationRecursive :: Bool
        }


data Reference
    = ValueReference   
        { _source :: Source
        , _name :: Name
        , _type :: Can.Type
        }
    | TypeReference 
        TypeFragment
        Ext.Dev.Lookup.LookupResult

data Source 
    = External ModuleName.Canonical
    | LetValue
    deriving (Eq, Ord, Show)
  


toLookupCollection :: ReferenceCollection -> LookupCollection
toLookupCollection (ReferenceCollection refMap types usedMods) =
    let
        refs = Map.elems refMap 

        lookup =
            List.foldl (flip getExternalTypeFragments) emptyLookup types
    in
    refs
        & List.foldl getTypeLookups lookup

getTypeLookups :: LookupCollection -> Reference -> LookupCollection
getTypeLookups found ref =
   case ref of
      ValueReference source name type_ ->
            case source of
                External canModName ->
                    found
                        & addFragment (Definition canModName name type_)
                        & getExternalTypeFragments type_ 

                LetValue ->
                    getExternalTypeFragments type_ found
      
      TypeReference fragment lookup ->
            found

data TypeFragment
    = TypeFragment ModuleName.Canonical Name [ Can.Type ]
    | AliasFragment ModuleName.Canonical Name [(Name, Can.Type)] Can.AliasType
    | Definition  ModuleName.Canonical Name Can.Type
    deriving (Eq, Show)

getFragmentCanonicalName :: TypeFragment -> ModuleName.Canonical 
getFragmentCanonicalName frag =
    case frag of
        TypeFragment can name types ->
            can

        AliasFragment can name vars aliasType ->
            can

        Definition can name type_ ->
            can


getFragmentName :: TypeFragment -> Name
getFragmentName frag =
    case frag of
        TypeFragment can name types ->
            name

        AliasFragment can name vars aliasType ->
            name

        Definition can name type_ ->
            name


data LookupCollection =
    LookupCollection
        { _fragments :: Map.Map ModuleName.Canonical (Map.Map Name TypeFragment)
        }
        deriving (Eq, Show)


emptyLookup :: LookupCollection
emptyLookup =
    LookupCollection (Map.empty)


addFragment :: TypeFragment -> LookupCollection -> LookupCollection
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

getExternalTypeFragments :: Can.Type -> LookupCollection -> LookupCollection
getExternalTypeFragments canType collection =
    case canType of
        Can.TLambda one two ->
            collection
                & getExternalTypeFragments one
                & getExternalTypeFragments two

        Can.TVar name ->
            collection

        Can.TType canMod name varTypes ->
            addFragment (TypeFragment canMod name varTypes)
                collection

        Can.TRecord fields extensibleName ->
            Map.foldl 
                (\innerCollection (Can.FieldType _ fieldType) -> 
                    getExternalTypeFragments fieldType innerCollection
                )
                collection
                fields

        Can.TUnit ->
            collection

        Can.TTuple one two Nothing ->
              collection 
                & getExternalTypeFragments one 
                & getExternalTypeFragments two

        Can.TTuple one two (Just three) ->
            collection
                & getExternalTypeFragments one 
                & getExternalTypeFragments two 
                & getExternalTypeFragments three
        
        Can.TAlias canMod name varTypes aliasType ->
            addFragment (AliasFragment canMod name varTypes aliasType)
                collection


lookUpTypesByFragments :: String -> LookupCollection -> IO [ Reference ]
lookUpTypesByFragments root (LookupCollection fragments) =
    List.concat <$> Monad.mapM (findFragment root) (Map.toList fragments)
    

        
findFragment root (canMod, fragmentMap) =
    if isSkippable canMod then 
        pure []

    else
        do 
            definitions <- Ext.Dev.Lookup.lookupMany root canMod (Map.keys fragmentMap)
            pure 
                (List.foldl 
                    (\gathered (fragname, frag) ->
                        case Map.lookup fragname definitions of
                            Nothing ->
                                gathered

                            Just fragDef ->
                                TypeReference frag fragDef : gathered
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
    




explain :: String -> Watchtower.Editor.PointLocation -> IO (Maybe Explanation)
explain root location@(Watchtower.Editor.PointLocation path _) = do
    (Ext.CompileProxy.Single source warnings canonical compiled) <- Ext.CompileProxy.loadSingle root path
    case (source, canonical) of
        (Right srcModule, Just canMod) -> do
            let localizer = Reporting.Render.Type.Localizer.fromModule srcModule
            let found = fmap (Ext.Dev.Find.Source.withCanonical canMod) (Ext.Dev.Find.Source.definitionAtPoint location srcModule)
            case found of
                Nothing ->
                    pure Nothing

                Just (Ext.Dev.Find.Source.FoundValue (Just def) (A.At pos val)) -> 
                    do
                        let (refCollection@(ReferenceCollection refMap usedTypes usedModules)) = getFoundDefTypes def emptyRefCollection
                        let refs = Map.elems refMap 
                        foundTypeRefs <- lookUpTypesByFragments root 
                                            (toLookupCollection refCollection)

                        Ext.Log.log Ext.Log.Misc (show (List.length foundTypeRefs))
            
                    
                        pure 
                            (Just 
                                (Explanation
                                    localizer
                                    (getDeclarationMetadata pos def)
                                    (refs <> foundTypeRefs)
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



getDeclarationMetadata pos foundDef =
    case foundDef of
        Ext.Dev.Find.Source.Def def ->
            getCanDeclarationMetadata pos def False

        Ext.Dev.Find.Source.DefRecursive def otherDefs ->
            getCanDeclarationMetadata pos def True
            

getCanDeclarationMetadata pos def recursive =
    case def of
        Can.Def (A.At _ name) patterns expr ->
            DeclarationMetadata name Nothing pos recursive
        
        Can.TypedDef (A.At _ name) freevars patterns expr type_ ->
            DeclarationMetadata name (Just type_) pos recursive


{-| REFERENCE COLLECTION -}

data ReferenceCollection =
    ReferenceCollection 
        { _refs :: Map.Map (Source, Name) Reference
        , _types :: [ Can.Type ]
        , _used :: Set.Set (ModuleName.Canonical, Name)
        }


emptyRefCollection :: ReferenceCollection
emptyRefCollection =
    ReferenceCollection Map.empty [] Set.empty


addRef :: Reference -> ReferenceCollection -> ReferenceCollection
addRef (ref@(ValueReference mod name type_)) (ReferenceCollection refs usedTypes usedModules) =
    ReferenceCollection 
        (Map.insert (mod, name) ref refs)
        usedTypes
        usedModules


addType :: Can.Type -> ReferenceCollection -> ReferenceCollection
addType type_ (ReferenceCollection refs usedTypes usedModules) =
    ReferenceCollection 
        refs
        (type_ : usedTypes)
        usedModules




{-| Traverse and build reference collection
-}



getFoundDefTypes :: Ext.Dev.Find.Source.Def -> ReferenceCollection  ->  ReferenceCollection
getFoundDefTypes foundDef collec =
    case foundDef of
        Ext.Dev.Find.Source.Def def ->
            getDefTypes def collec

        Ext.Dev.Find.Source.DefRecursive def otherDefs ->
            getDefTypes def collec


getDefTypes :: Can.Def ->  ReferenceCollection ->  ReferenceCollection
getDefTypes foundDef collec =
    case foundDef of
        Can.Def locatedName patterns expr ->
            patterns
                & List.foldl (flip getPatternTypes) collec 
                & getExprTypes expr

        Can.TypedDef (A.At namePos name) freeVars patterns expr type_ ->
            patterns
                & List.foldl 
                    (\innerCollec (pattern, patternType) ->  
                        innerCollec
                            & addType patternType
                            & getPatternTypes pattern
                    ) 
                    collec
                & getExprTypes expr
                & addRef (ValueReference LetValue name type_)


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
                & List.foldl (flip getPatternTypes) collec 

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



getExprTypes :: Can.Expr -> ReferenceCollection -> ReferenceCollection
getExprTypes expr collec =
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
            List.foldl (flip getExprTypes) collec innerExprs

        Can.Negate inner -> 
            getExprTypes inner collec

        Can.Binop binName modName name annotation one two  -> 
            collec
                & annotationToType modName name annotation 
                & getExprTypes one 
                & getExprTypes two 

        Can.Lambda patterns inner -> 
            List.foldl (flip getPatternTypes) collec patterns
                & getExprTypes inner

        Can.Call inner argExprs -> 
            List.foldl (flip getExprTypes) collec argExprs
                & getExprTypes inner 

        Can.If ifExprs inner -> 
            List.foldl 
                (\innerCollec (oneExpr, twoExpr) -> 
                    innerCollec
                        & getExprTypes oneExpr 
                        & getExprTypes twoExpr 
                )
                collec
                ifExprs
                & getExprTypes inner

        Can.Let innerDef inner -> 
            collec 
                & getDefTypes innerDef
                & getExprTypes inner

        Can.LetRec innerDefs inner -> 
            List.foldl 
                (\innerCollec letDef -> 
                    innerCollec
                        & getDefTypes letDef
                )
                collec
                innerDefs
                & getExprTypes inner

        Can.LetDestruct pattern one two -> 
            collec 
                & getPatternTypes pattern 
                & getExprTypes one 
                & getExprTypes two

        Can.Case inner branches -> 
            List.foldl 
                (\innerCollec (Can.CaseBranch casePattern caseExpr) -> 
                    innerCollec
                        & getPatternTypes casePattern 
                        & getExprTypes caseExpr
                )
                collec
                branches 
                & getExprTypes inner
                 
        Can.Accessor name -> 
            collec

        Can.Access inner locatedName -> 
            getExprTypes inner collec

        Can.Update name inner fields -> 
            List.foldl 
                (\innerCollec (Can.FieldUpdate region fieldExpr) -> 
                    getExprTypes fieldExpr innerCollec
                )
                collec
                (Map.elems fields) 
                & getExprTypes inner

        Can.Record fields -> 
            Map.elems fields
                & List.foldl (flip getExprTypes) collec 

        Can.Unit -> 
            collec

        Can.Tuple one two Nothing -> 
            collec
                & getExprTypes one 
                & getExprTypes two 
        
        Can.Tuple one two (Just three) -> 
           collec
                & getExprTypes one 
                & getExprTypes two 
                & getExprTypes three


        Can.Shader source types -> 
            collec


annotationToType modName name (Can.Forall vars type_) collec =
    addRef (ValueReference (External modName) name type_ ) collec
          


{- JSON ENCODING -}

encode :: Explanation -> Json.Encode.Value
encode (Explanation localizer def references) =
    Json.Encode.object
        [ "definition" ==> encodeDefinitionMetadata localizer def
        , "facts" ==>
            Json.Encode.list
                (encodeReference localizer)
                references
        ]
    
encodeDefinitionMetadata ::  Reporting.Render.Type.Localizer.Localizer ->  DeclarationMetadata -> Json.Encode.Value
encodeDefinitionMetadata localizer (DeclarationMetadata name maybeType region recursive) =
    Json.Encode.object 
        [ "name" ==> Json.Encode.chars (Name.toChars name)
        , "type" ==> 
            case maybeType of
                Nothing -> Json.Encode.null
                Just canType ->
                    encodeCanType localizer canType
        , "region" ==> Watchtower.Editor.encodeRegion region
        , "recursive" ==>  Json.Encode.bool recursive
        ]   


encodeReference :: Reporting.Render.Type.Localizer.Localizer -> Reference -> Json.Encode.Value
encodeReference localizer ref =
    case ref of
        ValueReference source name canType ->
            Json.Encode.object 
                [ "source" ==> 
                    case source of
                        External modName ->
                            encodeCanName modName

                        LetValue ->
                            Json.Encode.chars "local"
                , "name" ==> Json.Encode.chars (Name.toChars name)
                , "type" ==> encodeCanType localizer canType
                ]

        TypeReference fragment (Ext.Dev.Lookup.Union maybeComment canUnion) ->
            Json.Encode.object 
                [ "union" ==> 
                    encodeCanUnion localizer fragment maybeComment canUnion
                ]
        
        TypeReference fragment (Ext.Dev.Lookup.Alias maybeComment alias) ->
            Json.Encode.object 
                [ "alias" ==> 
                    encodeAlias localizer fragment maybeComment alias
                ]

        TypeReference fragment (Ext.Dev.Lookup.Def comment) ->
            Json.Encode.object 
                [ "definition" ==> 
                    encodeDefinition localizer fragment comment
                ]


encodeComment (Src.Comment docComment) =
    Json.Encode.string 
        (Json.String.fromComment docComment)


encodeMaybe encoder maybe =
    case maybe of
        Nothing -> Json.Encode.null
        Just val ->
            encoder val


encodeDefinition :: Reporting.Render.Type.Localizer.Localizer -> TypeFragment ->  Src.Comment -> Json.Encode.Value
encodeDefinition localizer fragment comment  =
    Json.Encode.object 
        [ "name" ==> Json.Encode.name (getFragmentName fragment)
        , "type" ==> (case fragment of
                            Definition canName name type_ ->
                                encodeCanType localizer type_
                            _ -> 
                                Json.Encode.null
                    )
        , "comment" ==> encodeComment comment
        ] 



encodeAlias :: Reporting.Render.Type.Localizer.Localizer -> TypeFragment ->  Maybe Src.Comment -> Can.Alias -> Json.Encode.Value
encodeAlias localizer fragment maybeComment (Can.Alias vars type_) =
    Json.Encode.object 
        [ "name" ==> Json.Encode.name (getFragmentName fragment)
        , "args" ==> encodeVarList localizer vars fragment
        , "type" ==> encodeCanType localizer type_
        , "comment" ==> encodeMaybe encodeComment maybeComment
        ] 


  
encodeCanName :: ModuleName.Canonical -> Json.Encode.Value
encodeCanName (ModuleName.Canonical pkgName modName) =
    Json.Encode.object 
        [ "pkg" ==> Package.encode pkgName
        , "module" ==>  Json.Encode.chars (Name.toChars modName)
        ]   

encodeCanUnion :: Reporting.Render.Type.Localizer.Localizer -> TypeFragment -> Maybe Src.Comment -> Can.Union -> Json.Encode.Value
encodeCanUnion localizer fragment maybeComment (Can.Union vars ctors i opts) =
    Json.Encode.object 
        [ "name" ==> Json.Encode.name (getFragmentName fragment)
        , "args" ==> 
            encodeVarList localizer vars fragment
        , "cases" ==> Json.Encode.list (encodeUnionVariant localizer) ctors
        , "comment" ==> encodeMaybe encodeComment maybeComment
        ] 


encodeVarList :: Reporting.Render.Type.Localizer.Localizer -> [ Name ] -> TypeFragment -> Json.Encode.Value
encodeVarList localizer varNames fragment =
    case fragment of
        TypeFragment canName name varTypes ->
            Json.Encode.list
                (encodeVar localizer)
                (List.zip varNames varTypes)

        AliasFragment canName name varNameTypes aliasType ->
            let 
                varTypes = fmap snd varNameTypes
            in
            Json.Encode.list
                (encodeVar localizer)
                (List.zip varNames varTypes)



encodeVar :: Reporting.Render.Type.Localizer.Localizer -> (Name, Can.Type) -> Json.Encode.Value
encodeVar localizer (name, type_) =
     Json.Encode.object 
        [ "name" ==> Json.Encode.name name
        , "type" ==> encodeCanType localizer type_
        ]


encodeUnionVariant :: Reporting.Render.Type.Localizer.Localizer -> Can.Ctor  -> Json.Encode.Value
encodeUnionVariant localizer (Can.Ctor name _ _ args) =
    Json.Encode.list id 
        [ Json.Encode.name name
        , Json.Encode.list (encodeCanType localizer) args
        ]



encodeCanType :: Reporting.Render.Type.Localizer.Localizer -> Can.Type -> Json.Encode.Value
encodeCanType localizer type_ =
    Reporting.Doc.encode
        (Reporting.Render.Type.canToDoc localizer Reporting.Render.Type.None type_)
    
    