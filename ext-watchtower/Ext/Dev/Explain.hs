{-# LANGUAGE OverloadedStrings #-}

module Ext.Dev.Explain
  ( explain
  , encode
  )
where


import qualified AST.Source as Src
import qualified AST.Canonical as Can

import qualified Elm.Package as Package
import qualified Elm.ModuleName as ModuleName
import qualified Reporting.Annotation as A
import qualified Reporting.Render.Type
import qualified Reporting.Render.Type.Localizer

import qualified Reporting.Doc

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Name as Name
import Data.Name (Name)

import Data.Function ((&))
import qualified Json.Encode
import Json.Encode ((==>))

import qualified Ext.CompileProxy
import qualified Ext.Dev.Find.Source
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

data Source 
    = External ModuleName.Canonical
    | LetValue
    deriving (Eq, Ord, Show)
  

data TypeDefinition
    = Union Can.Union
    | Alias Can.Alias
    deriving (Show)

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
                        let (ReferenceCollection refMap usedTypes usedModules) = getFoundDefTypes def emptyRefCollection
                        pure 
                            (Just 
                                (Explanation
                                    localizer
                                    (getDeclarationMetadata pos def)
                                    (Map.elems refMap)
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
        , _usedTypes :: Set.Set (ModuleName.Canonical, Name)
        , _usedModules :: Map.Map ModuleName.Canonical (Set.Set Name)
        }


emptyRefCollection :: ReferenceCollection
emptyRefCollection =
    ReferenceCollection Map.empty Set.empty Map.empty

addRef :: Reference -> ReferenceCollection -> ReferenceCollection
addRef (ref@(ValueReference mod name type_)) (ReferenceCollection refs usedTypes usedModules) =
    ReferenceCollection 
        (Map.insert (mod, name) ref refs)
        usedTypes
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
            (fmap fst patterns)
                & List.foldl (flip getPatternTypes) collec 
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
        

  
encodeCanName :: ModuleName.Canonical -> Json.Encode.Value
encodeCanName (ModuleName.Canonical pkgName modName) =
    Json.Encode.object 
        [ "pkg" ==> Package.encode pkgName
        , "module" ==>  Json.Encode.chars (Name.toChars modName)
        ]   



encodeDefinition :: TypeDefinition -> Json.Encode.Value
encodeDefinition typeDef =
    case typeDef of
        Union union ->
            encodeCanUnion union

        Alias alias ->
            Json.Encode.chars "alias"


encodeCanUnion (Can.Union vars ctors i opts) =
    Json.Encode.object 
        [ "type" ==> Json.Encode.chars "UNION"
        , "vars" ==> Json.Encode.list (Json.Encode.chars . Name.toChars) vars
        , "variants" ==> Json.Encode.list encodeUnionVariant ctors
        -- , "module" ==>  Json.Encode.chars (Name.toChars modName)
        ] 


encodeUnionVariant (Can.Ctor name _ _ values) =
    Json.Encode.object 
        [ "name" ==> Json.Encode.chars (Name.toChars name)
        -- , "values" ==> Json.Encode.list encodeCanType values
        ]



encodeCanType localizer type_ =
    Json.Encode.chars
        (Reporting.Doc.toString
            (Reporting.Render.Type.canToDoc localizer Reporting.Render.Type.None type_)
        )
    