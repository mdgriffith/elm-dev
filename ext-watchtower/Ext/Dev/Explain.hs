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
        , _references :: [ Reference ]
        }


data Reference =
    TypeReference   
        { _mod :: ModuleName.Canonical
        , _name :: Name
        , _type :: Can.Type
        }
        deriving (Show)


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
                    pure 
                        (Just 
                            (Explanation
                                localizer
                                (getFoundDefTypes def)
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


getFoundDefTypes :: Ext.Dev.Find.Source.Def ->  [ Reference ]
getFoundDefTypes foundDef =
    case foundDef of
        Ext.Dev.Find.Source.Def def ->
            getDefTypes def

        Ext.Dev.Find.Source.DefRecursive def otherDefs ->
            getDefTypes def


getDefTypes :: Can.Def ->  [ Reference ]
getDefTypes foundDef =
    case foundDef of
        Can.Def locatedName patterns expr ->
            List.concatMap getPatternTypes patterns
                <> getExprTypes expr

        Can.TypedDef locatedName freeVars patterns expr type_ ->
            getExprTypes expr


getPatternTypes :: Can.Pattern -> [ Reference ]
getPatternTypes pattern =
    case A.toValue pattern of
        Can.PAnything ->
            []

        Can.PVar name ->
            []

        Can.PRecord names ->
            []

        Can.PAlias innerPattern name ->
            getPatternTypes innerPattern

        Can.PUnit ->
            []

        Can.PTuple onePattern twoPattern Nothing ->
            getPatternTypes onePattern <> getPatternTypes twoPattern 
        
        Can.PTuple onePattern twoPattern (Just threePattern) ->
            getPatternTypes onePattern <> getPatternTypes twoPattern <> getPatternTypes threePattern 

        Can.PList patterns ->
            List.concatMap getPatternTypes patterns

        Can.PCons onePattern twoPattern ->
             getPatternTypes onePattern <> getPatternTypes twoPattern 

        Can.PBool union bool ->
            []

        Can.PChr str ->
            []

        Can.PStr str ->
            []

        Can.PInt int ->
            []

        Can.PCtor canName typeName union name index args ->
            -- We may want to return `Union` from ctor!
            []



getExprTypes :: Can.Expr -> [ Reference ]
getExprTypes expr =
    case A.toValue expr of
        Can.VarLocal name -> 
            []

        Can.VarTopLevel modName name -> 
            []

        Can.VarKernel internalName name -> 
            []

        Can.VarForeign modName name annotation -> 
            [ annotationToType modName name annotation ]

        Can.VarCtor opts modName name zeroBasedIndex annotation -> 
            [ annotationToType modName name annotation ]

        Can.VarDebug modName name annotation -> 
            [ annotationToType modName name annotation ]

        Can.VarOperator opName modName name annotation  -> 
            [ annotationToType modName name annotation ]

        Can.Chr str -> 
            []

        Can.Str str -> 
            []

        Can.Int int -> 
            []

        Can.Float float -> 
            []

        Can.List innerExprs -> 
            List.concatMap getExprTypes innerExprs

        Can.Negate inner -> 
            getExprTypes inner

        Can.Binop binName modName name annotation one two  -> 
            annotationToType modName name annotation : getExprTypes one <> getExprTypes two 

        Can.Lambda patterns inner -> 
            List.concatMap getPatternTypes patterns <> getExprTypes inner

        Can.Call inner argExprs -> 
            getExprTypes inner <> List.concatMap getExprTypes argExprs

        Can.If ifExprs inner -> 
             getExprTypes inner
                 <> List.concatMap 
                        (\(oneExpr, twoExpr) -> 
                             getExprTypes oneExpr <> getExprTypes twoExpr 
                        )
                        ifExprs 

        Can.Let innerDef inner -> 
            getDefTypes innerDef <>  getExprTypes inner

        Can.LetRec innerDefs inner -> 
            List.concatMap getDefTypes innerDefs <>  getExprTypes inner

        Can.LetDestruct pattern one two -> 
            getPatternTypes pattern <> getExprTypes one <> getExprTypes two

        Can.Case inner branches -> 
            getExprTypes inner
                <> List.concatMap 
                        (\(Can.CaseBranch casePattern caseExpr) -> 
                             getPatternTypes casePattern <> getExprTypes caseExpr
                        )
                        branches 
                

        Can.Accessor name -> 
            []

        Can.Access inner locatedName -> 
            getExprTypes inner

        Can.Update name inner fields -> 
            getExprTypes inner
                 <> List.concatMap 
                        (\(Can.FieldUpdate region fieldExpr) -> 
                             getExprTypes fieldExpr
                        )
                        (Map.elems fields) 

        Can.Record fields -> 
            List.concatMap getExprTypes (Map.elems fields)

        Can.Unit -> 
            []

        Can.Tuple one two Nothing -> 
            getExprTypes one <> getExprTypes two
        
        Can.Tuple one two (Just three) -> 
            getExprTypes one <> getExprTypes two <> getExprTypes three

        Can.Shader source types -> 
            []


annotationToType modName name (Can.Forall vars type_) =
    TypeReference modName name type_ 
          


{- JSON ENCODING -}

encode :: Explanation -> Json.Encode.Value
encode (Explanation localizer references) =
    Json.Encode.list
        (encodeReference localizer)
        references

encodeReference :: Reporting.Render.Type.Localizer.Localizer -> Reference -> Json.Encode.Value
encodeReference localizer ref =
    case ref of
        TypeReference modName name canType maybeDef ->
            Json.Encode.object 
                [ "module" ==> encodeCanName modName
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
    