{-# LANGUAGE OverloadedStrings #-}

module Ext.Dev.InScope
  ( inScope, encode
  )
where

import AST.Canonical (Type (..))
import qualified AST.Canonical as Can
import qualified AST.Optimized as Opt
import qualified AST.Source as Src
import qualified Elm.Interface
import qualified Elm.ModuleName


import Data.Function ((&))
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.NonEmptyList as NE
import qualified Data.Map as Map
import qualified Data.Name as Name
import Data.Name (Name)
import qualified Ext.CompileProxy
import qualified Ext.CompileHelpers.Generic
import qualified Elm.Package as Package
import qualified Elm.ModuleName as ModuleName
import qualified Reporting.Render.Type.Localizer
import qualified Reporting.Doc
import qualified Reporting.Render.Type
import qualified Reporting.Annotation as A

import qualified Json.Encode
import qualified Json.String
import Json.Encode ((==>))


data Scope =
    Scope
        { _imported         :: [ Src.Import ]
        , _topLevelValues   :: [ Declaration ]
        , _interfaces       :: Map.Map Elm.ModuleName.Raw Elm.Interface.Interface
        , _localizer :: Reporting.Render.Type.Localizer.Localizer
        }

data Declaration =
    Declaration Name (Maybe Can.Type)

{-| All values that are in scope for a specific file.

-}
inScope :: String -> String -> IO (Maybe Scope)
inScope root path = do
    (Ext.CompileProxy.Single source warnings interfaces canonical compiled) <- Ext.CompileProxy.loadSingle root path
    case (source, canonical, interfaces) of
        (Right srcModule, Just canMod, Just faces) -> do
            let localizer = Reporting.Render.Type.Localizer.fromModule srcModule
            pure (Just (captureScope srcModule canMod localizer faces))

        _ ->
             pure Nothing

captureScope :: Src.Module -> Can.Module ->  Reporting.Render.Type.Localizer.Localizer -> Map.Map Elm.ModuleName.Raw Elm.Interface.Interface -> Scope
captureScope srcModule canModule@(Can.Module name exports docs decls unions aliases binops effects) localizer interfaces = 
    Scope 
        (getImports srcModule)
        (getDeclarations decls)
        interfaces
        localizer

getImports :: Src.Module -> [ Src.Import ]
getImports (Src.Module name exports docs imports values unions aliases binops effects) =
    imports


getDeclarations :: Can.Decls -> [ Declaration ]
getDeclarations decls =
    getDeclarationHelper decls []



getDeclarationHelper :: Can.Decls -> [ Declaration ] -> [ Declaration ]
getDeclarationHelper decls found =
    case decls of
        Can.SaveTheEnvironment -> found
        Can.Declare def moarDecls ->
           Declaration (getDefName def) Nothing : getDeclarationHelper moarDecls found

        Can.DeclareRec def _defs moarDecls ->
           Declaration (getDefName def) Nothing : getDeclarationHelper moarDecls found
        
getDefName :: Can.Def -> Name
getDefName def =
    case def of
        Can.Def (A.At _ name) _ _ ->
            name
        Can.TypedDef (A.At _ name) _ _ _ _ ->
            name


{- ENCODING -}

encode :: Scope -> Json.Encode.Value
encode (Scope imported topLevels interfaces localizer) =
    -- Json.Encode.dict
    --     (Json.String.fromChars . ModuleName.toChars)
    --     encodeInterface
    --     interfaces
    Json.Encode.list id (Maybe.catMaybes (fmap (encodeImport localizer interfaces) imported))


encodeInterface :: Elm.Interface.Interface -> Json.Encode.Value
encodeInterface (Elm.Interface.Interface pkgName values unions aliases binops) =
    Json.Encode.object 
        [ "package" ==> Package.encode pkgName
        , "values"  ==> Json.Encode.list Json.Encode.name (Map.keys values)
        , "unions"  ==> Json.Encode.list Json.Encode.name (Map.keys unions)
        , "aliases" ==> Json.Encode.list Json.Encode.name (Map.keys aliases)
        , "binops"  ==> Json.Encode.list Json.Encode.name (Map.keys binops)
        ]

encodeImport :: 
    Reporting.Render.Type.Localizer.Localizer 
      -> Map.Map Elm.ModuleName.Raw Elm.Interface.Interface
      -> Src.Import 
      -> Maybe (Json.Encode.Value)
encodeImport localizer interfaces (Src.Import (A.At _ name) maybeAlias exposing) =
    case Map.lookup name interfaces of
        Nothing ->
            Nothing

        Just (interface@(Elm.Interface.Interface pkgName values unions aliases binops)) ->
            Just 
                (Json.Encode.object 
                    [ "package"   ==> Package.encode pkgName
                    , "name"      ==> Json.Encode.name name
                    , "alias"     ==> 
                        case maybeAlias of
                            Nothing -> Json.Encode.null
                            Just alias_ ->
                                Json.Encode.name alias_
                    , "values" ==>
                        (Json.Encode.object
                            (encodeMapAsList (encodeExposedValue localizer) values)
                        )
                    , "unions" ==>
                        (Json.Encode.object
                            (encodeMapAsList (encodeExposedUnion localizer (Src.Public A.zero)) unions)
                        )
                    , "aliases" ==>
                        (Json.Encode.object
                            (encodeMapAsList (encodeExposedAlias localizer) aliases)
                        )
                    , "binops" ==>
                        (Json.Encode.object
                            (encodeMapAsList (encodeExposedBinOp localizer) binops)
                        )

                    , "exposed"  ==> encodeExposing localizer interface exposing
                    ]
                )

encodeExposing :: Reporting.Render.Type.Localizer.Localizer  -> Elm.Interface.Interface -> Src.Exposing -> Json.Encode.Value
encodeExposing localizer (interface@(Elm.Interface.Interface pkgName values unions aliases binops)) exposing  =
    case exposing of
        Src.Open ->
            Json.Encode.list encodeExposed []

        Src.Explicit itemList ->
            Json.Encode.list encodeExposed itemList
                


encodeMapAsList encodeValue map =
    Map.toList map
        & fmap 
            (\(key, val) -> 
                encodeValue val
                    & withName key
            )


encodeExposed :: 
    Src.Exposed 
     -> Json.Encode.Value
encodeExposed exposed =
    case exposed of
        Src.Lower (A.At _ name) ->
            Json.Encode.name name

        Src.Upper (A.At _ name) (privacy@Src.Private) ->
            Json.Encode.name name
        
        Src.Upper (A.At _ name) (privacy@(Src.Public _)) ->
            Json.Encode.name name

        Src.Operator _ name ->
            Json.Encode.name name



withName :: Name -> Json.Encode.Value -> (Json.String.String, Json.Encode.Value)
withName name val =
    ( Json.String.fromName name
    , val
    )


encodeExposedValue :: Reporting.Render.Type.Localizer.Localizer -> Can.Annotation -> Json.Encode.Value
encodeExposedValue localizer (Can.Forall freevars tipe) =
    encodeCanType localizer tipe


encodeExposedUnion :: Reporting.Render.Type.Localizer.Localizer -> Src.Privacy -> Elm.Interface.Union -> Json.Encode.Value
encodeExposedUnion localizer privacy interfaceUnion =
    case interfaceUnion of
        Elm.Interface.OpenUnion union ->
            encodeUnion localizer union
        
        Elm.Interface.ClosedUnion union ->
            encodeUnion localizer union

        Elm.Interface.PrivateUnion union ->
            Json.Encode.chars "private"


encodeUnion localizer (Can.Union vars ctors _ _) =
    Json.Encode.object 
        [  "args" ==> 
            Json.Encode.list Json.Encode.name vars
        , "cases" ==>
            Json.Encode.list (encodeUnionVariant localizer) ctors
     
        ] 

encodeUnionVariant :: Reporting.Render.Type.Localizer.Localizer -> Can.Ctor  -> Json.Encode.Value
encodeUnionVariant localizer (Can.Ctor name _ _ args) =
    Json.Encode.list id 
        [ Json.Encode.name name
        , Json.Encode.list (encodeCanType localizer) args
        ]


encodeExposedAlias :: Reporting.Render.Type.Localizer.Localizer -> Elm.Interface.Alias -> Json.Encode.Value
encodeExposedAlias localizer interfaceAlias =
   case interfaceAlias of
        Elm.Interface.PublicAlias (Can.Alias vars type_) ->
            Json.Encode.object 
                [ "args" ==> Json.Encode.list Json.Encode.name vars
                , "type" ==> encodeCanType localizer type_
                ]
           
        Elm.Interface.PrivateAlias (Can.Alias vars type_) ->
            Json.Encode.object 
                [ "args" ==> Json.Encode.list Json.Encode.name vars
                , "type" ==> encodeCanType localizer type_
                ]


encodeExposedBinOp :: Reporting.Render.Type.Localizer.Localizer -> Elm.Interface.Binop -> Json.Encode.Value
encodeExposedBinOp localizer (Elm.Interface.Binop name (Can.Forall freevars tipe) _ _) =
    encodeCanType localizer tipe



encodeCanType :: Reporting.Render.Type.Localizer.Localizer -> Can.Type -> Json.Encode.Value
encodeCanType localizer type_ =
    Json.Encode.chars
        (Reporting.Doc.toLine
            (Reporting.Render.Type.canToDoc localizer Reporting.Render.Type.None type_)
        )
    
    
