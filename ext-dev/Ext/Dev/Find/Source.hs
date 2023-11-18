{-# LANGUAGE OverloadedStrings #-}

module Ext.Dev.Find.Source
  ( definitionNamed 
  , definitionAtPoint, Found(..)
  , withCanonical, Def(..)
  )
where

import qualified AST.Source as Src
import qualified AST.Canonical as Can
import qualified Reporting.Annotation as A
import qualified Watchtower.Editor
import qualified Data.List as List


import Data.Name (Name)
import qualified Data.Map as Map
import Data.Function ((&))

data Found
    = FoundValue (Maybe Def) (A.Located Src.Value)
    | FoundUnion (Maybe Can.Union) (A.Located Src.Union)
    | FoundAlias (Maybe Can.Alias) (A.Located Src.Alias)


data Def
    = Def Can.Def
    | DefRecursive Can.Def [Can.Def]


withCanonical :: Can.Module -> Found -> Found
withCanonical (Can.Module name exports docs decls unions aliases binops effects) found =
    case found of
        FoundValue _ value@(A.At loc (Src.Value (A.At _ name) patterns_ expr_ maybeType_)) ->
            FoundValue (getDefNamed name decls) value

        FoundUnion _ (union@(A.At loc (Src.Union (A.At _ name) _ _))) ->
            FoundUnion (Map.lookup name unions) union
        
        FoundAlias _ (A.At loc alias_) ->
            found


getDefNamed :: Name -> Can.Decls -> Maybe Def
getDefNamed name decls =
    case decls of
        Can.SaveTheEnvironment -> Nothing

        Can.Declare def moarDecls ->
            if defNamed name def then
                Just (Def def)
            else
                getDefNamed name moarDecls

        Can.DeclareRec def internalDefs moarDecls ->
            if defNamed name def then
                Just (DefRecursive def internalDefs)
            else
                getDefNamed name moarDecls


defNamed :: Name -> Can.Def -> Bool
defNamed name def =
    case def of
        Can.Def (A.At _ defName) _ _ ->
            name == defName
        
        Can.TypedDef (A.At _ defName) _ _ _ _ ->
            name == defName


definitionNamed :: Name -> Src.Module -> Maybe Found
definitionNamed valueName (Src.Module name exports docs imports values unions aliases infixes effects) =
    find (withName valueName toValueName (FoundValue Nothing)) values
        & orFind (withName valueName toUnionName (FoundUnion Nothing)) unions
        & orFind (withName valueName toAliasName (FoundAlias Nothing)) aliases


withName :: Name -> (a -> Name) ->  (A.Located a -> Found) ->  A.Located a -> Maybe Found
withName name getName toFound (locatedItem@(A.At _ val)) =
    if name == getName val then
        Just (toFound locatedItem)
    else
        Nothing


toValueName :: Src.Value -> Name
toValueName (Src.Value (A.At _ name) _ _ _) =
    name

toUnionName :: Src.Union -> Name
toUnionName (Src.Union (A.At _ name) _ _) =
    name

toAliasName :: Src.Alias -> Name
toAliasName (Src.Alias (A.At _ name) _ _) =
    name


definitionAtPoint :: Watchtower.Editor.PointLocation -> Src.Module -> Maybe Found
definitionAtPoint point (Src.Module name exports docs imports values unions aliases infixes effects) =
    find (atLocation point (FoundValue Nothing)) values
        & orFind (atLocation point (FoundUnion Nothing)) unions
        & orFind (atLocation point (FoundAlias Nothing)) aliases


atLocation :: Watchtower.Editor.PointLocation -> (A.Located a -> Found) -> A.Located a -> Maybe Found
atLocation (Watchtower.Editor.PointLocation _ point) toFound (locatedItem@(A.At region _)) =
    if withinRegion point region then
        Just (toFound locatedItem)
    else
        Nothing

find :: (A.Located a -> Maybe Found) -> [ A.Located a ] -> Maybe Found
find toResult items =
    List.foldl 
        (\found located ->
            case found of
                Nothing ->
                    (toResult located)
                    
                Just _ ->
                    found
        
        ) Nothing items

orFind :: (A.Located a -> Maybe Found) -> [ A.Located a] -> Maybe Found -> Maybe Found
orFind toResult items previousResult =
    case previousResult of
        Nothing ->
           find toResult items

        _ ->
            previousResult


withinRegion :: A.Position -> A.Region -> Bool
withinRegion (A.Position row col) (A.Region (A.Position startRow startCol) (A.Position endRow endCol)) =
  (row == startRow && col >= startCol || row > startRow) 
        && (row == endRow && col <= endCol || row < endRow)




