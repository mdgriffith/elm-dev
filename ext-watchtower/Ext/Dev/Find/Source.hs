{-# LANGUAGE OverloadedStrings #-}

module Ext.Dev.Find.Source
  ( definitionAtPoint, Found(..)
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


definitionAtPoint :: Watchtower.Editor.PointLocation -> Src.Module -> Maybe Found
definitionAtPoint point (Src.Module name exports docs imports values unions aliases infixes effects) =
    find point (FoundValue Nothing) values
        & orFind point (FoundUnion Nothing) unions
        & orFind point (FoundAlias Nothing) aliases


find :: Watchtower.Editor.PointLocation -> (A.Located a -> Found) -> [ A.Located a ] -> Maybe Found
find (Watchtower.Editor.PointLocation _ point) toResult items =
    List.foldl 
        (\found located@(A.At region item) ->
            case found of
                Nothing ->
                    if withinRegion point region then
                        Just (toResult located)
                    else
                        found

                Just _ ->
                    found
        
        ) Nothing items

orFind :: Watchtower.Editor.PointLocation -> (A.Located a -> Found) -> [ A.Located a] -> Maybe Found -> Maybe Found
orFind point toResult items previousResult =
    case previousResult of
        Nothing ->
           find point toResult items

        _ ->
            previousResult


withinRegion :: A.Position -> A.Region -> Bool
withinRegion (A.Position row col) (A.Region (A.Position startRow startCol) (A.Position endRow endCol)) =
  (row == startRow && col >= startCol || row > startRow) 
        && (row == endRow && col <= endCol || row < endRow)




