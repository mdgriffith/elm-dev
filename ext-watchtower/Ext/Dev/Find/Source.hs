{-# LANGUAGE OverloadedStrings #-}

module Ext.Dev.Find.Source
  ( definitionAtPoint, Found(..)
  )
where

import qualified AST.Source as Src
import qualified Reporting.Annotation as A
import qualified Watchtower.Editor
import qualified Data.List as List
import Data.Function ((&))

data Found
    = FoundValue (A.Located Src.Value)
    | FoundUnion (A.Located Src.Union)
    | FoundAlias (A.Located Src.Alias)
    | FoundInfix (A.Located Src.Infix)


definitionAtPoint :: Watchtower.Editor.PointLocation -> Src.Module -> Maybe Found
definitionAtPoint point (Src.Module name exports docs imports values unions aliases infixes effects) =
    find point FoundValue values
        & orFind point FoundUnion unions
        & orFind point FoundAlias aliases
        & orFind point FoundInfix infixes


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