module Ext.Dev.Help (toMissingTypeLookup, isExposed) where


import qualified Data.Map as Map
import qualified Data.Name as Name
import qualified AST.Canonical as Can
import qualified Reporting.Warning as Warning
import qualified Data.Maybe as Maybe
import qualified Ext.CompileProxy
import Data.Function ((&))


{-|

This isn't the best way to approach this, we should likely patch the `Can.AST` that is returned by `Single`.

But that work may be overtaken when we move to a singular AST.

-}
toMissingTypeLookup :: Ext.CompileProxy.SingleFileResult -> Map.Map Name.Name Can.Type
toMissingTypeLookup (Ext.CompileProxy.Single source warnings maybeInterfaces maybeCanonical compiled) =
    let
        warningList = (Maybe.fromMaybe [] warnings)
    in
    warningList
        & fmap (\warning ->
            case warning of
                Warning.MissingTypeAnnotation region name annotation ->
                    Just (name, annotation)

                _ ->
                    Nothing
            )
        & Maybe.catMaybes
        & Map.fromList


isExposed :: Name.Name -> Can.Exports -> Bool
isExposed name exports =
    case exports of
        Can.ExportEverything _ ->
            True
 
        Can.Export map ->
            Map.member name map
