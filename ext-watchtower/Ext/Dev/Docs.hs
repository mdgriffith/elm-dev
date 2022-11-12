{-# LANGUAGE OverloadedStrings #-}

module Ext.Dev.Docs (fromArtifacts) where



import Data.Map ((!))
import qualified Data.Map as Map
import qualified Data.OneOrMore as OneOrMore
import qualified Data.Name as Name
import qualified Data.NonEmptyList as NE

import qualified Elm.ModuleName as ModuleName
import qualified Elm.Compiler.Type.Extract as Extract
import qualified Elm.Compiler.Type as Type
import qualified Elm.Docs as Docs


import qualified Reporting.Error.Docs
import qualified Reporting.Result as Result
import qualified Reporting.Annotation as A
import qualified Json.String
import qualified Compile
import qualified AST.Canonical as Can
import qualified AST.Source as Src


{-|
This is a more permissive way of generating a Docs.Module.

Unlike normal docs.json generation, it will *not* fail in these cases:
    1. If it's an application
    2. Missing doc comment (an empty one will be used)
    3. Missing type signature (inferred typesignature will be used in that case)
    4. No module doc comment is present (an empty string will be used instead)
    5. If the module doc comment and the exposing line are out of sync.
    6. If the module is exposing all.



This largely mirrors Elm.Docs.fromModule, just returning errors and the Docs.Module instead doing a strict check.

-}
fromArtifacts :: Compile.Artifacts -> Either Reporting.Error.Docs.Error Docs.Module
fromArtifacts (Compile.Artifacts modul@(Can.Module _ exports docs decls _ _ _ _) tipes graph) =
    let
        exportDict =
            case exports of
                Can.ExportEverything region ->
                    exposeAll modul

                Can.Export dict ->
                    dict

        (overview, comments) =
            case docs of
                Src.NoDocs region ->
                    (Json.String.fromChars "" , Map.fromList [])

                Src.YesDocs (Src.Comment over) comm ->
                    (Json.String.fromComment over, Map.fromList comm)


    in
    checkDefs exportDict overview comments modul tipes


exposeAll :: Can.Module -> Map.Map Name.Name (A.Located Can.Export) 
exposeAll modul@(Can.Module _ exports docs decls unions aliases binops effects) =
    addPorts effects (exposeAllHelper modul decls Map.empty)


addPorts :: Can.Effects -> Map.Map Name.Name (A.Located Can.Export) -> Map.Map Name.Name (A.Located Can.Export) 
addPorts effects gathered =
    case effects of
        Can.NoEffects ->
            gathered
        
        Can.Ports portMap ->
            Map.union gathered
                (Map.map (\portValue -> A.At A.zero Can.ExportPort) portMap)

        Can.Manager _ _ _ _ ->
            gathered

exposeAllHelper :: Can.Module -> Can.Decls -> Map.Map Name.Name (A.Located Can.Export) -> Map.Map Name.Name (A.Located Can.Export) 
exposeAllHelper mod decls gathered =
    case decls of
        Can.SaveTheEnvironment ->
            gathered
        
        Can.Declare (Can.Def (A.At loc name) _ (A.At _ expr)) moreDecls ->
            let
                exportType = getExportType mod name
            in
            exposeAllHelper mod moreDecls (Map.insert name (A.At loc exportType) gathered)

        Can.Declare (Can.TypedDef (A.At loc name) _ _ _ _) moreDecls ->
            let
                exportType = getExportType mod name
            in
            exposeAllHelper mod moreDecls (Map.insert name (A.At loc exportType) gathered) 
        
        Can.DeclareRec (Can.Def (A.At loc name) _ (A.At _ expr)) defs moreDecls ->
            -- recursive declaration
            -- not totally sure what `defs` is
            let
                exportType = getExportType mod name
            in
            exposeAllHelper mod moreDecls (Map.insert name (A.At loc exportType) gathered)
        
        Can.DeclareRec (Can.TypedDef (A.At loc name) _ _ _ _) defs moreDecls ->
            -- recursive declaration
            -- not totally sure what `defs` is
            let
                exportType = getExportType mod name
            in
            exposeAllHelper mod moreDecls (Map.insert name (A.At loc exportType) gathered)


{-|


data Export
  = ExportValue
  | ExportBinop
  | ExportAlias
  | ExportUnionOpen
  | ExportUnionClosed
  | ExportPort

-}
getExportType :: Can.Module -> Name.Name -> Can.Export
getExportType mod@(Can.Module _ exports docs decls unions aliases binops _) name =
    if Map.member name unions then
        Can.ExportUnionOpen

    else if Map.member name binops then
        Can.ExportBinop

    else if Map.member name aliases then
        Can.ExportAlias

    else
        Can.ExportValue


checkDefs :: Map.Map Name.Name (A.Located Can.Export) -> Json.String.String -> Map.Map Name.Name Src.Comment -> Can.Module -> Map.Map Name.Name Can.Annotation -> Either Reporting.Error.Docs.Error Docs.Module
checkDefs exportDict overview comments (Can.Module name _ _ decls unions aliases infixes effects) allTypes =
  let
    types = Docs.gatherTypes decls Map.empty
    info = Info comments types allTypes unions aliases infixes effects
  in
  case Result.run (Map.traverseWithKey (checkExport info) exportDict) of
    (_, Left  problems ) -> Left  $ Reporting.Error.Docs.DefProblems (OneOrMore.destruct NE.List problems)
    (_, Right inserters) -> Right $ foldr ($) (emptyModule name overview) inserters


data Info =
  Info
    { _iComments :: Map.Map Name.Name Src.Comment
    , _iValues   :: Map.Map Name.Name (Either A.Region Can.Type)
    , _cTypes :: Map.Map Name.Name Can.Annotation
    , _iUnions   :: Map.Map Name.Name Can.Union
    , _iAliases  :: Map.Map Name.Name Can.Alias
    , _iBinops   :: Map.Map Name.Name Can.Binop
    , _iEffects  :: Can.Effects
    }

emptyModule :: ModuleName.Canonical -> Json.String.String -> Docs.Module
emptyModule (ModuleName.Canonical _ name) overview =
  Docs.Module name overview Map.empty Map.empty Map.empty Map.empty


checkExport :: Info -> Name.Name -> A.Located Can.Export -> Result.Result i w Reporting.Error.Docs.DefProblem (Docs.Module -> Docs.Module)
checkExport info name (A.At region export) =
  case export of
    Can.ExportValue ->
      do  tipe <- getType name info
          comment <- getComment region name info
          Result.ok $ \m ->
            m { Docs._values = Map.insert name (Docs.Value comment tipe) (Docs._values m) }

    Can.ExportBinop ->
      do  let (Can.Binop_ assoc prec realName) = _iBinops info ! name
          tipe <- getType realName info
          comment <- getComment region realName info
          Result.ok $ \m ->
            m { Docs._binops = Map.insert name (Docs.Binop comment tipe assoc prec) (Docs._binops m) }

    Can.ExportAlias ->
      do  let (Can.Alias tvars tipe) = _iAliases info ! name
          comment <- getComment region name info
          Result.ok $ \m ->
            m { Docs._aliases = Map.insert name (Docs.Alias comment tvars (Extract.fromType tipe)) (Docs._aliases m) }

    Can.ExportUnionOpen ->
      do  let (Can.Union tvars ctors _ _) = _iUnions info ! name
          comment <- getComment region name info
          Result.ok $ \m ->
            m { Docs._unions = Map.insert name (Docs.Union comment tvars (map Docs.dector ctors)) (Docs._unions m) }

    Can.ExportUnionClosed ->
      do  let (Can.Union tvars _ _ _) = _iUnions info ! name
          comment <- getComment region name info
          Result.ok $ \m ->
            m { Docs._unions = Map.insert name (Docs.Union comment tvars []) (Docs._unions m) }

    Can.ExportPort ->
      do  tipe <- getType name info
          comment <- getComment region name info
          Result.ok $ \m ->
            m { Docs._values = Map.insert name (Docs.Value comment tipe) (Docs._values m) }



getType :: Name.Name -> Info -> Result.Result i w Reporting.Error.Docs.DefProblem Type.Type
getType name info =
  case _iValues info ! name of
    Left region ->
      case _cTypes info ! name of
        Can.Forall vars tipe ->
          Result.ok (Extract.fromType tipe)


    Right tipe ->
      Result.ok (Extract.fromType tipe)

{-|


-}
getComment :: A.Region -> Name.Name -> Info -> Result.Result i w Reporting.Error.Docs.DefProblem Docs.Comment
getComment region name info =
  case Map.lookup name (_iComments info) of
    Nothing ->
      Result.ok (Json.String.fromChars "")

    Just (Src.Comment snippet) ->
      Result.ok (Json.String.fromComment snippet)