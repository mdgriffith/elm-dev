{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Ext.Render.Type
  ( renderAlias
  )
  where


import qualified Data.List as List
import qualified Data.Set as Set

import qualified AST.Canonical as Can
import qualified Elm.ModuleName as ModuleName
import qualified Data.Name as Name
import qualified Reporting.Doc as D
import qualified Reporting.Render.Type as RT
import qualified Reporting.Render.Type.Localizer as L


-- PUBLIC API


renderAlias :: L.Localizer -> Can.Type -> String
renderAlias localizer tipe =
  let
    aliases = collectAliases tipe
    docs = map (aliasToDoc localizer) aliases
  in
  case docs of
    [] -> ""
    _  -> D.toString (D.stack docs)


-- INTERNALS


type AliasKey = (ModuleName.Canonical, Name.Name)


data AliasInfo = AliasInfo
  { _home :: ModuleName.Canonical
  , _name :: Name.Name
  , _args :: [(Name.Name, Can.Type)]
  , _aliased :: Can.AliasType
  }


collectAliases :: Can.Type -> [AliasInfo]
collectAliases root =
  let
    step :: Set.Set AliasKey -> [AliasInfo] -> Can.Type -> (Set.Set AliasKey, [AliasInfo])
    step seen acc t =
      case t of
        Can.TLambda a b ->
          let (seen1, acc1) = step seen acc a in step seen1 acc1 b

        Can.TVar _ -> (seen, acc)

        Can.TType _ _ args ->
          List.foldl' (\(s,a1) x -> step s a1 x) (seen, acc) args

        Can.TRecord fields maybeExt ->
          let
            (s1,a1) = List.foldl' (\(s,a2) (_, ft) -> step s a2 ft) (seen, acc) (Can.fieldsToList fields)
          in
          case maybeExt of
            Nothing -> (s1,a1)
            Just _ -> (s1,a1)

        Can.TUnit -> (seen, acc)

        Can.TTuple a b maybeC ->
          let (s1,a1) = step seen acc a
              (s2,a2) = step s1 a1 b
          in
          case maybeC of
            Nothing -> (s2,a2)
            Just c -> step s2 a2 c

        Can.TAlias home name args aliasType ->
          let key = (home, name)
          in if Set.member key seen then
               -- still traverse inner body to find nested aliases
               case aliasType of
                 Can.Holey inner -> step seen acc inner
                 Can.Filled inner -> step seen acc inner
             else
               let seen' = Set.insert key seen
                   acc' = acc ++ [AliasInfo home name args aliasType]
               in case aliasType of
                    Can.Holey inner -> step seen' acc' inner
                    Can.Filled inner -> step seen' acc' inner
  in
  snd (step Set.empty [] root)


aliasToDoc :: L.Localizer -> AliasInfo -> D.Doc
aliasToDoc localizer (AliasInfo home name args aliasType) =
  let
    nameDoc = L.toDoc localizer home name
    argNames = map fst args
    header = case argNames of
      [] -> D.hsep ["type alias", nameDoc, "="]
      _  -> D.hsep ["type alias", D.hsep (nameDoc : map D.fromName argNames), "="]
    bodyType = case aliasType of
      Can.Holey t -> t
      Can.Filled t -> t
    bodyDoc = RT.canToDoc localizer RT.None bodyType
  in
  D.hang 4 (D.sep [ header, bodyDoc ])


