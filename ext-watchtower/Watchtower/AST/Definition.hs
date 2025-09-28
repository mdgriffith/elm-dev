{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NamedFieldPuns #-}

module Watchtower.AST.Definition
  ( findTopLevelDefRegion
  , findLocalDefinitionRegion
  , collectLocalReferences
  , patternBinders
  ) where

import Control.Applicative ((<|>))
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Name as Name
import qualified Reporting.Annotation as Ann
import qualified AST.Canonical as Can


-- Find the region for a top-level definition by name in a canonical module
findTopLevelDefRegion :: Can.Module -> Name.Name -> Maybe Ann.Region
findTopLevelDefRegion (Can.Module _ _ _ decls _ _ _ _) targetName =
  goDecls decls
  where
    goDecls ds =
      case ds of
        Can.Declare d rest -> findInDef d <|> goDecls rest
        Can.DeclareRec d ds' rest -> foldr (<|>) (goDecls rest) (map findInDef (d:ds'))
        Can.SaveTheEnvironment -> Nothing

    findInDef def =
      case def of
        Can.Def locatedName _ _ ->
          let n = Ann.toValue locatedName in
          if n == targetName then Just (Ann.toRegion locatedName) else Nothing
        Can.TypedDef locatedName _ _ _ _ ->
          let n = Ann.toValue locatedName in
          if n == targetName then Just (Ann.toRegion locatedName) else Nothing


-- Collect binders (name with the region of its binding site) from a pattern
patternBinders :: Can.Pattern -> [(Name.Name, Ann.Region)]
patternBinders (Ann.At preg patt) =
  case patt of
    Can.PVar n -> [(n, preg)]
    Can.PAlias sub n -> (n, preg) : patternBinders sub
    Can.PTuple a b mc -> patternBinders a ++ patternBinders b ++ maybe [] patternBinders mc
    Can.PList xs -> concatMap patternBinders xs
    Can.PCons h t -> patternBinders h ++ patternBinders t
    Can.PCtor { Can._p_args = args } -> concatMap argBinders args
    _ -> []
  where
    argBinders (Can.PatternCtorArg _ _ arg) = patternBinders arg


-- Determine the binding site region for a local variable at a given position
findLocalDefinitionRegion :: Can.Module -> Ann.Position -> Name.Name -> Maybe Ann.Region
findLocalDefinitionRegion canModule pos target =
  snd <$> findLocalWithEnv canModule pos target


-- Also collect all references (regions) for that same local binding within the module
collectLocalReferences :: Can.Module -> Ann.Position -> Name.Name -> [Ann.Region]
collectLocalReferences canModule pos target =
  case findLocalWithEnv canModule pos target of
    Nothing -> []
    Just (_, binderRegion) ->
      collectRefs binderRegion canModule
  where
    collectRefs binderRegion (Can.Module _ _ _ decls _ _ _ _) =
      refsInDecls binderRegion decls

    refsInDecls binderRegion decls =
      case decls of
        Can.Declare def rest -> refsInDef binderRegion def ++ refsInDecls binderRegion rest
        Can.DeclareRec def defs rest -> concatMap (refsInDef binderRegion) (def:defs) ++ refsInDecls binderRegion rest
        Can.SaveTheEnvironment -> []

    refsInDef binderRegion def =
      case def of
        Can.Def locatedName patterns expr ->
          let paramEnv = extendEnv mempty (concatMap patternBinders patterns)
          in refsInExpr binderRegion paramEnv expr
        Can.TypedDef _ _ annotatedParams expr _ ->
          let paramEnv = mempty -- no additional named binders from types
              paramBinders = concatMap (patternBinders . fst) annotatedParams
              env1 = extendEnv paramEnv paramBinders
          in refsInExpr binderRegion env1 expr

    refsInExpr binderRegion env (Ann.At region expr_) =
      let deeper = case expr_ of
            Can.List xs -> concatMap (refsInExpr binderRegion env) xs
            Can.Negate x -> refsInExpr binderRegion env x
            Can.Binop _ _ _ _ a b -> refsInExpr binderRegion env a ++ refsInExpr binderRegion env b
            Can.Lambda patterns body ->
              let env' = extendEnv env (concatMap patternBinders patterns) in
              refsInExpr binderRegion env' body
            Can.Call f args -> refsInExpr binderRegion env f ++ concatMap (refsInExpr binderRegion env) args
            Can.If pairs fallback -> concatMap (\(a,b) -> refsInExpr binderRegion env a ++ refsInExpr binderRegion env b) pairs ++ refsInExpr binderRegion env fallback
            Can.Let def body ->
              let envBody = case def of
                              Can.Def locatedName _ _ -> extendEnv env [(Ann.toValue locatedName, Ann.toRegion locatedName)]
                              Can.TypedDef locatedName _ _ _ _ -> extendEnv env [(Ann.toValue locatedName, Ann.toRegion locatedName)]
                  envExpr = case def of
                              Can.Def _ patterns expr -> extendEnv env (concatMap patternBinders patterns)
                              Can.TypedDef _ _ annotatedParams expr _ -> extendEnv env (concatMap (patternBinders . fst) annotatedParams)
              in refsInDef binderRegion def ++ refsInExpr binderRegion envBody body
            Can.LetRec defs body ->
              let defBinders = map (\d -> case d of
                                              Can.Def locatedName _ _ -> (Ann.toValue locatedName, Ann.toRegion locatedName)
                                              Can.TypedDef locatedName _ _ _ _ -> (Ann.toValue locatedName, Ann.toRegion locatedName)
                                        ) defs
                  envBody = extendEnv env defBinders
                  refsDefs = concatMap (refsInDef binderRegion) defs
              in refsDefs ++ refsInExpr binderRegion envBody body
            Can.LetDestruct pattern e body ->
              let env' = extendEnv env (patternBinders pattern) in
              refsInExpr binderRegion env e ++ refsInExpr binderRegion env' body
            Can.Case scrut branches ->
              refsInExpr binderRegion env scrut ++ concatMap (\(Can.CaseBranch p e) -> let env' = extendEnv env (patternBinders p) in refsInExpr binderRegion env' e) branches
            Can.Access e (Ann.At _ _) -> refsInExpr binderRegion env e
            Can.Update _ e updates ->
              let updateExprs = map (\(Can.FieldUpdate _ ue) -> ue) (Map.elems updates)
              in refsInExpr binderRegion env e ++ concatMap (refsInExpr binderRegion env) updateExprs
            Can.Record fields -> concatMap (refsInExpr binderRegion env) (Map.elems fields)
            Can.Tuple a b mc -> refsInExpr binderRegion env a ++ refsInExpr binderRegion env b ++ maybe [] (refsInExpr binderRegion env) mc
            _ -> []
      in case expr_ of
           Can.VarLocal n -> if currentBinder env n == Just binderRegion then region:deeper else deeper
           _ -> deeper

    extendEnv env binders = foldl (\acc (n,r) -> Map.insert n r acc) env binders
    currentBinder env n = Map.lookup n env


-- Internal: find local with environment tracking; returns the matched name and its binder region
findLocalWithEnv :: Can.Module -> Ann.Position -> Name.Name -> Maybe (Name.Name, Ann.Region)
findLocalWithEnv (Can.Module _ _ _ decls _ _ _ _) pos target =
  goDecls mempty decls
  where
    (<|>) = mplus
    mplus a b = case a of
      Just _ -> a
      Nothing -> b

    goDecls env ds =
      case ds of
        Can.Declare d rest -> goDef env d <|> goDecls env rest
        Can.DeclareRec d ds' rest -> foldr (<|>) (goDecls env rest) (map (goDef env) (d:ds'))
        Can.SaveTheEnvironment -> Nothing

    goDef env def =
      case def of
        Can.Def locatedName patterns expr ->
          let envExpr = extendEnv env (concatMap patternBinders patterns)
              envBody = extendEnv env [(Ann.toValue locatedName, Ann.toRegion locatedName)]
          in goExpr envExpr expr <|> goExpr envBody (Ann.At (Ann.toRegion (Ann.At (Ann.toRegion locatedName) (Ann.toValue locatedName))) (Can.Unit)) -- no-op path; definition doesn't contain pos
        Can.TypedDef locatedName _ annotatedParams expr _ ->
          let envExpr = extendEnv env (concatMap (patternBinders . fst) annotatedParams)
          in goExpr envExpr expr

    goExpr env (Ann.At region expr_) =
      if not (contains region pos) then Nothing else
      let recurse = case expr_ of
            Can.List xs -> foldr (<|>) Nothing (map (goExpr env) xs)
            Can.Negate x -> goExpr env x
            Can.Binop _ _ _ _ a b -> goExpr env a <|> goExpr env b
            Can.Lambda patterns body ->
              let env' = extendEnv env (concatMap patternBinders patterns)
              in goExpr env' body
            Can.Call f args -> goExpr env f <|> foldr (<|>) Nothing (map (goExpr env) args)
            Can.If pairs fallback -> foldr (<|>) (goExpr env fallback) (map (\(a,b) -> goExpr env a <|> goExpr env b) pairs)
            Can.Let def body ->
              let envExpr = case def of
                              Can.Def _ patterns _ -> extendEnv env (concatMap patternBinders patterns)
                              Can.TypedDef _ _ annotatedParams _ _ -> extendEnv env (concatMap (patternBinders . fst) annotatedParams)
                  envBody = case def of
                              Can.Def locatedName _ _ -> extendEnv env [(Ann.toValue locatedName, Ann.toRegion locatedName)]
                              Can.TypedDef locatedName _ _ _ _ -> extendEnv env [(Ann.toValue locatedName, Ann.toRegion locatedName)]
              in goDef env def <|> goExpr envExpr (getDefExpr def) <|> goExpr envBody body
            Can.LetRec defs body ->
              let defBinders = map (\d -> case d of
                                              Can.Def locatedName _ _ -> (Ann.toValue locatedName, Ann.toRegion locatedName)
                                              Can.TypedDef locatedName _ _ _ _ -> (Ann.toValue locatedName, Ann.toRegion locatedName)
                                        ) defs
                  envBody = extendEnv env defBinders
                  rec = foldr (<|>) Nothing (map (goDef env) defs)
              in rec <|> goExpr envBody body
            Can.LetDestruct pattern e body ->
              let env' = extendEnv env (patternBinders pattern)
              in goExpr env e <|> goExpr env' body
            Can.Case scrut branches ->
              goExpr env scrut <|> foldr (<|>) Nothing (map (\(Can.CaseBranch p e) -> let env' = extendEnv env (patternBinders p) in goExpr env' e) branches)
            Can.Access e (Ann.At _ _) -> goExpr env e
            Can.Update _ e updates ->
              let updateExprs = map (\(Can.FieldUpdate _ ue) -> ue) (Map.elems updates)
              in goExpr env e <|> foldr (<|>) Nothing (map (goExpr env) updateExprs)
            Can.Record fields -> foldr (<|>) Nothing (map (goExpr env) (Map.elems fields))
            Can.Tuple a b mc -> goExpr env a <|> goExpr env b <|> maybe Nothing (goExpr env) mc
            _ -> Nothing
      in case expr_ of
           Can.VarLocal n -> if n == target then case Map.lookup n env of
                                                Just r -> Just (n, r)
                                                Nothing -> Nothing
                              else recurse
           _ -> recurse

    getDefExpr d =
      case d of
        Can.Def _ _ e -> e
        Can.TypedDef _ _ _ e _ -> e

    contains (Ann.Region (Ann.Position sr sc) (Ann.Position er ec)) (Ann.Position pr pc) =
      (pr > sr || (pr == sr && pc >= sc)) && (pr < er || (pr == er && pc <= ec))

    extendEnv env binders = foldl (\acc (n,r) -> Map.insert n r acc) env binders


