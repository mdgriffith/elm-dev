{-# OPTIONS_GHC -Wall #-}

module Watchtower.AST.References
  ( collectVarRefsInModule
  ) where

import qualified Data.Map as Map
import qualified Reporting.Annotation as Ann
import qualified AST.Canonical as Can
import qualified Elm.ModuleName as ModuleName
import qualified Data.Name as Name


-- Collect regions of references to a value defined in `home` with `name`
collectVarRefsInModule :: Can.Module -> ModuleName.Canonical -> Name.Name -> [Ann.Region]
collectVarRefsInModule (Can.Module _ _ _ decls _ _ _ _) home targetName =
  refsInDecls decls
  where
    refsInDecls ds =
      case ds of
        Can.Declare def rest -> refsInDef def ++ refsInDecls rest
        Can.DeclareRec def defs rest -> concatMap refsInDef (def:defs) ++ refsInDecls rest
        Can.SaveTheEnvironment -> []

    refsInDef def =
      case def of
        Can.Def _ _ expr -> refsInExpr expr
        Can.TypedDef _ _ _ expr _ -> refsInExpr expr

    refsInExpr (Ann.At region expr_) =
      let here = case expr_ of
            Can.VarTopLevel h n | h == home && n == targetName -> [region]
            Can.VarForeign h n _ | h == home && n == targetName -> [region]
            Can.VarOperator _ h real _ | h == home && real == targetName -> [region]
            Can.Binop _ h real _ _ _ | h == home && real == targetName -> [region]
            Can.VarCtor _ h n _ _ | h == home && n == targetName -> [region]
            _ -> []
          deeper = case expr_ of
            Can.List xs -> concatMap refsInExpr xs
            Can.Negate x -> refsInExpr x
            Can.Binop _ _ _ _ a b -> refsInExpr a ++ refsInExpr b
            Can.Lambda patterns body -> concatMap refsInPattern patterns ++ refsInExpr body
            Can.Call f args -> refsInExpr f ++ concatMap refsInExpr args
            Can.If pairs fallback -> concatMap (\(a,b) -> refsInExpr a ++ refsInExpr b) pairs ++ refsInExpr fallback
            Can.Let def body -> refsInDef def ++ refsInExpr body
            Can.LetRec defs body -> concatMap refsInDef defs ++ refsInExpr body
            Can.LetDestruct pattern e body -> refsInPattern pattern ++ refsInExpr e ++ refsInExpr body
            Can.Case scrut branches -> refsInExpr scrut ++ concatMap (\(Can.CaseBranch p e) -> refsInPattern p ++ refsInExpr e) branches
            Can.Access e (Ann.At _ _) -> refsInExpr e
            Can.Update _ e updates ->
              let updateExprs = map (\(Can.FieldUpdate _ ue) -> ue) (Map.elems updates)
              in refsInExpr e ++ concatMap refsInExpr updateExprs
            Can.Record fields -> concatMap refsInExpr (Map.elems fields)
            Can.Tuple a b mc -> refsInExpr a ++ refsInExpr b ++ maybe [] refsInExpr mc
            _ -> []
      in here ++ deeper

    refsInPattern (Ann.At _ patt) =
      case patt of
        Can.PTuple a b mc -> refsInPattern a ++ refsInPattern b ++ maybe [] refsInPattern mc
        Can.PList xs -> concatMap refsInPattern xs
        Can.PCons h t -> refsInPattern h ++ refsInPattern t
        Can.PCtor { Can._p_args = args } -> concatMap (refsInPattern . Can._arg) args
        _ -> []


