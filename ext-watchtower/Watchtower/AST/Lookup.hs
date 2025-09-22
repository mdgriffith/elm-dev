{-# OPTIONS_GHC -Wall #-}

module Watchtower.AST.Lookup
  ( Found(..)
  , findAtPosition
  ) where

import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Reporting.Annotation as Ann
import qualified AST.Canonical as Can
import qualified Data.Map as Map


-- The result of a hover lookup in the canonical AST
data Found
  = FoundAnnotation Can.Annotation
  | FoundType Can.Type


-- Public API: find the innermost canonical node at a position that carries
-- either a cached Annotation or a Type.
findAtPosition :: Can.Module -> Ann.Position -> Maybe (Found, Ann.Region)
findAtPosition (Can.Module _ _ _ decls _ _ _ _) pos =
  selectSmallest (findInDecls pos decls)


-- Helpers

regionContains :: Ann.Region -> Ann.Position -> Bool
regionContains (Ann.Region (Ann.Position sr sc) (Ann.Position er ec)) (Ann.Position pr pc) =
  (pr > sr || (pr == sr && pc >= sc)) && (pr < er || (pr == er && pc <= ec))

selectSmallest :: [(a, Ann.Region)] -> Maybe (a, Ann.Region)
selectSmallest xs =
  case xs of
    [] -> Nothing
    _  -> Just (List.minimumBy compareRegionSize xs)

compareRegionSize :: (a, Ann.Region) -> (a, Ann.Region) -> Ordering
compareRegionSize (_, r1) (_, r2) = compare (area r1) (area r2)
  where
    area (Ann.Region (Ann.Position sr sc) (Ann.Position er ec)) = (er - sr) * 100000 + (ec - sc)


-- Traverse canonical declarations
findInDecls :: Ann.Position -> Can.Decls -> [(Found, Ann.Region)]
findInDecls pos decls =
  case decls of
    Can.Declare def rest -> findInDef pos def ++ findInDecls pos rest
    Can.DeclareRec def defs rest -> concatMap (findInDef pos) (def:defs) ++ findInDecls pos rest
    Can.SaveTheEnvironment -> []


-- Traverse canonical definition
findInDef :: Ann.Position -> Can.Def -> [(Found, Ann.Region)]
findInDef pos def =
                case def of
    Can.Def locatedName patterns expr ->
      let nameRegion = Ann.toRegion locatedName
          nameHits = if regionContains nameRegion pos then findInExpr pos expr else []
          patternHits = concatMap (findInPattern pos) patterns
      in nameHits ++ patternHits ++ findInExpr pos expr
    Can.TypedDef locatedName _ annotatedParams expr tipe ->
      let nameRegion = Ann.toRegion locatedName
          fromName = if regionContains nameRegion pos then [(FoundType tipe, nameRegion)] else []
          -- surface the parameter type if hovering within that parameter pattern region
          paramHits = concatMap (\(p, t) -> let pr = Ann.toRegion p in if regionContains pr pos then [(FoundType t, pr)] else []) annotatedParams
      in fromName ++ paramHits ++ findInExpr pos expr


-- Traverse canonical expressions, collecting candidates that contain the position
findInExpr :: Ann.Position -> Can.Expr -> [(Found, Ann.Region)]
findInExpr pos (Ann.At region expr_) =
  if not (regionContains region pos) then [] else
  let here = foundHere expr_ region
      deeper = case expr_ of
        Can.List xs -> concatMap (findInExpr pos) xs
        Can.Negate x -> findInExpr pos x
        Can.Binop _ _ _ _ a b -> findInExpr pos a ++ findInExpr pos b
        Can.Lambda patterns body -> concatMap (findInPattern pos) patterns ++ findInExpr pos body
        Can.Call f args -> findInExpr pos f ++ concatMap (findInExpr pos) args
        Can.If pairs fallback -> concatMap (\(a,b) -> findInExpr pos a ++ findInExpr pos b) pairs ++ findInExpr pos fallback
        Can.Let def body -> findInDef pos def ++ findInExpr pos body
        Can.LetRec defs body -> concatMap (findInDef pos) defs ++ findInExpr pos body
        Can.LetDestruct pattern e body -> findInPattern pos pattern ++ findInExpr pos e ++ findInExpr pos body
        Can.Case scrut branches -> findInExpr pos scrut ++ concatMap (\(Can.CaseBranch p e) -> findInPattern pos p ++ findInExpr pos e) branches
        Can.Access e (Ann.At _ _) -> findInExpr pos e
        Can.Update _ e updates ->
          let updateExprs = map (\(Can.FieldUpdate _ ue) -> ue) (Map.elems updates)
          in findInExpr pos e ++ concatMap (findInExpr pos) updateExprs
        Can.Record fields -> concatMap (findInExpr pos) (Map.elems fields)
        Can.Tuple a b mc -> findInExpr pos a ++ findInExpr pos b ++ maybe [] (findInExpr pos) mc
        _ -> []
  in here ++ deeper


-- If the current expression node carries an annotation, surface it
foundHere :: Can.Expr_ -> Ann.Region -> [(Found, Ann.Region)]
foundHere expr_ region =
  case expr_ of
    Can.VarForeign _ _ ann -> [(FoundAnnotation ann, region)]
    Can.VarCtor _ _ _ _ ann -> [(FoundAnnotation ann, region)]
    Can.VarDebug _ _ ann -> [(FoundAnnotation ann, region)]
    Can.VarOperator _ _ _ ann -> [(FoundAnnotation ann, region)]
    Can.Binop _ _ _ ann _ _ -> [(FoundAnnotation ann, region)]
    _ -> []


-- Traverse canonical patterns; surface types for constructor args using their arg regions
findInPattern :: Ann.Position -> Can.Pattern -> [(Found, Ann.Region)]
findInPattern pos (Ann.At preg patt) =
  if not (regionContains preg pos) then [] else
  case patt of
    Can.PAlias sub _ -> findInPattern pos sub
    Can.PTuple a b mc -> findInPattern pos a ++ findInPattern pos b ++ maybe [] (findInPattern pos) mc
    Can.PList xs -> concatMap (findInPattern pos) xs
    Can.PCons h t -> findInPattern pos h ++ findInPattern pos t
    Can.PCtor { Can._p_args = args } -> concatMap (argHits pos) args
    _ -> []
  where
    argHits :: Ann.Position -> Can.PatternCtorArg -> [(Found, Ann.Region)]
    argHits p (Can.PatternCtorArg _ tipe argPattern) =
      let argRegion = Ann.toRegion argPattern in
      if regionContains argRegion p
        then (FoundType tipe, argRegion) : findInPattern p argPattern
        else []
