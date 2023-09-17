{-# LANGUAGE OverloadedStrings #-}

module Ext.Dev.Find.Canonical
  ( usedModules
  , usedValues
  )
where

import AST.Canonical (Type (..))
import qualified AST.Canonical as Can
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List as List
import Data.Text (Text, pack)
import qualified Data.Text as T

import Data.Function ((&))
import Data.Name (Name)
import qualified Data.Name as Name

import qualified Elm.Details
import qualified Elm.ModuleName as ModuleName
import qualified Elm.String
import qualified Ext.CompileProxy
import Json.Encode ((==>))
import qualified Json.Encode
import qualified Json.String

import qualified Reporting.Annotation as A
import Reporting.Error.Docs (SyntaxProblem (Name))
import StandaloneInstances



usedValues :: Can.Module -> Set.Set (ModuleName.Canonical, Name)
usedValues (Can.Module name exports docs decls unions aliases binops effects) = 
   usedValueInDecls decls Set.empty





usedValueInDecls :: Can.Decls -> Set.Set (ModuleName.Canonical, Name) -> Set.Set (ModuleName.Canonical, Name)
usedValueInDecls decls found =
    case decls of
        Can.Declare def moarDecls ->
            (usedValueInDef def found)
                & usedValueInDecls moarDecls
        
        Can.DeclareRec def defs moarDecls ->
            List.foldl (flip usedValueInDef) found (def : defs)
                & usedValueInDecls moarDecls

        Can.SaveTheEnvironment ->
            found
            

usedValueInDef :: Can.Def -> Set.Set (ModuleName.Canonical, Name) -> Set.Set (ModuleName.Canonical, Name)
usedValueInDef def found =
    case def of
        Can.Def _ patterns expr ->
            usedValueInExpr expr found
        
        Can.TypedDef _ _ patternTypes expr tipe ->
            usedValueInExpr expr found




usedValueInExpr :: Can.Expr -> Set.Set (ModuleName.Canonical, Name) -> Set.Set (ModuleName.Canonical, Name)
usedValueInExpr (A.At pos expr) found =
    case expr of
        Can.VarLocal _ ->
            found

        Can.VarTopLevel canMod name ->
            Set.insert (canMod, name) found
            
        Can.VarKernel _ _ ->
            found

        Can.VarForeign canMod name annotation ->
            Set.insert (canMod, name) found

        Can.VarCtor _ canMod name index annotation ->
            Set.insert (canMod, name) found

        Can.VarDebug canMod name annotation ->
            Set.insert (canMod, name) found

        Can.VarOperator _ canMod name annotation ->
            Set.insert (canMod, name) found

        Can.Chr _ ->
            found

        Can.Str _ ->
            found

        Can.Int _ ->
            found

        Can.Float _ ->
            found

        Can.List exprList ->
            List.foldr usedValueInExpr found exprList

        Can.Negate expr ->
            usedValueInExpr expr found

        Can.Binop _ canMod name annotation exprOne exprTwo ->
            Set.insert (canMod, name) found
                 & usedValueInExpr exprOne
                 & usedValueInExpr exprTwo

        Can.Lambda patternList expr ->
            usedValueInExpr expr found

        Can.Call expr exprList ->
            List.foldr usedValueInExpr found exprList
                & usedValueInExpr expr

        Can.If listTuple expr ->
            List.foldr 
                (\(oneExpr, twoExpr) f ->
                        usedValueInExpr oneExpr f 
                            & usedValueInExpr twoExpr
                ) 
                found listTuple
                & usedValueInExpr expr

        Can.Let def expr ->
            found
                 & usedValueInDef def
                 & usedValueInExpr expr

        Can.LetRec defList expr ->
             List.foldr usedValueInDef found defList
                & usedValueInExpr expr

        Can.LetDestruct pattern oneExpr twoExpr ->
            found
                 & usedValueInExpr oneExpr
                 & usedValueInExpr twoExpr

        Can.Case expr branches ->
           branches
                & List.foldr usedValuesInBranch
                     (usedValueInExpr expr found)

        Can.Accessor _ ->
            found

        Can.Access expr _ ->
            usedValueInExpr expr found

        Can.Update _ expr record ->
            Map.elems record
                & List.foldl (\innerFound (Can.FieldUpdate _ expr) -> usedValueInExpr expr innerFound) found
                & usedValueInExpr expr

        Can.Record record ->
            Map.elems record
                & List.foldl (flip usedValueInExpr) found
        
        Can.Unit ->
            found

        Can.Tuple one two Nothing ->
            usedValueInExpr one found
                & usedValueInExpr two

        Can.Tuple one two (Just three) ->
            usedValueInExpr one found
                & usedValueInExpr two
                & usedValueInExpr three

        Can.Shader _ _ ->
            found



usedValuesInBranch :: Can.CaseBranch -> Set.Set (ModuleName.Canonical, Name) -> Set.Set (ModuleName.Canonical, Name)
usedValuesInBranch (Can.CaseBranch pattern expr) found =
    usedValueInExpr expr found




{- Used Modules -}


usedModules :: Can.Module -> Set.Set ModuleName.Canonical
usedModules (Can.Module name exports docs decls unions aliases binops effects) = 
    Set.unions 
        [ usedInDecls decls Set.empty
        , Map.foldr (usedInUnion) Set.empty unions
        , Map.foldr (usedInAlias) Set.empty aliases
        ]

    
    
    
usedInDecls :: Can.Decls -> Set.Set ModuleName.Canonical -> Set.Set ModuleName.Canonical
usedInDecls decls found =
    case decls of
        Can.Declare def moarDecls ->
            (usedInDef def found)
                & usedInDecls moarDecls
        
        Can.DeclareRec def defs moarDecls ->
            List.foldl (flip usedInDef) found (def : defs)
                & usedInDecls moarDecls

        Can.SaveTheEnvironment ->
            found
            

usedInDef :: Can.Def -> Set.Set ModuleName.Canonical -> Set.Set ModuleName.Canonical
usedInDef def found =
    case def of
        Can.Def _ patterns expr ->
            List.foldl (flip usedInPattern) found patterns
                & usedInExpr expr
        
        Can.TypedDef _ _ patternTypes expr tipe ->
            List.foldl
                (\innerFound (pattern, patternTipe) -> 
                    innerFound
                        & usedInPattern pattern  
                        & usedInType patternTipe
                ) found patternTypes
                & usedInExpr expr
                & usedInType tipe


usedInPattern :: Can.Pattern -> Set.Set ModuleName.Canonical -> Set.Set ModuleName.Canonical
usedInPattern (A.At _ pattern) found =
    case pattern of
        Can.PCtor modName _ union _ _ args ->
            List.foldl 
                (\innerFound (Can.PatternCtorArg _ tipe pattern) -> 
                    innerFound
                        & usedInType tipe
                        & usedInPattern pattern
                ) 
                found args
                & usedInUnion union
                & Set.insert modName

        Can.PCons consOne consTwo ->
            usedInPattern consOne found
                & usedInPattern consTwo

        Can.PList patterns ->
            List.foldr usedInPattern found patterns

        Can.PAlias pattern _ ->
            usedInPattern pattern found
        
        Can.PTuple one two Nothing ->
            usedInPattern one found
                & usedInPattern two
        
        Can.PTuple one two (Just three) ->
            usedInPattern one found
                & usedInPattern two
                & usedInPattern three
        
        _ ->
            found
            
            
usedInBranch :: Can.CaseBranch -> Set.Set ModuleName.Canonical -> Set.Set ModuleName.Canonical
usedInBranch (Can.CaseBranch pattern expr) found =
    usedInExpr expr found
        & usedInPattern pattern


usedInExpr :: Can.Expr -> Set.Set ModuleName.Canonical -> Set.Set ModuleName.Canonical
usedInExpr (A.At pos expr) found =
    case expr of
        Can.VarLocal _ ->
            found

        Can.VarTopLevel used _ ->
            Set.insert used found
            
        Can.VarKernel _ _ ->
            found

        Can.VarForeign used _ annotation ->
            Set.insert used found
                & usedInAnnotation annotation 

        Can.VarCtor _ used _ index annotation ->
            Set.insert used found
                & usedInAnnotation annotation

        Can.VarDebug used _ annotation ->
            Set.insert used found
                & usedInAnnotation annotation

        Can.VarOperator _ used _ annotation ->
            Set.insert used found
                & usedInAnnotation annotation

        Can.Chr _ ->
            found

        Can.Str _ ->
            found

        Can.Int _ ->
            found

        Can.Float _ ->
            found

        Can.List exprList ->
            List.foldr usedInExpr found exprList

        Can.Negate expr ->
            usedInExpr expr found

        Can.Binop name used _ annotation exprOne exprTwo ->
            Set.insert used found
                 & usedInExpr exprOne
                 & usedInExpr exprTwo
                 & usedInAnnotation annotation

        Can.Lambda patternList expr ->
            List.foldr usedInPattern found patternList
                & usedInExpr expr

        Can.Call expr exprList ->
            List.foldr usedInExpr found exprList
                & usedInExpr expr
        Can.If listTuple expr ->
            List.foldr 
                (\(oneExpr, twoExpr) f ->
                        usedInExpr oneExpr f 
                            & usedInExpr twoExpr
                ) 
                found listTuple
                & usedInExpr expr

        Can.Let def expr ->
            found
                 & usedInDef def
                 & usedInExpr expr

        Can.LetRec defList expr ->
             List.foldr usedInDef found defList
                & usedInExpr expr

        Can.LetDestruct pattern oneExpr twoExpr ->
            found
                 & usedInPattern pattern
                 & usedInExpr oneExpr
                 & usedInExpr twoExpr

        Can.Case expr branches ->
           branches
                & List.foldr usedInBranch
                     (usedInExpr expr found)

        Can.Accessor _ ->
            found

        Can.Access expr _ ->
            usedInExpr expr found

        Can.Update _ expr record ->
            Map.elems record
                & List.foldl (\innerFound (Can.FieldUpdate _ expr) -> usedInExpr expr innerFound) found
                & usedInExpr expr

        Can.Record record ->
            Map.elems record
                & List.foldl (flip usedInExpr) found
        
        Can.Unit ->
            found

        Can.Tuple one two Nothing ->
            usedInExpr one found
                & usedInExpr two

        Can.Tuple one two (Just three) ->
            usedInExpr one found
                & usedInExpr two
                & usedInExpr three

        Can.Shader _ _ ->
            found



usedInUnion :: Can.Union -> Set.Set ModuleName.Canonical -> Set.Set ModuleName.Canonical
usedInUnion (Can.Union _ constructors _ _) found =
    List.foldl 
        (\innerFound (Can.Ctor _ _ _ tipes) -> 
            List.foldl (\f tipe -> usedInType tipe f) innerFound tipes
        ) 
        found
        constructors


usedInAlias :: Can.Alias -> Set.Set ModuleName.Canonical -> Set.Set ModuleName.Canonical
usedInAlias (Can.Alias _ tipe) found =
    usedInType tipe found



usedInAnnotation :: Can.Annotation -> Set.Set ModuleName.Canonical -> Set.Set ModuleName.Canonical
usedInAnnotation (Can.Forall freevars type_) found =
    usedInType type_ found


usedInType :: Can.Type -> Set.Set ModuleName.Canonical -> Set.Set ModuleName.Canonical
usedInType type_ found =
    case type_ of 
        Can.TLambda typeOne typeTwo ->
            found
                & usedInType typeOne
                & usedInType typeTwo

        Can.TVar _ ->
            found

        Can.TType modName name types ->
            List.foldl (flip usedInType) found types
                & Set.insert modName
                
        Can.TRecord fields _ ->
            List.foldr (\(Can.FieldType _ tipe) -> usedInType tipe) 
                found
                fields

        Can.TUnit ->
            found

        Can.TTuple one two Nothing ->
            found
                & usedInType one
                & usedInType two

        Can.TTuple one two (Just three) ->
            found
                & usedInType one
                & usedInType two
                & usedInType three

        Can.TAlias modName _ fields (Can.Holey aliasType) ->
             List.foldr (\(_, tipe) -> usedInType tipe) 
                found
                fields
                & usedInType aliasType
                & Set.insert modName

        Can.TAlias modName _ fields (Can.Filled aliasType) ->
             List.foldr (\(_, tipe) -> usedInType tipe) 
                found
                fields
                & usedInType aliasType
                & Set.insert modName




data Found
    = FoundDef Can.Def
    | FoundUnion Can.Union
    | FoundAlias Can.Alias


fromName :: Name -> Can.Module -> Maybe Found
fromName name (Can.Module modName exports docs decls unions aliases binops effects) = 
    (FoundUnion <$> Map.lookup name unions)
        & orLookup FoundAlias name aliases
        & orFindDecl name decls

orFindDecl :: Name -> Can.Decls -> Maybe Found -> Maybe Found
orFindDecl name decls previous =
    case previous of
        Nothing ->
            findDeclNamed name decls

        _ ->
          previous


findDeclNamed :: Name -> Can.Decls -> Maybe Found
findDeclNamed name decls =
    case decls of
        Can.SaveTheEnvironment ->
            Nothing

        Can.Declare def moarDecls ->
            if defNamed def name then
                Just (FoundDef def)

            else
                findDeclNamed name moarDecls

        Can.DeclareRec def subDefs moarDecls ->
            if defNamed def name then
                Just (FoundDef def)

            else
                findDeclNamed name moarDecls


defNamed :: Can.Def -> Name -> Bool
defNamed def name =
    case def of
        Can.Def (A.At _ defName) _ _ ->
            name == defName
        
        Can.TypedDef (A.At _ defName) _ _ _ _ ->
            name == defName


orLookup toResult name map previousResult =
    case previousResult of
        Nothing ->
            toResult <$> Map.lookup name map

        _ ->
            previousResult
