{-# LANGUAGE OverloadedStrings #-}

module Ext.Dev.CallGraph
  ( callgraph, encode
  )
where

import AST.Canonical (Type (..))
import qualified AST.Canonical as Can
import qualified AST.Optimized as Opt
import qualified AST.Source as Src

import Data.Function ((&))
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Name as Name
import Data.Name (Name)
import qualified Ext.CompileProxy
import qualified Elm.Package as Package
import qualified Elm.ModuleName as ModuleName

import qualified Reporting.Annotation as A

import qualified Json.Encode
import Json.Encode ((==>))


data Calls =
    Calls
        { _nodes :: [ Node ]

        }

data Node =
    Node 
        { _id :: Id
        , _recursive :: Bool
        , _calls :: Set.Set Call
        , _callers :: Set.Set Id
        }

data Call =
    Call 
        { _callType :: CallType 
        , _callId :: Id
        }
        deriving (Eq, Ord)

data CallType =
    Local | TopLevel | Foreign | Constructor | Debug | Operator
    deriving (Eq, Ord)

data Id =
    Id 
        { _mod :: ModuleName.Canonical 
        , _name :: Name
        }
        deriving (Eq, Ord)


encode :: Calls -> Json.Encode.Value
encode (Calls nodes) =
    Json.Encode.list
        encodeNode
        nodes

encodeNode :: Node -> Json.Encode.Value
encodeNode (Node id recursive callsMade callers) =
    Json.Encode.object 
        [ "id" ==> encodeId id
        , "recursive" ==> Json.Encode.bool recursive
        , "calls" ==> Json.Encode.list encodeCall (Set.toList callsMade)
        , "callers" ==> Json.Encode.list encodeId (Set.toList callers)
        ]



encodeCall :: Call -> Json.Encode.Value
encodeCall (Call callType id) =
     Json.Encode.object 
        [ "id" ==> encodeId id
        , "callType" ==> encodeCallType callType
        ]

encodeCallType :: CallType -> Json.Encode.Value
encodeCallType callType =
    case callType of
        Local ->  Json.Encode.chars "local"
        TopLevel ->  Json.Encode.chars "top-level"
        Foreign ->  Json.Encode.chars "foreign"
        Constructor ->  Json.Encode.chars "constructor"
        Debug ->  Json.Encode.chars "debug"
        Operator ->  Json.Encode.chars "operator"



encodeId :: Id -> Json.Encode.Value
encodeId (Id (ModuleName.Canonical pkgName modName) name) =
     Json.Encode.chars
        (Package.toChars pkgName 
            <> "$"
            <> Name.toChars modName
            <> "."
            <> (Name.toChars name)
        )
    



callgraph :: String -> String -> IO (Maybe Calls)
callgraph root path = do
    (Ext.CompileProxy.Single source warnings interfaces canonical compiled) <- Ext.CompileProxy.loadSingle root path
    case (source, canonical) of
        (Right srcModule, Just canMod) -> do
             pure (Just (calls canMod))

        (_, _) ->
             pure Nothing



calls :: Can.Module -> Calls
calls (Can.Module name exports docs decls unions aliases binops effects) = 
    Calls (callsInDecls name decls [])


{-  -}



callsInDecls :: ModuleName.Canonical -> Can.Decls -> [ Node ] -> [ Node ]
callsInDecls moduleName decls found =
    case decls of
        Can.Declare def moarDecls ->
            (defToNodes moduleName def found)
                & callsInDecls moduleName moarDecls            
        
        Can.DeclareRec def defs moarDecls ->
            List.foldl (flip (defToNodes moduleName)) found (def : defs)
                & callsInDecls moduleName moarDecls

        Can.SaveTheEnvironment ->
            found
            

defToNodes :: ModuleName.Canonical -> Can.Def -> [ Node ] -> [ Node ]
defToNodes moduleName def found =
    case def of
        Can.Def (A.At _ defName) patterns expr ->
            let 
                callsMade = callsInExpr moduleName expr Set.empty
            in
            Node (Id moduleName defName) False callsMade Set.empty : found
            
        
        Can.TypedDef (A.At _ defName) _ patternTypes expr tipe ->
            let 
                callsMade = callsInExpr moduleName expr Set.empty
            in
            Node (Id moduleName defName) False callsMade Set.empty : found



callsInDef :: ModuleName.Canonical -> Can.Def -> Set.Set Call -> Set.Set Call
callsInDef selfCanMod def found =
    case def of
        Can.Def (A.At _ defName) patterns expr ->
            callsInExpr selfCanMod expr found

        Can.TypedDef (A.At _ defName) _ patternTypes expr tipe ->
            callsInExpr selfCanMod expr found
            

            
callsInBranch :: ModuleName.Canonical -> Can.CaseBranch -> Set.Set Call -> Set.Set Call
callsInBranch selfCanMod (Can.CaseBranch pattern expr) found =
    callsInExpr selfCanMod expr found


callsInExpr :: ModuleName.Canonical -> Can.Expr -> Set.Set Call -> Set.Set Call
callsInExpr selfCanMod (A.At pos expr) found =
    case expr of
        Can.VarLocal name ->
            Set.insert (Call Local (Id selfCanMod name)) found

        Can.VarTopLevel canMod name ->
            Set.insert (Call TopLevel (Id canMod name)) found
            
        Can.VarKernel _ _ ->
            found

        Can.VarForeign canMod name annotation ->
            Set.insert (Call Foreign (Id canMod name)) found
                

        Can.VarCtor _ canMod name index annotation ->
            Set.insert (Call Constructor (Id canMod name)) found 

        Can.VarDebug canMod name annotation ->
            Set.insert (Call Debug (Id canMod name)) found
                
        Can.VarOperator _ canMod name annotation ->
            Set.insert (Call Operator (Id canMod name)) found

        Can.Chr _ ->
            found

        Can.Str _ ->
            found

        Can.Int _ ->
            found

        Can.Float _ ->
            found

        Can.List exprList ->
            List.foldr (callsInExpr selfCanMod) found exprList

        Can.Negate expr ->
            callsInExpr selfCanMod expr found

        Can.Binop _ canMod name annotation exprOne exprTwo ->
            Set.insert (Call Operator (Id canMod name)) found
                 & callsInExpr selfCanMod exprOne
                 & callsInExpr selfCanMod exprTwo

        Can.Lambda patternList expr ->
            callsInExpr selfCanMod expr found

        Can.Call expr exprList ->
            List.foldr (callsInExpr selfCanMod) found exprList
                & callsInExpr selfCanMod expr

        Can.If listTuple expr ->
            List.foldr 
                (\(oneExpr, twoExpr) f ->
                    callsInExpr selfCanMod oneExpr f 
                        & callsInExpr selfCanMod twoExpr
                ) 
                found listTuple
                & callsInExpr selfCanMod expr

        Can.Let def expr ->
            found
                 & callsInDef selfCanMod def
                 & callsInExpr selfCanMod expr

        Can.LetRec defList expr ->
             List.foldr (callsInDef selfCanMod) found defList
                & callsInExpr selfCanMod expr

        Can.LetDestruct pattern oneExpr twoExpr ->
            found
                 & callsInExpr selfCanMod oneExpr
                 & callsInExpr selfCanMod twoExpr

        Can.Case expr branches ->
           branches
                & List.foldr (callsInBranch selfCanMod)
                     (callsInExpr selfCanMod expr found)

        Can.Accessor _ ->
            found

        Can.Access expr _ ->
            callsInExpr selfCanMod expr found

        Can.Update _ expr record ->
            Map.elems record
                & List.foldl (\innerFound (Can.FieldUpdate _ expr) -> callsInExpr selfCanMod expr innerFound) found
                & callsInExpr selfCanMod expr

        Can.Record record ->
            Map.elems record
                & List.foldl (flip (callsInExpr selfCanMod)) found
        
        Can.Unit ->
            found

        Can.Tuple one two Nothing ->
            callsInExpr selfCanMod one found
                & callsInExpr selfCanMod two

        Can.Tuple one two (Just three) ->
            callsInExpr selfCanMod one found
                & callsInExpr selfCanMod two
                & callsInExpr selfCanMod three

        Can.Shader _ _ ->
            found