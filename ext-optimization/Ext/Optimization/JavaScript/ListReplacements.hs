{-# LANGUAGE OverloadedStrings #-}
module Ext.Optimization.JavaScript.ListReplacements
  ( replacement
  ) where


import qualified Data.Name as Name

import qualified AST.Optimized as Opt
import qualified Elm.ModuleName as ModuleName
import qualified Ext.Optimization.JavaScript.DirectCalls as DirectCalls
import qualified Generate.JavaScript.Builder as JS
import qualified Generate.JavaScript.Name as JsName
import qualified Generate.Mode as Mode


replacement :: Mode.Mode -> ModuleName.Canonical -> Name.Name -> Opt.Global -> Maybe JS.Stmt
replacement mode home name (Opt.Global _ _) =
  if DirectCalls.isEnabled mode && home == ModuleName.list then
    case Name.toChars name of
      "all" -> Just (varF2 home name [isOkay, list] allBody)
      "append" -> Just (varF2 home name [xs, ys] (copyListBody xs ys))
      "concat" -> Just (varFn home name [lists] concatBody)
      "concatMap" -> Just (varF2 home name [f, lists] concatMapBody)
      "filter" -> Just (varF2 home name [f, xs] filterBody)
      "indexedMap" -> Just (varF2 home name [f, xs] indexedMapBody)
      "intersperse" -> Just (varF2 home name [sep, xs] intersperseBody)
      "map" -> Just (varF2 home name [f, xs] mapBody)
      "partition" -> Just (varF2 home name [f, xs] partitionBody)
      "take" -> Just (varF2 home name [n, xs] takeBody)
      "unzip" -> Just (varFn home name [pairs] unzipBody)
      _ -> Nothing
  else
    Nothing


varF2 :: ModuleName.Canonical -> Name.Name -> [JsName.Name] -> [JS.Stmt] -> JS.Stmt
varF2 home name args body =
  JS.Var (JsName.fromGlobal home name) $
    JS.Call (JS.Ref (JsName.makeF 2)) [ JS.Function Nothing args body ]


varFn :: ModuleName.Canonical -> Name.Name -> [JsName.Name] -> [JS.Stmt] -> JS.Stmt
varFn home name args body =
  JS.Var (JsName.fromGlobal home name) $
    JS.Function Nothing args body


mapBody :: [JS.Stmt]
mapBody =
  listBuilder [ whileCons xs [ appendNext (call f [ access xs a ]) ] ]


filterBody :: [JS.Stmt]
filterBody =
  listBuilder
    [ whileCons xs
      [ JS.IfStmt
          (call f [ access xs a ])
          (JS.Block [ appendNext (access xs a) ])
          JS.EmptyStmt
      ]
    ]


indexedMapBody :: [JS.Stmt]
indexedMapBody =
  listBuilder
    [ JS.Var i (JS.Int 0)
    , whileCons xs
      [ appendNext (callA 2 [ ref f, ref i, access xs a ])
      , assignRef i (infix_ JS.OpAdd (ref i) (JS.Int 1))
      ]
    ]


takeBody :: [JS.Stmt]
takeBody =
  listBuilder
    [ JS.Var i (JS.Int 0)
    , JS.While
        (infix_ JS.OpAnd (infix_ JS.OpLt (ref i) (ref n)) (access xs b))
        (JS.Block
          [ appendNext (access xs a)
          , assignRef xs (access xs b)
          , assignRef i (infix_ JS.OpAdd (ref i) (JS.Int 1))
          ])
    ]


appendBody :: JsName.Name -> [JS.Stmt]
appendBody finalTail =
  [ whileCons xs [ appendNext (access xs a) ]
  , assignDot (ref end) b (ref finalTail)
  ]


copyListBody :: JsName.Name -> JsName.Name -> [JS.Stmt]
copyListBody source finalTail =
  listBuilderWith source (appendBody finalTail)


concatBody :: [JS.Stmt]
concatBody =
  [ JS.IfStmt (not_ (access lists b)) (JS.Block [ JS.Return nil ]) JS.EmptyStmt ]
  ++ listBuilderWith lists
    [ JS.While
        (accessExpr (access lists b) b)
        (JS.Block
          [ JS.Var xs (access lists a)
          , whileCons xs [ appendNext (access xs a) ]
          , assignRef lists (access lists b)
          ])
    , assignDot (ref end) b (access lists a)
    ]


concatMapBody :: [JS.Stmt]
concatMapBody =
  [ JS.IfStmt (not_ (access lists b)) (JS.Block [ JS.Return nil ]) JS.EmptyStmt ]
  ++ listBuilderWith lists
    [ JS.While
        (accessExpr (access lists b) b)
        (JS.Block
          [ JS.Var xs (call f [ access lists a ])
          , whileCons xs [ appendNext (access xs a) ]
          , assignRef lists (access lists b)
          ])
    , assignDot (ref end) b (call f [ access lists a ])
    ]


allBody :: [JS.Stmt]
allBody =
  [ whileCons list
      [ JS.IfStmt
          (not_ (call isOkay [ access list a ]))
          (JS.Block [ JS.Return (JS.Bool False) ])
          JS.EmptyStmt
      ]
  , JS.Return (JS.Bool True)
  ]


intersperseBody :: [JS.Stmt]
intersperseBody =
  [ JS.IfStmt (not_ (access xs b)) (JS.Block [ JS.Return (ref xs) ]) JS.EmptyStmt ]
  ++ listBuilderWith xs
    [ appendNext (access xs a)
    , assignRef xs (access xs b)
    , whileCons xs
      [ JS.Var valNode (cons (access xs a) nil)
      , JS.Var sepNode (cons (ref sep) (ref valNode))
      , assignDot (ref end) b (ref sepNode)
      , assignRef end (ref valNode)
      ]
    ]


partitionBody :: [JS.Stmt]
partitionBody =
  [ JS.Var truesHead (cons JS.Null nil)
  , JS.Var falsesHead (cons JS.Null nil)
  , JS.Var truesEnd (ref truesHead)
  , JS.Var falsesEnd (ref falsesHead)
  , whileCons xs
      [ JS.Var next (cons (access xs a) nil)
      , JS.IfStmt
          (call f [ access xs a ])
          (JS.Block [ assignDot (ref truesEnd) b (ref next), assignRef truesEnd (ref next) ])
          (JS.Block [ assignDot (ref falsesEnd) b (ref next), assignRef falsesEnd (ref next) ])
      ]
  , JS.Return (tuple2 (accessExpr (ref truesHead) b) (accessExpr (ref falsesHead) b))
  ]


unzipBody :: [JS.Stmt]
unzipBody =
  [ JS.Var aHead (cons JS.Null nil)
  , JS.Var bHead (cons JS.Null nil)
  , JS.Var aEnd (ref aHead)
  , JS.Var bEnd (ref bHead)
  , whileCons pairs
      [ JS.Var tuple (access pairs a)
      , JS.Var aNext (cons (accessExpr (ref tuple) a) nil)
      , assignDot (ref aEnd) b (ref aNext)
      , assignRef aEnd (ref aNext)
      , JS.Var bNext (cons (accessExpr (ref tuple) b) nil)
      , assignDot (ref bEnd) b (ref bNext)
      , assignRef bEnd (ref bNext)
      ]
  , JS.Return (tuple2 (accessExpr (ref aHead) b) (accessExpr (ref bHead) b))
  ]


listBuilder :: [JS.Stmt] -> [JS.Stmt]
listBuilder = listBuilderWith xs


listBuilderWith :: JsName.Name -> [JS.Stmt] -> [JS.Stmt]
listBuilderWith source stmts =
  [ JS.Var tmp (cons JS.Null nil)
  , JS.Var end (ref tmp)
  ]
  ++ stmts
  ++ [ JS.Return (accessExpr (ref tmp) b) ]


appendNext :: JS.Expr -> JS.Stmt
appendNext value =
  JS.Block
    [ JS.Var next (cons value nil)
    , assignDot (ref end) b (ref next)
    , assignRef end (ref next)
    ]


whileCons :: JsName.Name -> [JS.Stmt] -> JS.Stmt
whileCons source body =
  JS.While (access source b) (JS.Block (body ++ [ assignRef source (access source b) ]))


call :: JsName.Name -> [JS.Expr] -> JS.Expr
call name args =
  JS.Call (ref name) args


callA :: Int -> [JS.Expr] -> JS.Expr
callA arity args =
  JS.Call (JS.Ref (JsName.makeA arity)) args


cons :: JS.Expr -> JS.Expr -> JS.Expr
cons hd tl =
  JS.Call (JS.Ref (JsName.fromKernel listKernel (Name.fromChars "Cons"))) [ hd, tl ]


tuple2 :: JS.Expr -> JS.Expr -> JS.Expr
tuple2 left right =
  JS.Call (JS.Ref (JsName.fromKernel utilsKernel (Name.fromChars "Tuple2"))) [ left, right ]


nil :: JS.Expr
nil =
  JS.Ref (JsName.fromKernel listKernel (Name.fromChars "Nil"))


ref :: JsName.Name -> JS.Expr
ref = JS.Ref


access :: JsName.Name -> JsName.Name -> JS.Expr
access source field =
  JS.Access (ref source) field


accessExpr :: JS.Expr -> JsName.Name -> JS.Expr
accessExpr = JS.Access


assignRef :: JsName.Name -> JS.Expr -> JS.Stmt
assignRef name expr =
  JS.ExprStmt (JS.Assign (JS.LRef name) expr)


assignDot :: JS.Expr -> JsName.Name -> JS.Expr -> JS.Stmt
assignDot object field expr =
  JS.ExprStmt (JS.Assign (JS.LDot object field) expr)


infix_ :: JS.InfixOp -> JS.Expr -> JS.Expr -> JS.Expr
infix_ = JS.Infix


not_ :: JS.Expr -> JS.Expr
not_ = JS.Prefix JS.PrefixNot


listKernel :: Name.Name
listKernel = Name.fromChars "List"


utilsKernel :: Name.Name
utilsKernel = Name.fromChars "Utils"


a, b, aHead, aEnd, aNext, bHead, bEnd, bNext, end, f, falsesEnd, falsesHead, i, isOkay, list, lists, n, next, pairs, sep, sepNode, tmp, truesEnd, truesHead, tuple, valNode, xs, ys :: JsName.Name
a = local "a"
b = local "b"
aHead = local "aHead"
aEnd = local "aEnd"
aNext = local "aNext"
bHead = local "bHead"
bEnd = local "bEnd"
bNext = local "bNext"
end = local "end"
f = local "f"
falsesEnd = local "falsesEnd"
falsesHead = local "falsesHead"
i = local "i"
isOkay = local "isOkay"
list = local "list"
lists = local "lists"
n = local "n"
next = local "next"
pairs = local "pairs"
sep = local "sep"
sepNode = local "sepNode"
tmp = local "tmp"
truesEnd = local "truesEnd"
truesHead = local "truesHead"
tuple = local "tuple"
valNode = local "valNode"
xs = local "xs"
ys = local "ys"


local :: String -> JsName.Name
local = JsName.fromLocal . Name.fromChars
