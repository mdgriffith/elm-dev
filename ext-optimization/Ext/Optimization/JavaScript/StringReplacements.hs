{-# LANGUAGE OverloadedStrings #-}
module Ext.Optimization.JavaScript.StringReplacements
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
  if DirectCalls.isEnabled mode && home == ModuleName.string then
    case Name.toChars name of
      "join" -> Just (varF2 home name [sep, strs] joinBody)
      "repeat" -> Just (varF2 home name [n, chunk] repeatBody)
      _ -> Nothing
  else
    Nothing


varF2 :: ModuleName.Canonical -> Name.Name -> [JsName.Name] -> [JS.Stmt] -> JS.Stmt
varF2 home name args body =
  JS.Var (JsName.fromGlobal home name) $
    JS.Call (JS.Ref (JsName.makeF 2)) [ JS.Function Nothing args body ]


joinBody :: [JS.Stmt]
joinBody =
  [ JS.IfStmt (not_ (access strs b)) (JS.Block [ JS.Return emptyString ]) JS.EmptyStmt
  , JS.Var acc (infix_ JS.OpAdd emptyString (access strs a))
  , assignRef strs (access strs b)
  , JS.While
      (access strs b)
      (JS.Block
        [ assignRef acc (infix_ JS.OpAdd (infix_ JS.OpAdd (infix_ JS.OpAdd emptyString (ref acc)) (ref sep)) (access strs a))
        , assignRef strs (access strs b)
        ])
  , JS.Return (ref acc)
  ]


repeatBody :: [JS.Stmt]
repeatBody =
  [ JS.Var result emptyString
  , JS.While
      (JS.Bool True)
      (JS.Block
        [ JS.IfStmt
            (infix_ JS.OpLe (ref n) (JS.Int 0))
            (JS.Block [ JS.Return (ref result) ])
            (JS.Block
              [ assignRef result $
                  JS.If
                    (not_ (infix_ JS.OpBitwiseAnd (ref n) (JS.Int 1)))
                    (ref result)
                    (infix_ JS.OpAdd (ref result) (ref chunk))
              , assignRef n (infix_ JS.OpSpRShift (ref n) (JS.Int 1))
              , assignRef chunk (infix_ JS.OpAdd (ref chunk) (ref chunk))
              ])
        ])
  ]


emptyString :: JS.Expr
emptyString =
  JS.String mempty


ref :: JsName.Name -> JS.Expr
ref = JS.Ref


access :: JsName.Name -> JsName.Name -> JS.Expr
access source field =
  JS.Access (ref source) field


assignRef :: JsName.Name -> JS.Expr -> JS.Stmt
assignRef name expr =
  JS.ExprStmt (JS.Assign (JS.LRef name) expr)


infix_ :: JS.InfixOp -> JS.Expr -> JS.Expr -> JS.Expr
infix_ = JS.Infix


not_ :: JS.Expr -> JS.Expr
not_ = JS.Prefix JS.PrefixNot


a, acc, b, chunk, n, result, sep, strs :: JsName.Name
a = local "a"
acc = local "acc"
b = local "b"
chunk = local "chunk"
n = local "n"
result = local "result"
sep = local "sep"
strs = local "strs"


local :: String -> JsName.Name
local = JsName.fromLocal . Name.fromChars
