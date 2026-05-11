{-# LANGUAGE OverloadedStrings #-}
module Ext.Optimization.JavaScript.DirectCalls
  ( directCall
  , isEnabled
  , rawGlobalName
  , rawArity
  , splitGlobalFunction
  )
  where


import qualified Data.Map as Map
import qualified Data.Name as Name

import qualified AST.Optimized as Opt
import qualified Ext.Optimization.Level as Level
import qualified Generate.JavaScript.Builder as JS
import qualified Generate.JavaScript.Name as JsName
import qualified Generate.Mode as Mode


isEnabled :: Mode.Mode -> Bool
isEnabled mode =
  case mode of
    Mode.Prod Level.O2 _ _ _ -> True
    Mode.Prod Level.O3 _ _ _ -> True
    _ -> False


rawArity :: Mode.Mode -> Opt.Global -> Maybe Int
rawArity mode global =
  case mode of
    Mode.Prod level _ rawFunctions _ | level == Level.O2 || level == Level.O3 ->
      Mode._rawArity <$> Map.lookup global rawFunctions

    _ ->
      Nothing


rawGlobalName :: Mode.Mode -> Opt.Global -> Maybe JsName.Name
rawGlobalName mode global =
  case mode of
    Mode.Prod level _ rawFunctions _ | level == Level.O2 || level == Level.O3 ->
      toRawName . Mode._rawGlobal <$> Map.lookup global rawFunctions

    _ ->
      Nothing


directCall :: Mode.Mode -> Opt.Global -> [JS.Expr] -> Maybe JS.Expr
directCall mode global args =
  case (rawArity mode global, rawGlobalName mode global) of
    (Just arity, Just rawName) | arity == length args ->
      Just (JS.Call (JS.Ref rawName) args)

    _ ->
      Nothing


splitGlobalFunction :: Mode.Mode -> Opt.Global -> [Name.Name] -> [JS.Stmt] -> Maybe JS.Stmt
splitGlobalFunction mode global@(Opt.Global home name) args body =
  case rawArity mode global of
    Just arity | arity == length args ->
      let
        rawName = toRawName global
        wrappedName = JsName.fromGlobal home name
      in
      Just $ JS.Block
        [ JS.Var rawName (JS.Function Nothing (map JsName.fromLocal args) body)
        , JS.Var wrappedName (JS.Call (JS.Ref (JsName.makeF arity)) [ JS.Ref rawName ])
        ]

    _ ->
      Nothing


toRawName :: Opt.Global -> JsName.Name
toRawName (Opt.Global home name) =
  JsName.fromGlobalSuffix home name "_fn"
