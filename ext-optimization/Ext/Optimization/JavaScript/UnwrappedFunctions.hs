{-# LANGUAGE OverloadedStrings #-}
module Ext.Optimization.JavaScript.UnwrappedFunctions
  ( call
  , info
  , name
  )
  where


import qualified Data.Map as Map

import qualified AST.Optimized as Opt
import qualified Ext.Optimization.Level as Level
import qualified Ext.Optimization.JavaScript.DirectCalls as DirectCalls
import qualified Generate.JavaScript.Builder as JS
import qualified Generate.JavaScript.Name as JsName
import qualified Generate.Mode as Mode


info :: Mode.Mode -> Opt.Global -> Maybe Mode.UnwrappedFunction
info mode global =
  case mode of
    Mode.Prod level _ _ unwrappedFunctions | level == Level.O2 || level == Level.O3 ->
      Map.lookup global unwrappedFunctions

    _ ->
      Nothing


name :: Opt.Global -> JsName.Name
name (Opt.Global home localName) =
  JsName.fromGlobalSuffix home localName "_unwrapped"


call :: Mode.Mode -> Opt.Global -> [Opt.Expr] -> [JS.Expr] -> Maybe JS.Expr
call mode global elmArgs jsArgs =
  case info mode global of
    Just unwrapped | Mode._unwrappedArity unwrapped == length elmArgs ->
      let
        paramIndex = Mode._unwrappedParamIndex unwrapped
      in
      case (drop paramIndex elmArgs, drop paramIndex jsArgs) of
        (Opt.VarGlobal callbackGlobal : _, _ : _) ->
          case (DirectCalls.rawArity mode callbackGlobal, DirectCalls.rawGlobalName mode callbackGlobal) of
            (Just callbackArity, Just rawCallbackName) | callbackArity == Mode._unwrappedParamArity unwrapped ->
              Just $ JS.Call (JS.Ref (name global)) $ replaceAt paramIndex (JS.Ref rawCallbackName) jsArgs

            _ ->
              Nothing

        _ ->
          Nothing

    _ ->
      Nothing


replaceAt :: Int -> a -> [a] -> [a]
replaceAt index replacement list =
  case splitAt index list of
    (before, _ : after) -> before ++ replacement : after
    _ -> list
