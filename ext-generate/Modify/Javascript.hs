{-# LANGUAGE OverloadedStrings #-}
module Modify.Javascript
  ( modify
  ) where

import Data.ByteString.Builder as B
import qualified Data.Name as Name
import qualified Elm.ModuleName as ModuleName
import qualified AST.Optimized as Opt
import qualified Generate.JavaScript.Builder as JS
import qualified Generate.JavaScript.Name as JsName


-- | Modify JavaScript generation for specific functions.
-- Returns Just stmt if this function needs special handling, Nothing otherwise.
modify :: ModuleName.Canonical -> Name.Name -> Opt.Global -> Maybe JS.Stmt
modify home name global@(Opt.Global _ _) =
  -- Special case: Everything.check needs runtime test checking code
  if isEverythingCheck home name then
    Just (generateTestCheck global)
  else
    Nothing


-- | Check if this is the Everything.check function that needs special handling
isEverythingCheck :: ModuleName.Canonical -> Name.Name -> Bool
isEverythingCheck (ModuleName.Canonical _ home) name =
  home == Name.fromChars "Everything" && name == Name.fromChars "check"


-- | Generate the runtime test checking code for Everything.check
-- Generates: function(value) { return value && value.$ && value.$.startsWith('ElmTestVariant__') ? $elm$core$Maybe$Just(value) : $elm$core$Maybe$Nothing; }
generateTestCheck :: Opt.Global -> JS.Stmt
generateTestCheck (Opt.Global home name) =
  let
    valueName = JsName.fromLocal (Name.fromChars "value")
    dollarName = JsName.fromLocal (Name.fromChars "$")
    startsWithName = JsName.fromLocal (Name.fromChars "startsWith")
    valueRef = JS.Ref valueName
    
    -- value.$
    valueDollar = JS.Access valueRef dollarName
    
    -- value.$.startsWith('ElmTestVariant__')
    startsWithCall = JS.Call
      (JS.Access valueDollar startsWithName)
      [JS.String (B.stringUtf8 "ElmTestVariant__")]
    
    -- value && value.$ && value.$.startsWith('ElmTestVariant__')
    condition = JS.Infix JS.OpAnd valueRef
      (JS.Infix JS.OpAnd valueDollar startsWithCall)
    
    -- $elm$core$Maybe$Just(value)
    justCall = JS.Call
      (JS.Ref (JsName.fromGlobal ModuleName.maybe (Name.fromChars "Just")))
      [valueRef]
    
    -- $elm$core$Maybe$Nothing
    nothingRef = JS.Ref (JsName.fromGlobal ModuleName.maybe (Name.fromChars "Nothing"))
    
    -- Ternary: condition ? justCall : nothingRef
    ternary = JS.If condition justCall nothingRef
  in
  JS.Var (JsName.fromGlobal home name) $
    JS.Function Nothing [valueName] [JS.Return ternary]
