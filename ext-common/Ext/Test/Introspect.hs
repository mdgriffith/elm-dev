module Ext.Test.Introspect
  ( findTests
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Name as Name
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified Elm.Interface as I
import qualified AST.Canonical as Can


type ModuleName = ModuleName.Raw
type ValueName = Name.Name


findTests :: Map.Map ModuleName I.Interface -> [(ModuleName, ValueName)]
findTests ifaceByModule =
  Map.foldrWithKey collect [] ifaceByModule
  where
    collect modName iface acc =
      let vals = I._values iface
          testsHere = [ (modName, valName)
                      | (valName, Can.Forall _ tipe) <- Map.toList vals
                      , isTestType (resultType tipe)
                      ]
      in testsHere ++ acc


resultType :: Can.Type -> Can.Type
resultType tipe =
  case tipe of
    Can.TLambda _ out -> resultType out
    _ -> tipe


isTestType :: Can.Type -> Bool
isTestType tipe =
  case tipe of
    Can.TType (ModuleName.Canonical pkg mod_) name _args ->
      pkgIsElmExplorations pkg && Name.toChars mod_ == "Test" && Name.toChars name == "Test"
    Can.TAlias _ _ _ aliasBody ->
      case aliasBody of
        Can.Filled t -> isTestType t
        Can.Holey t -> isTestType t
    _ -> False


pkgIsElmExplorations :: Pkg.Name -> Bool
pkgIsElmExplorations pkg =
  let authorProject = Pkg.toChars pkg
  in take 18 authorProject == "elm-explorations/"



