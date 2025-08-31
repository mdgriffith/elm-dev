module Ext.Test.Introspect
  ( findTests
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Name as Name
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified Elm.Interface as I
import qualified AST.Canonical as Can
import qualified Ext.Log
import           StandaloneInstances ()


type ModuleName = ModuleName.Raw
type ValueName = Name.Name


findTests :: Map.Map ModuleName I.Interface -> IO [(ModuleName, ValueName)]
findTests ifaceByModule = do
  -- Map.foldrWithKey collect [] ifaceByModule
  Ext.Log.log Ext.Log.Test ("Found " <> show ifaceByModule)
  pure (Map.foldrWithKey collect [] ifaceByModule)
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



{-
Example real interface we're looking for:

    { _home = Name "author" "project"
    , _values = 
        [ ("suite"
          ,(Forall (Map.fromList []) 
                (TAlias 
                    (Module.Canonical  (Name "elm-explorations" "test") "Test") 
                    "Test" 
                    [] 
                    (Filled 
                    (TType (Module.Canonical (Name "elm-explorations" "test") "Test.Internal") "Test" []
                
                )))))]
    , _unions = fromList []
    , _aliases = fromList []
    , _binops = fromList []
    }
    )]


-}
isTestType :: Can.Type -> Bool
isTestType tipe =
  case tipe of
    Can.TType (ModuleName.Canonical pkg moduleNmae) name _args ->
      pkgIsElmExplorations pkg
        && Name.toChars name == "Test"
        && (Name.toChars moduleNmae == "Test" || Name.toChars moduleNmae == "Test.Internal")
    Can.TAlias _ _ _ aliasBody ->
      case aliasBody of
        Can.Filled t -> isTestType t
        Can.Holey t -> isTestType t
    _ -> False


pkgIsElmExplorations :: Pkg.Name -> Bool
pkgIsElmExplorations pkg =
  Pkg.toChars pkg == "elm-explorations/test"



