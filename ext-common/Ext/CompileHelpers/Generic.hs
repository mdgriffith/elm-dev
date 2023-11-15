module Ext.CompileHelpers.Generic where

import qualified AST.Canonical as Can
import qualified AST.Optimized as Opt
import qualified AST.Source as Src
import qualified Reporting.Error.Type
import qualified Data.NonEmptyList
import qualified Data.Map as Map
import qualified Data.Name as Name
import qualified Data.OneOrMore as OneOrMore
import qualified Elm.Interface as I
import qualified Elm.ModuleName as ModuleName
import qualified Reporting.Error
import qualified System.IO.Unsafe
import qualified Type.Constrain.Module as Type
import qualified Type.Solve as Type
import qualified StandaloneInstances

data Artifacts =
  Artifacts
    { _ifaces :: Map.Map ModuleName.Raw I.Interface
    , _graph :: Opt.GlobalGraph
    }
    deriving (Show)


toInterfaces :: Map.Map ModuleName.Canonical I.DependencyInterface -> Map.Map ModuleName.Raw I.Interface
toInterfaces deps =
  Map.mapMaybe toUnique $ Map.fromListWith OneOrMore.more $
    Map.elems (Map.mapMaybeWithKey getPublic deps)


getPublic :: ModuleName.Canonical -> I.DependencyInterface -> Maybe (ModuleName.Raw, OneOrMore.OneOrMore I.Interface)
getPublic (ModuleName.Canonical _ name) dep =
  case dep of
    I.Public  iface -> Just (name, OneOrMore.one iface)
    I.Private _ _ _ -> Nothing


toUnique :: OneOrMore.OneOrMore a -> Maybe a
toUnique oneOrMore =
  case oneOrMore of
    OneOrMore.One value -> Just value
    OneOrMore.More _ _  -> Nothing



typeCheck :: 
  Src.Module 
    -> Can.Module 
    -> Either 
        (Data.NonEmptyList.List
            Reporting.Error.Type.Error) 
        (Map.Map Name.Name Can.Annotation)
typeCheck modul canonical =
  System.IO.Unsafe.unsafePerformIO (Type.run =<< Type.constrain canonical)
    
