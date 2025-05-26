module Ext.CompileHelpers.Generic where

import qualified Data.ByteString.Builder as B
import qualified AST.Canonical as Can
import qualified AST.Optimized as Opt
import qualified AST.Source as Src
import qualified Reporting.Error.Type
import qualified Data.NonEmptyList
import qualified Data.Map as Map
import qualified Data.Name as Name
import qualified Data.OneOrMore as OneOrMore
import qualified Elm.Details as Details
import qualified Elm.Interface as I
import qualified Elm.ModuleName as ModuleName
import qualified Reporting.Error
import qualified System.IO.Unsafe
import qualified Type.Constrain.Module as Type
import qualified Type.Solve as Type
import qualified Reporting.Task as Task
import qualified Reporting.Exit as Exit
import qualified StandaloneInstances
import qualified Make
import qualified Generate
import qualified Build

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
    

data DesiredMode = Debug | Dev | Prod

getMode :: Bool -> Bool -> DesiredMode
getMode debug optimize =
  case (debug, optimize) of
    (True , True ) -> Debug
    (True , False) -> Debug
    (False, False) -> Dev
    (False, True ) -> Prod


generate :: FilePath -> Details.Details -> DesiredMode -> Build.Artifacts -> Task.Task Exit.Reactor B.Builder
generate root details desiredMode artifacts =
  Task.mapError Exit.ReactorBadGenerate $
    case desiredMode of
      Debug -> Generate.debug root details artifacts
      Dev   -> Generate.dev   root details artifacts
      Prod  -> Generate.prod  root details artifacts