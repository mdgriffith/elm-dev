module Ext.CompileHelpers.Generic where

import qualified Data.ByteString.Builder as B
import qualified AST.Canonical as Can
import qualified Data.ByteString as B
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
import qualified Elm.Package as Pkg
import qualified Reporting.Error
import qualified System.IO.Unsafe
import qualified Type.Constrain.Module as Type
import qualified Type.Solve as Type
import qualified Reporting.Task as Task
import qualified Reporting.Exit as Exit
import qualified Reporting.Error as Error
import qualified StandaloneInstances
import qualified Compile
import qualified Make
import qualified Generate
import qualified Reporting
import qualified Generate.Html
import qualified Build
import qualified Modify

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
    


data CompilationFlags =
  CompilationFlags
    { modifications :: Modify.Modifications
    }

compilationModsFromFlags :: DesiredMode -> CompilationFlags
compilationModsFromFlags mode =
  case mode of
    Debug -> CompilationFlags { modifications = Modify.Modifications True }
    Dev   -> CompilationFlags { modifications = Modify.Modifications False }
    Prod  -> CompilationFlags { modifications = Modify.Modifications False }

-- This is weirdly named becase it mirrors Compile.compile
-- but does our Canonical updates
compile :: CompilationFlags -> Pkg.Name -> Map.Map ModuleName.Raw I.Interface -> Src.Module -> Either Error.Error Compile.Artifacts
compile (CompilationFlags modifications) pkg ifaces modul =
  do  dirtyCanonical   <- Compile.canonicalize pkg ifaces modul
      let canonical = Modify.update modifications dirtyCanonical
      annotations <- Compile.typeCheck modul canonical
      ()          <- Compile.nitpick canonical
      objects     <- Compile.optimize modul annotations canonical
      return (Compile.Artifacts canonical annotations objects)


-- For running compilation

data Flags =
  Flags
    { _mode :: DesiredMode
    , _output :: Output
    }

data Output = NoOutput | OutputTo OutputFormat

data OutputFormat = Html | Js

data DesiredMode = Debug | Dev | Prod

data CompilationResult
    = CompiledJs B.Builder
    | CompiledHtml B.Builder
    | CompiledSkippedOutput
    

getMode :: Bool -> Bool -> DesiredMode
getMode debug optimize =
  case (debug, optimize) of
    (True , True ) -> Debug
    (True , False) -> Debug
    (False, False) -> Dev
    (False, True ) -> Prod


generate :: FilePath -> Details.Details -> DesiredMode -> Build.Artifacts -> Output -> Task.Task Exit.Reactor CompilationResult
generate root details desiredMode artifacts output =
  case output of
    NoOutput -> pure CompiledSkippedOutput
    OutputTo format ->
      Task.mapError Exit.ReactorBadGenerate $ do
        js <- case desiredMode of
                Debug -> Generate.debug root details artifacts
                Dev   -> Generate.dev   root details artifacts
                Prod  -> Generate.prod  root details artifacts
        case format of
          Html -> 
              pure (CompiledHtml (Generate.Html.sandwich (Name.fromChars "Main") js))
          Js   -> pure (CompiledJs js)
    





{- Some other common functionaltiy

-}


checkRoot :: CompilationFlags -> Build.Env -> Build.ResultDict -> Build.RootStatus -> IO Build.RootResult
checkRoot flags env@(Build.Env _ root _ _ _ _ _) results rootStatus =
  case rootStatus of
    Build.SInside name ->
      return (Build.RInside name)

    Build.SOutsideErr err ->
      return (Build.ROutsideErr err)

    Build.SOutsideOk local@(Details.Local path time deps _ _ lastCompile) source modul@(Src.Module _ _ _ imports _ _ _ _ _) ->
      do  depsStatus <- Build.checkDeps root results deps lastCompile
          case depsStatus of
            Build.DepsChange ifaces ->
              compileOutside flags env local source ifaces modul

            Build.DepsSame same cached ->
              do  maybeLoaded <- Build.loadInterfaces root same cached
                  case maybeLoaded of
                    Nothing     -> return Build.ROutsideBlocked
                    Just ifaces -> compileOutside flags env local source ifaces modul

            Build.DepsBlock ->
              return Build.ROutsideBlocked

            Build.DepsNotFound problems ->
              return $ Build.ROutsideErr $ Error.Module (Src.getName modul) path time source $
                  Error.BadImports (Build.toImportErrors env results imports problems)


compileOutside :: CompilationFlags -> Build.Env -> Details.Local -> B.ByteString -> Map.Map ModuleName.Raw I.Interface -> Src.Module -> IO Build.RootResult
compileOutside flags (Build.Env key _ projectType _ _ _ _) (Details.Local path time _ _ _ _) source ifaces modul =
  let
    pkg = Build.projectTypeToPkg projectType
    name = Src.getName modul
  in
  case compile flags pkg ifaces modul of
    Right (Compile.Artifacts canonical annotations objects) -> do
      
      Reporting.report key Reporting.BDone
      return $ Build.ROutsideOk name (I.fromModule pkg canonical annotations) objects

    Left errors ->
      return $ Build.ROutsideErr $ Error.Module name path time source errors

