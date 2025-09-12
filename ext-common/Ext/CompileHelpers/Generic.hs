module Ext.CompileHelpers.Generic where

import qualified Data.ByteString.Builder as B
import qualified AST.Canonical as Can
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder
import qualified Data.ByteString.Lazy
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified AST.Optimized as Opt
import qualified AST.Source as Src
import qualified Reporting.Error.Type
import qualified Canonicalize.Module
import qualified Data.NonEmptyList
import qualified Data.Map as Map
import qualified Data.Name as Name
import qualified Data.OneOrMore as OneOrMore
import qualified Elm.Details as Details
import qualified Elm.Interface as I
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified Nitpick.PatternMatches

import qualified System.IO.Unsafe
import qualified Reporting.Render.Type.Localizer
import qualified Type.Constrain.Module
import qualified Type.Solve
import qualified Reporting.Task as Task
import qualified Reporting.Exit as Exit
import qualified Reporting.Error
import qualified Reporting.Result
import qualified Reporting.Warning
import qualified StandaloneInstances
import qualified Optimize.Module
import qualified Compile
import qualified Make
import qualified Generate
import qualified Reporting
import qualified Generate.Html
import qualified Build
import qualified Modify
import qualified Modify.Inject.Loader

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


{- COMPILATION!!!! -}

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
compile :: CompilationFlags -> Pkg.Name -> Map.Map ModuleName.Raw I.Interface -> Src.Module -> ([Reporting.Warning.Warning], Either Reporting.Error.Error Compile.Artifacts)
compile (CompilationFlags modifications) pkg ifaces modul =
  let
    (canonWarnings, canonResult) = canonicalize pkg ifaces modul
  in
  case canonResult of
    Left err -> (canonWarnings, Left err)
    Right dirtyCanonical ->
      let
        canonical = Modify.update modifications dirtyCanonical
      in
      case typeCheck modul canonical of
        Left err -> (canonWarnings, Left err)
        Right annotations ->
          case nitpick canonical of
            Left err -> (canonWarnings, Left err)
            Right () ->
              let
                (optWarnings, optResult) = optimize modul annotations canonical
              in
              case optResult of
                Left err -> (canonWarnings ++ optWarnings, Left err)
                Right objects -> (canonWarnings ++ optWarnings, Right (Compile.Artifacts canonical annotations objects))


-- PHASES.  These mirror what is in Compile.hs


canonicalize :: Pkg.Name -> Map.Map ModuleName.Raw I.Interface -> Src.Module -> ([Reporting.Warning.Warning], Either Reporting.Error.Error Can.Module)
canonicalize pkg ifaces modul =
  case Reporting.Result.run (Canonicalize.Module.canonicalize pkg ifaces modul) of
    (warnings, Right canonical) ->
      (warnings, Right canonical)

    (warnings, Left errors) ->
      (warnings, Left $ Reporting.Error.BadNames errors)


typeCheck :: Src.Module -> Can.Module -> Either Reporting.Error.Error (Map.Map Name.Name Can.Annotation)
typeCheck modul canonical =
  case System.IO.Unsafe.unsafePerformIO (Type.Solve.run =<< Type.Constrain.Module.constrain canonical) of
    Right annotations ->
      Right annotations

    Left errors ->
      Left (Reporting.Error.BadTypes (Reporting.Render.Type.Localizer.fromModule modul) errors)


nitpick :: Can.Module -> Either Reporting.Error.Error ()
nitpick canonical =
  case Nitpick.PatternMatches.check canonical of
    Right () ->
      Right ()

    Left errors ->
      Left (Reporting.Error.BadPatterns errors)


optimize :: Src.Module -> Map.Map Name.Name Can.Annotation -> Can.Module -> ([Reporting.Warning.Warning], Either Reporting.Error.Error Opt.LocalGraph)
optimize modul annotations canonical =
  case Reporting.Result.run (Optimize.Module.optimize annotations canonical) of
    (warnings, Right localGraph) ->
      (warnings, Right localGraph)

    (warnings, Left errors) ->
      (warnings, Left (Reporting.Error.BadMains (Reporting.Render.Type.Localizer.fromModule modul) errors))





-- For running compilation

data Flags =
  Flags
    { _mode :: DesiredMode
    , _output :: Output
    , _injectHotJs :: Maybe ElmDevWsUrl
    }
    deriving (Show, Eq)

data Output = NoOutput | OutputTo OutputFormat
  deriving (Show, Eq)

data OutputFormat = Html | Js
  deriving (Show, Eq)

data DesiredMode = Debug | Dev | Prod
  deriving (Show, Eq)

newtype ElmDevWsUrl = ElmDevWsUrl B.Builder

instance Show ElmDevWsUrl where
  show _ = "ElmDevWsUrl <builder>"

-- Equality is only used to detect whether flags changed.
-- We treat any two ElmDevWsUrl values as equal to avoid comparing builders.
instance Eq ElmDevWsUrl where
  _ == _ = True

data CompilationResult
    = CompiledJs B.Builder
    | CompiledHtml B.Builder
    | CompiledSkippedOutput
    deriving (Show)

getMode :: Bool -> Bool -> DesiredMode
getMode debug optimize =
  case (debug, optimize) of
    (True , True ) -> Debug
    (True , False) -> Debug
    (False, False) -> Dev
    (False, True ) -> Prod


generate :: FilePath -> Details.Details -> DesiredMode -> Build.Artifacts -> Output -> Maybe ElmDevWsUrl -> Task.Task Exit.Reactor CompilationResult
generate root details desiredMode artifacts output maybeWsUrl =
  case output of
    NoOutput -> pure CompiledSkippedOutput
    OutputTo format ->
      Task.mapError Exit.ReactorBadGenerate $ do
        let maybeInjectJs =
              case maybeWsUrl of
                Nothing -> Nothing
                Just (ElmDevWsUrl urlBuilder) ->
                  let urlText =
                        Data.Text.Encoding.decodeUtf8
                          (Data.ByteString.Lazy.toStrict (Data.ByteString.Builder.toLazyByteString urlBuilder))
                  in Just (Modify.Inject.Loader.hotJs (Data.Text.unpack urlText))
        js <- case desiredMode of
                Debug -> Generate.debug root details artifacts maybeInjectJs
                Dev   -> Generate.dev   root details artifacts maybeInjectJs
                Prod  -> Generate.prod  root details artifacts maybeInjectJs
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
              return $ Build.ROutsideErr $ Reporting.Error.Module (Src.getName modul) path time source $
                  Reporting.Error.BadImports (Build.toImportErrors env results imports problems)


compileOutside :: CompilationFlags -> Build.Env -> Details.Local -> B.ByteString -> Map.Map ModuleName.Raw I.Interface -> Src.Module -> IO Build.RootResult
compileOutside flags (Build.Env key _ projectType _ _ _ _) (Details.Local path time _ _ _ _) source ifaces modul =
  let
    pkg = Build.projectTypeToPkg projectType
    name = Src.getName modul
  in
  case compile flags pkg ifaces modul of
    (_warnings, Right (Compile.Artifacts canonical annotations objects)) -> do
      
      Reporting.report key Reporting.BDone
      return $ Build.ROutsideOk name (I.fromModule pkg canonical annotations) objects

    (_warnings, Left errors) ->
      return $ Build.ROutsideErr $ Reporting.Error.Module name path time source errors

