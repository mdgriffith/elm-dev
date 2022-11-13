{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ext.CompileProxy
 ( loadFileSource
 , loadSingleArtifacts
 , loadSingleArtifactsWithSource
 , loadCanonicalizeEnv
 , loadProject
 , parse
 , warnings
 , compileToJson
 )
 where

{- This is a proxy for all compilation related functions
   that ensures we can transparently swap compilation providers/methods
   (i.e. Disk vs MemoryCached)
 -}

import Control.Concurrent.MVar
import Ext.Common
import Ext.CompileMode (getMode, CompileMode(..))
import Json.Encode ((==>))
import qualified AST.Canonical as Can
import qualified AST.Optimized as Opt
import qualified AST.Source as Src
import qualified BackgroundWriter as BW
import qualified Build
import qualified Canonicalize.Module as Canonicalize
import qualified Compile
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import Data.Name (Name)
import qualified Data.NonEmptyList as NE
import qualified Data.Set as Set
import qualified Elm.Docs as Docs
import qualified Elm.Details as Details
import qualified Elm.Interface as I
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified Ext.CompileHelpers.Disk
import qualified Ext.CompileHelpers.Generic as CompileHelpers
import qualified Elm.ModuleName as ModuleName
import qualified Ext.CompileHelpers.Memory
import qualified Ext.FileProxy as File
import qualified Json.Encode as Encode
import qualified Optimize.Module as Optimize
import qualified Parse.Module as Parse
import qualified Reporting
import qualified Reporting.Annotation as A
import qualified Reporting.Error
import qualified Reporting.Error.Syntax
import qualified Reporting.Exit as Exit
import qualified Reporting.Result
import qualified Reporting.Task as Task
import qualified Reporting.Warning as Warning
import qualified Stuff
import qualified System.Directory as Dir
import System.IO.Unsafe (unsafePerformIO)

import StandaloneInstances
import qualified Canonicalize.Environment
import qualified Canonicalize.Environment.Foreign
import qualified Reporting.Error.Canonicalize
import Data.OneOrMore (OneOrMore(..))
import qualified Canonicalize.Environment.Local


type AggregateStatistics = Map.Map CompileMode Double


{-# NOINLINE aggregateStatistics #-}
aggregateStatistics :: MVar AggregateStatistics
aggregateStatistics = unsafePerformIO $
  let blank :: AggregateStatistics = Map.fromList [ (Disk, 0) , (Memory, 0) ]
  in
  newMVar blank


{-# NOINLINE addToAggregate #-}
addToAggregate mode t label = do
  Ext.Common.debug $ "📈 " ++ label ++ " +" ++ show t
  modifyMVar_ aggregateStatistics (\agg -> pure $ Map.update (\existing -> Just $ existing + t) mode agg )


aggregateSummary :: IO String
aggregateSummary = do
  x <- readMVar aggregateStatistics
  pure $ show x


modeRunner identifier ioDisk ioMemory = do
  -- Ext.Common.debug $ concat ["👁 compileProxy:", identifier ]
  case getMode of
    Disk -> do
      (t, label, result) <- Ext.Common.track_ ("🎻 classic   " ++ identifier) $ ioDisk
      addToAggregate Disk t label
      summary <- aggregateSummary
      Ext.Common.debug $ "📊 " <> summary
      File.debugSummary
      pure result
    Memory -> do
      (t, label, result) <- Ext.Common.track_ ("🧠 memcached " ++ identifier) $ ioMemory
      addToAggregate Memory t label
      summary <- aggregateSummary
      Ext.Common.debug $ "📊 " <> summary
      File.debugSummary
      pure result
    Race -> do
      results <- Ext.Common.race identifier [ ("🧠 memcached " ++ identifier, ioMemory) , ("🎻 classic   " ++ identifier, ioDisk) ]
      results & zip [Memory, Disk] & mapM_ (\(m, (t, l, r)) -> addToAggregate m t (l ++ " " ++ identifier))
      summary <- aggregateSummary
      Ext.Common.debug $ "📊 " <> summary
      File.debugSummary
      (results !! 1) & (\(_,_,x) -> x) & pure


-- Interfaces


compileToJson :: FilePath -> NE.List FilePath -> IO (Either Encode.Value Encode.Value)
compileToJson root paths = do
  res <- modeRunner "compileToJson"
    (Ext.CompileHelpers.Disk.compileToJson root paths)
    (Ext.CompileHelpers.Memory.compileToJson root paths)
  pure res


allDepArtifacts :: IO CompileHelpers.Artifacts
allDepArtifacts =
  modeRunner "allDepArtifacts"
    (Ext.CompileHelpers.Disk.allDepArtifacts)
    (Ext.CompileHelpers.Memory.allDepArtifacts)


allInterfaces :: [FilePath] -> IO (Map.Map ModuleName.Raw I.Interface)
allInterfaces paths = do
  artifactsDeps <- allDepArtifacts
  ifacesProject <-
    case paths of
      -- @TODO change to NE.List
      [] -> error "Ext.CompileProxy.allInterfaces must have at least one path specified!"
      path:paths -> allProjectInterfaces (NE.List path paths)

  pure $ Map.union ifacesProject (CompileHelpers._ifaces artifactsDeps)


loadSingleArtifacts :: FilePath -> FilePath -> IO (Either Reporting.Error.Error Compile.Artifacts)
loadSingleArtifacts root path =
  Dir.withCurrentDirectory root $ do
    ifaces <- allInterfaces [path]
    source <- File.readUtf8 path
    case Parse.fromByteString Parse.Application source of
      Right modul ->
        pure $ Compile.compile Pkg.dummyName ifaces modul

      Left err ->
        pure $ Left $ Reporting.Error.BadSyntax err


allProjectInterfaces :: NE.List FilePath -> IO (Map.Map ModuleName.Raw I.Interface)
allProjectInterfaces paths =
  BW.withScope $ \scope -> do
    root <- getProjectRoot
    runTaskUnsafe $
      do  details    <- Task.eio Exit.ReactorBadDetails $ Details.load Reporting.silent scope root
          artifacts  <- Task.eio Exit.ReactorBadBuild $ Build.fromPaths Reporting.silent root details paths

          Task.io $ extractInterfaces $ Build._modules artifacts


loadFileSource :: FilePath -> FilePath -> IO (Either Reporting.Error.Syntax.Error (BS.ByteString, Src.Module))
loadFileSource root path = do
  Dir.withCurrentDirectory root $ do
    source <- File.readUtf8 path
    case Parse.fromByteString Parse.Application source of
      Right modul -> do
        -- hindentPrintValue "module source" modul
        pure $ Right (source, modul)

      Left err ->
        pure $ Left err

loadSingleArtifactsWithSource :: FilePath -> FilePath -> Src.Module -> IO (Either Reporting.Error.Error Compile.Artifacts)
loadSingleArtifactsWithSource root path srcMod =
  Dir.withCurrentDirectory root $ do
    ifaces <- allInterfaces [path]
    pure $ Compile.compile Pkg.dummyName ifaces srcMod


loadCanonicalizeEnv ::
  FilePath ->
  FilePath ->
  Src.Module ->
  IO (Either (OneOrMore Reporting.Error.Canonicalize.Error) Canonicalize.Environment.Env)
loadCanonicalizeEnv root path srcMod = do
  Dir.withCurrentDirectory root $ do
    ifaces <- allInterfaces [path]

    let home = ModuleName.Canonical Pkg.dummyName $ Src.getName srcMod

    let (_, eitherResult) = Reporting.Result.run $
          Canonicalize.Environment.Local.add srcMod =<<
            Canonicalize.Environment.Foreign.createInitialEnv home ifaces (Src._imports srcMod)

    case eitherResult of
      Left err ->
        pure $ Left err
      Right (env, _, _) ->
        pure $ Right env

{- Appropriated from worker/src/Artifacts.hs
   WARNING: does not load any user code!!!

   We generally do this when we want a mapping of modulenames to filepaths
-}
loadProject :: IO Details.Details
loadProject =
  BW.withScope $ \scope ->
  do  --debug "Loading allDeps"
      let style = Reporting.silent
      root <- getProjectRoot
      result <- Details.load style scope root
      case result of
        Left _ ->
          error $ "Ran into some problem loading elm.json\nTry running `elm make` in: " ++ root

        Right details ->
          return details


parse :: FilePath -> FilePath -> IO (Either Reporting.Error.Syntax.Error Src.Module)
parse root path =
  Dir.withCurrentDirectory root $ do
    source <- File.readUtf8 path
    return $ Parse.fromByteString Parse.Application source


-- @TODO this is a disk mode function
docs :: FilePath -> NE.List ModuleName.Raw -> IO Docs.Documentation
docs root exposed =
    BW.withScope $ \scope ->
        runTaskUnsafeMake $
          do
            details  <- Task.eio Exit.MakeBadDetails $ Details.load Reporting.silent scope root
            Task.eio Exit.MakeCannotBuild $ Build.fromExposed Reporting.silent root details Build.KeepDocs exposed



-- @TODO this is a disk mode function
warnings :: FilePath -> FilePath -> IO (Either () (Src.Module, [ Warning.Warning ]))
warnings root path =
  Dir.withCurrentDirectory root $ do
    ifaces <- allInterfaces [path]
    source <- File.readUtf8 path
    case Parse.fromByteString Parse.Application source of
      Right srcModule@(Src.Module _ _ _ imports _ _ _ _ _) ->
        do
          let (canWarnings, eitherCanned) = Reporting.Result.run $ Canonicalize.canonicalize Pkg.dummyName ifaces srcModule
          case eitherCanned of
            Left errs ->
              pure (Right (srcModule, canWarnings))

            Right canModule ->
                case CompileHelpers.typeCheck srcModule canModule of
                  Left typeErrors ->
                      pure (Right (srcModule, canWarnings))

                  Right annotations ->
                    do
                      let (optWarnings, eitherLocalGraph) = Reporting.Result.run $ Optimize.optimize annotations canModule
                      case eitherLocalGraph of
                        Left errs ->
                          pure (Right (srcModule, canWarnings <> optWarnings))

                        Right localGraph -> do
                          let filteredImports = filterOutDefaultImports imports
                          let importNames = Set.fromList $ fmap Src.getImportName filteredImports
                          let usedModules = collectUsedImports localGraph
                          let unusedImports = Set.difference importNames usedModules
                          let unusedImportWarnings = importsToWarnings (Set.toList unusedImports) filteredImports

                          pure (Right (srcModule, canWarnings <> optWarnings <> unusedImportWarnings))

      Left err ->
        pure (Left ())



-- Helpers


-- By default every Elm module has these modules imported with these region pairings.
-- If they add a manual import of, e.g. `import Maybe`, then we'll get the same name
-- but with a non-zero based region
filterOutDefaultImports :: [Src.Import] -> [Src.Import]
filterOutDefaultImports imports =
    filter
      (\(Src.Import (A.At region name) _ _) ->
        not $ any (\defaultImport -> defaultImport == (name,region)) defaultImports
      )
      imports


defaultImports :: [(Name, A.Region)]
defaultImports =
  [ ("Platform.Sub", A.Region (A.Position 0 0) (A.Position 0 0))
  , ("Platform.Cmd", A.Region (A.Position 0 0) (A.Position 0 0))
  , ("Platform", A.Region (A.Position 0 0) (A.Position 0 0))
  , ("Tuple", A.Region (A.Position 0 0) (A.Position 0 0))
  , ("Char", A.Region (A.Position 0 0) (A.Position 0 0))
  , ("String", A.Region (A.Position 0 0) (A.Position 0 0))
  , ("Result", A.Region (A.Position 0 0) (A.Position 0 0))
  , ("Maybe", A.Region (A.Position 0 0) (A.Position 0 0))
  , ("List", A.Region (A.Position 0 0) (A.Position 0 0))
  , ("Debug", A.Region (A.Position 0 0) (A.Position 0 0))
  , ("Basics", A.Region (A.Position 0 0) (A.Position 0 0))
  ]



importsToWarnings :: [Name] -> [Src.Import] -> [Warning.Warning]
importsToWarnings unusedNames imports =
  importsToWarningsHelper unusedNames imports []


importsToWarningsHelper :: [Name] -> [Src.Import] -> [Warning.Warning] -> [Warning.Warning]
importsToWarningsHelper unusedNames imports warnings =
  case imports of
    [] -> warnings
    (Src.Import (A.At region name) _ _) : remainingImports ->
      if any (\unusedName -> unusedName == name) unusedNames
        then importsToWarningsHelper unusedNames remainingImports (Warning.UnusedImport region name : warnings)
        else importsToWarningsHelper unusedNames remainingImports warnings


collectUsedImports :: Opt.LocalGraph -> Set.Set Name
collectUsedImports ocalGraph@(Opt.LocalGraph _ nodes fields) =
  Set.fromList $ findModules (Map.elems nodes) []


findModules :: [Opt.Node] -> [Name] -> [Name]
findModules nodes names =
  case nodes of
    [] -> names

    Opt.Define expr globals : remainingNodes ->
      findModules remainingNodes (findModulesInExpr expr <> setGloablToNames globals <> names)

    Opt.DefineTailFunc _ expr globals : remainingNodes ->
      findModules remainingNodes (findModulesInExpr expr <> setGloablToNames globals <> names)

    Opt.Link (Opt.Global (ModuleName.Canonical _ name) _) : remainingNodes ->
      findModules remainingNodes (name : names)

    Opt.Cycle _ pairsNameExpr defs globals : remainingNodes ->
      findModules remainingNodes
        (concatMap (\(_, expr) -> findModulesInExpr expr) pairsNameExpr
          <> concatMap
              (\def ->
                case def of
                  Opt.Def _ defExpr ->
                    findModulesInExpr defExpr

                  Opt.TailDef _ _ defExpr ->
                    findModulesInExpr defExpr
              )
              defs
          <> setGloablToNames globals
          <> names
        )

    Opt.PortIncoming expr globals : remainingNodes ->
      findModules remainingNodes (findModulesInExpr expr <> setGloablToNames globals <> names)

    Opt.PortOutgoing expr globals : remainingNodes ->
      findModules remainingNodes (findModulesInExpr expr <> setGloablToNames globals <> names)

    _ : remainingNodes ->
      findModules remainingNodes names

  where
    setGloablToNames globals = fmap (\(Opt.Global (ModuleName.Canonical _ name) _) -> name) $ Set.toList globals


findModulesInExpr :: Opt.Expr -> [Name]
findModulesInExpr expr =
  case expr of
    -- These have expressions which may have global names aka module names
    Opt.Function _ expr_ ->
      findModulesInExpr expr_

    Opt.Call leftExpr rightExprs ->
      findModulesInExpr leftExpr <>  concatMap findModulesInExpr rightExprs

    Opt.List exprs ->
      concatMap findModulesInExpr exprs

    Opt.TailCall _ pairsNameEpxr ->
      concatMap (\(_,expr_) ->findModulesInExpr expr_) pairsNameEpxr

    Opt.If pairsExprExpr expr_ ->
      concatMap (\(leftExpr,rightExpr) -> findModulesInExpr leftExpr <> findModulesInExpr rightExpr) pairsExprExpr
        <> findModulesInExpr expr_

    Opt.Let (Opt.Def _ defExpr) expr_ ->
      findModulesInExpr defExpr <> findModulesInExpr expr_

    Opt.Let (Opt.TailDef _ _ defExpr) expr_ ->
      findModulesInExpr defExpr <> findModulesInExpr expr_

    Opt.Destruct _ expr_ ->
      findModulesInExpr expr_

    Opt.Case _ _ decider pairsIntExpr ->
      (concatMap findModulesInExpr $ deciderToExprs decider)
        <> concatMap (\(_, expr_) -> findModulesInExpr expr_) pairsIntExpr

    Opt.Access expr_ _ ->
      findModulesInExpr expr_

    Opt.Update expr_ mapNameExprs ->
      findModulesInExpr expr_ <> (concatMap findModulesInExpr $ Map.elems mapNameExprs)

    Opt.Record mapNameExprs ->
      concatMap findModulesInExpr $ Map.elems mapNameExprs

    Opt.Tuple expr1 expr2 Nothing ->
      findModulesInExpr expr1 <> findModulesInExpr expr2

    Opt.Tuple expr1 expr2 (Just expr3) ->
      findModulesInExpr expr1 <> findModulesInExpr expr2 <> findModulesInExpr expr3

    -- These are the only expressions with module names
    Opt.VarGlobal (Opt.Global (ModuleName.Canonical _ name) _) -> [name]
    Opt.VarEnum (Opt.Global (ModuleName.Canonical _ name) _) _ -> [name]
    Opt.VarBox (Opt.Global (ModuleName.Canonical _ name) _) -> [name]

    -- Everything else doesn't have child expressions or a module name
    _ -> []


deciderToExprs :: Opt.Decider Opt.Choice -> [Opt.Expr]
deciderToExprs decider =
  case decider of
    Opt.Leaf (Opt.Inline expr) ->
      [expr]

    Opt.Leaf (Opt.Jump _) ->
      []

    Opt.Chain _ success failure ->
      deciderToExprs success <> deciderToExprs failure

    Opt.FanOut _ tests fallback ->
      concatMap (\(_,decider_) -> deciderToExprs decider_) tests <> deciderToExprs fallback


runTaskUnsafe :: Task.Task Exit.Reactor a -> IO a
runTaskUnsafe task = do
  result <- Task.run task
  case result of
    Right a ->
      return a

    Left exit ->
      do  Exit.toStderr (Exit.reactorToReport exit)
          error
            "\n-------------------------------------------------\
            \\nError in unsafe task, please report this.\
            \\n-------------------------------------------------\
            \\n"

runTaskUnsafeMake :: Task.Task Exit.Make a -> IO a
runTaskUnsafeMake task = do
  result <- Task.run task
  case result of
    Right a ->
      return a

    Left exit ->
      do  Exit.toStderr (Exit.makeToReport exit)
          error
            "\n-------------------------------------------------\
            \\nError in make task, please report this.\
            \\n-------------------------------------------------\
            \\n"

extractInterfaces :: [Build.Module] -> IO (Map.Map ModuleName.Raw I.Interface)
extractInterfaces modu = do
  k <- modu
    & mapM (\m ->
      case m of
        Build.Fresh nameRaw ifaces _ ->
          pure $ Just (nameRaw, ifaces)
        Build.Cached name _ mCachedInterface ->
          cachedHelp name mCachedInterface
    )
  pure $ Map.fromList $ justs k


{- Appropriated from Build.loadInterface -}
cachedHelp :: ModuleName.Raw -> MVar Build.CachedInterface -> IO (Maybe (ModuleName.Raw, I.Interface))
cachedHelp name ciMvar = do
  cachedInterface <- takeMVar ciMvar
  case cachedInterface of
    Build.Corrupted ->
      do  putMVar ciMvar cachedInterface
          return Nothing

    Build.Loaded iface ->
      do  putMVar ciMvar cachedInterface
          return (Just (name, iface))

    Build.Unneeded ->
      do  root <- getProjectRoot
          maybeIface <- File.readBinary (Stuff.elmi root name)
          case maybeIface of
            Nothing ->
              do  putMVar ciMvar Build.Corrupted
                  return Nothing

            Just iface ->
              do  putMVar ciMvar (Build.Loaded iface)
                  return (Just (name, iface))