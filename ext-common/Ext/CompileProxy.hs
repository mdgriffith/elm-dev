{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ext.CompileProxy
  ( loadSingle,
    SingleFileResult (..),
    loadFileSource,
    loadCanonicalizeEnv,
    loadProject,
    allPackageArtifacts,
    parse,
    compileToJson,
    compileToDocs,
    loadAndEnsureCompiled,
    ensureModulesAreCompiled,
    compilationErrorToJson,
    CompilationError (..),
  )
where

{- This is a proxy for all compilation related functions
   that ensures we can transparently swap compilation providers/methods
   (i.e. Disk vs MemoryCached)
 -}

import qualified AST.Canonical as Can
import qualified AST.Optimized as Opt
import qualified AST.Source as Src
import qualified BackgroundWriter
import qualified BackgroundWriter as BW
import qualified Build
import qualified Canonicalize.Environment
import qualified Canonicalize.Environment.Foreign
import qualified Canonicalize.Environment.Local
import qualified Canonicalize.Module as Canonicalize
import qualified Compile
import Control.Concurrent.MVar
import qualified Control.Monad (filterM, foldM_, mapM)
import qualified Data.ByteString as BS
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Name as Name (Name, fromChars, toChars)
import qualified Data.NonEmptyList as NE
import Data.OneOrMore (OneOrMore (..))
import qualified Data.Set as Set
import qualified Elm.Details as Details
import qualified Elm.Docs as Docs
import qualified Elm.Interface as I
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Outline
import qualified Elm.Package as Pkg
import Ext.Common
import qualified Ext.CompileHelpers.Disk
import qualified Ext.CompileHelpers.Generic as CompileHelpers
import qualified Ext.CompileHelpers.Memory
import Ext.CompileMode (CompileMode (..), getMode)
import qualified Ext.FileProxy as File
import qualified Ext.Log
import qualified Ext.Project.Find
import Json.Encode ((==>))
import qualified Json.Encode as Encode
import qualified Nitpick.PatternMatches
import qualified Optimize.Module as Optimize
import qualified Parse.Module as Parse
import qualified Reporting
import qualified Reporting.Annotation as A
import qualified Reporting.Error
import qualified Reporting.Error.Canonicalize
import qualified Reporting.Error.Syntax
import qualified Reporting.Exit as Exit
import qualified Reporting.Render.Type.Localizer as Localizer
import qualified Reporting.Result
import qualified Reporting.Task as Task
import qualified Reporting.Warning as Warning
import StandaloneInstances
import qualified Stuff
import qualified System.Directory as Dir
import qualified System.FilePath as Path
import qualified System.IO
import System.IO.Unsafe (unsafePerformIO)

type AggregateStatistics = Map.Map CompileMode Double

{-# NOINLINE aggregateStatistics #-}
aggregateStatistics :: MVar AggregateStatistics
aggregateStatistics =
  unsafePerformIO $
    let blank :: AggregateStatistics = Map.fromList [(Disk, 0), (Memory, 0)]
     in newMVar blank

{-# NOINLINE addToAggregate #-}
addToAggregate mode t label = do
  Ext.Log.log Ext.Log.Performance $ label ++ " +" ++ show t
  modifyMVar_ aggregateStatistics (\agg -> pure $ Map.update (\existing -> Just $ existing + t) mode agg)

aggregateSummary :: IO String
aggregateSummary = do
  x <- readMVar aggregateStatistics
  pure $ show x

modeRunner identifier ioDisk ioMemory = do
  -- Ext.Log.log Ext.Log.Performance $ concat ["👁 compileProxy:", identifier ]
  case getMode of
    Disk -> do
      (t, label, result) <- Ext.Common.track_ ("🎻 classic   " ++ identifier) $ ioDisk
      addToAggregate Disk t label
      summary <- aggregateSummary
      Ext.Log.log Ext.Log.Performance $ summary
      File.debugSummary
      pure result
    Memory -> do
      (t, label, result) <- Ext.Common.track_ ("🧠 memcached " ++ identifier) $ ioMemory
      addToAggregate Memory t label
      summary <- aggregateSummary
      Ext.Log.log Ext.Log.Performance $ summary
      File.debugSummary
      pure result
    Race -> do
      results <- Ext.Common.race identifier [("🧠 memcached " ++ identifier, ioMemory), ("🎻 classic   " ++ identifier, ioDisk)]
      results & zip [Memory, Disk] & mapM_ (\(m, (t, l, r)) -> addToAggregate m t (l ++ " " ++ identifier))
      summary <- aggregateSummary
      Ext.Log.log Ext.Log.Performance $ summary
      File.debugSummary
      (results !! 1) & (\(_, _, x) -> x) & pure

loadAndEnsureCompiled :: FilePath -> Maybe (NE.List ModuleName.Raw) -> IO (Either CompilationError Details.Details)
loadAndEnsureCompiled root exposed = do
  result <- ensureModulesAreCompiled root exposed
  case result of
    Left err ->
      pure (Left err)
    Right details ->
      pure (Right details)

-- Interfaces

-- | Needs memory mode!
compileToDocs :: FilePath -> NE.List ModuleName.Raw -> Details.Details -> IO (Either Exit.Reactor Docs.Documentation)
compileToDocs root modules details =
  Ext.CompileHelpers.Disk.compileToDocsCached root modules details

compileToJson :: FilePath -> NE.List FilePath -> IO (Either Encode.Value Encode.Value)
compileToJson root paths = do
  modeRunner
    "compileToJson"
    (Ext.CompileHelpers.Disk.compileToJson root paths)
    (Ext.CompileHelpers.Memory.compileToJson root paths)

allPackageArtifacts :: FilePath -> IO CompileHelpers.Artifacts
allPackageArtifacts root =
  modeRunner
    "allPackageArtifacts"
    (Ext.CompileHelpers.Disk.allPackageArtifacts root)
    (Ext.CompileHelpers.Memory.allPackageArtifacts root)

allInterfaces :: FilePath -> NE.List FilePath -> IO (Either Exit.Reactor (Map.Map ModuleName.Raw I.Interface))
allInterfaces root paths =
  Dir.withCurrentDirectory root $
    BW.withScope $ \scope -> Stuff.withRootLock root $
      Task.run $
        do
          details <- Task.eio Exit.ReactorBadDetails $ Details.load Reporting.silent scope root
          artifacts <- Task.eio Exit.ReactorBadBuild $ Build.fromPaths Reporting.silent root details paths

          Task.io $ extractInterfaces root $ Build._modules artifacts

loadFileSource :: FilePath -> FilePath -> IO (Either Reporting.Error.Syntax.Error (BS.ByteString, Src.Module))
loadFileSource root path = do
  Dir.withCurrentDirectory root $ do
    source <- File.readUtf8 path
    case Parse.fromByteString Parse.Application source of
      Right modul -> do
        pure $ Right (source, modul)
      Left err ->
        pure $ Left err

toMaybe :: Either x a -> Maybe a
toMaybe either =
  case either of
    Right a -> Just a
    Left _ -> Nothing

loadCanonicalizeEnv ::
  FilePath ->
  FilePath ->
  Src.Module ->
  IO (Maybe Canonicalize.Environment.Env)
loadCanonicalizeEnv root path srcMod = do
  Dir.withCurrentDirectory root $ do
    ifacesResult <- allInterfaces root (NE.List path [])
    (CompileHelpers.Artifacts packageIfaces globalGraph) <- allPackageArtifacts root
    case ifacesResult of
      Left _ ->
        pure Nothing
      Right localIfaces -> do
        let ifaces = Map.union localIfaces packageIfaces
        let home = ModuleName.Canonical Pkg.dummyName $ Src.getName srcMod

        let (_, eitherResult) =
              Reporting.Result.run $
                Canonicalize.Environment.Local.add srcMod
                  =<< Canonicalize.Environment.Foreign.createInitialEnv home ifaces (Src._imports srcMod)

        case eitherResult of
          Left err ->
            pure $ Nothing
          Right (env, _, _) ->
            pure $ Just env

{- Appropriated from worker/src/Artifacts.hs
   WARNING: does not load any user code!!!

   We generally do this when we want a mapping of modulenames to filepaths
-}
loadProject :: FilePath -> IO Details.Details
loadProject root =
  BW.withScope $ \scope ->
    do
      let style = Reporting.silent
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

data SingleFileResult = Single
  { _source :: Either Reporting.Error.Syntax.Error Src.Module,
    _warnings :: Maybe [Warning.Warning],
    _interfaces :: Maybe (Map.Map ModuleName.Raw I.Interface),
    _canonical :: Maybe Can.Module,
    _compiled :: Maybe (Either Reporting.Error.Error Compile.Artifacts)
  }

{-
The below function also modifies the canonical AST by hydrating missing types.

-}
-- @TODO this is a disk mode function
loadSingle :: FilePath -> FilePath -> IO SingleFileResult
loadSingle root path =
  Dir.withCurrentDirectory root $ do
    source <- File.readUtf8 path
    case Parse.fromByteString Parse.Application source of
      Right srcModule ->
        do
          ifacesResult <- allInterfaces root (NE.List path [])
          (CompileHelpers.Artifacts packageIfaces globalGraph) <- allPackageArtifacts root
          case ifacesResult of
            Left exit ->
              -- report exit : Exit.Reactor?
              pure
                ( Single
                    (Right srcModule)
                    Nothing
                    Nothing
                    Nothing
                    Nothing
                )
            Right localIfaces -> do
              let ifaces = Map.union localIfaces packageIfaces
              let (canWarnings, eitherCanned) = Reporting.Result.run $ Canonicalize.canonicalize Pkg.dummyName ifaces srcModule
              case eitherCanned of
                Left errs ->
                  pure
                    ( Single
                        (Right srcModule)
                        (Just canWarnings)
                        (Just ifaces)
                        Nothing
                        (Just (Left (Reporting.Error.BadNames errs)))
                    )
                Right initialCanModule ->
                  let canModule = addMissingTypes canWarnings initialCanModule
                   in case CompileHelpers.typeCheck srcModule canModule of
                        Left typeErrors ->
                          pure
                            ( Single
                                (Right srcModule)
                                (Just canWarnings)
                                (Just ifaces)
                                (Just canModule)
                                (Just (Left (Reporting.Error.BadTypes (Localizer.fromModule srcModule) typeErrors)))
                            )
                        Right annotations ->
                          do
                            let nitpicks = Nitpick.PatternMatches.check canModule

                            let (optWarnings, eitherLocalGraph) = Reporting.Result.run $ Optimize.optimize annotations canModule
                            case eitherLocalGraph of
                              Left errs ->
                                pure
                                  ( Single
                                      (Right srcModule)
                                      (Just (canWarnings <> optWarnings))
                                      (Just ifaces)
                                      (Just canModule)
                                      (Just (Left (Reporting.Error.BadMains (Localizer.fromModule srcModule) errs)))
                                  )
                              Right localGraph -> do
                                pure
                                  ( Single
                                      (Right srcModule)
                                      (Just (canWarnings <> optWarnings))
                                      (Just ifaces)
                                      (Just canModule)
                                      ( Just
                                          ( case nitpicks of
                                              Right () ->
                                                Right (Compile.Artifacts canModule annotations localGraph)
                                              Left errors ->
                                                Left (Reporting.Error.BadPatterns errors)
                                          )
                                      )
                                  )
      Left err ->
        pure
          ( Single
              (Left err)
              Nothing
              Nothing
              Nothing
              Nothing
          )

-- Helpers

extractInterfaces :: FilePath -> [Build.Module] -> IO (Map.Map ModuleName.Raw I.Interface)
extractInterfaces root modu = do
  k <-
    modu
      & mapM
        ( \m ->
            case m of
              Build.Fresh nameRaw ifaces _ ->
                pure $ Just (nameRaw, ifaces)
              Build.Cached name _ mCachedInterface ->
                cachedHelp root name mCachedInterface
        )
  pure $ Map.fromList $ justs k

{- Appropriated from Build.loadInterface -}
cachedHelp :: FilePath -> ModuleName.Raw -> MVar Build.CachedInterface -> IO (Maybe (ModuleName.Raw, I.Interface))
cachedHelp root name ciMvar = do
  cachedInterface <- takeMVar ciMvar
  case cachedInterface of
    Build.Corrupted ->
      do
        putMVar ciMvar cachedInterface
        return Nothing
    Build.Loaded iface ->
      do
        putMVar ciMvar cachedInterface
        return (Just (name, iface))
    Build.Unneeded ->
      do
        maybeIface <- File.readBinary (Stuff.elmi root name)
        case maybeIface of
          Nothing ->
            do
              putMVar ciMvar Build.Corrupted
              return Nothing
          Just iface ->
            do
              putMVar ciMvar (Build.Loaded iface)
              return (Just (name, iface))

addMissingTypes :: [Warning.Warning] -> Can.Module -> Can.Module
addMissingTypes warnings canModul =
  let lookup :: Map.Map Name.Name Can.Type
      lookup =
        warnings
          & fmap
            ( \warning ->
                case warning of
                  Warning.MissingTypeAnnotation region name annotation ->
                    Just (name, annotation)
                  _ ->
                    Nothing
            )
          & Maybe.catMaybes
          & Map.fromList
   in canModul {Can._decls = addMissingTypeToDecl lookup (Can._decls canModul)}

addMissingTypeToDecl :: Map.Map Name.Name Can.Type -> Can.Decls -> Can.Decls
addMissingTypeToDecl lookup decl =
  case decl of
    Can.Declare def moarDecls ->
      Can.Declare (addMissingTypeToDef lookup def) (addMissingTypeToDecl lookup moarDecls)
    Can.DeclareRec def defs moarDecls ->
      Can.DeclareRec (addMissingTypeToDef lookup def) (fmap (addMissingTypeToDef lookup) defs) (addMissingTypeToDecl lookup moarDecls)
    Can.SaveTheEnvironment ->
      Can.SaveTheEnvironment

addMissingTypeToDef :: Map.Map Name.Name Can.Type -> Can.Def -> Can.Def
addMissingTypeToDef typeLookup def =
  case def of
    Can.Def locatedName patterns expr ->
      let maybeType = Map.lookup (A.toValue locatedName) typeLookup
       in case maybeType of
            Nothing -> def
            Just tipe ->
              Can.TypedDef locatedName Map.empty (fmap (\patt -> (patt, Can.TUnit)) patterns) expr tipe
    _ ->
      def

data CompilationError
  = DetailsFailedToLoad Exit.Details
  | BuildProblem Exit.BuildProblem
  | ModuleProblem Ext.Project.Find.ModuleListError

compilationErrorToJson :: CompilationError -> Encode.Value
compilationErrorToJson err =
  case err of
    DetailsFailedToLoad details ->
      Exit.toJson (Exit.toDetailsReport details)
    BuildProblem buildProblem ->
      Exit.toJson (Exit.toBuildProblemReport buildProblem)
    ModuleProblem (Ext.Project.Find.OutlineError outlineProblem) ->
      Exit.toJson (Exit.toOutlineReport outlineProblem)
    ModuleProblem Ext.Project.Find.NoModulesFoundForApp ->
      Encode.object ["error" ==> Encode.chars "No .elm files found!"]
    ModuleProblem Ext.Project.Find.NoModulesFoundForPackage ->
      Encode.object ["error" ==> Encode.chars "No modules are exposed for this package."]

ensureModulesAreCompiled :: FilePath -> Maybe (NE.List ModuleName.Raw) -> IO (Either CompilationError Details.Details)
ensureModulesAreCompiled root manuallySpecifiedEntrypoints =
  BackgroundWriter.withScope $ \scope ->
    Stuff.withRootLock root $
      do
        exposedElmModuleResult <- case manuallySpecifiedEntrypoints of
          Nothing -> Ext.Project.Find.getAllElmModules root
          Just exposed ->
            pure (Right exposed)

        case exposedElmModuleResult of
          Left err ->
            pure (Left (ModuleProblem err))
          Right elmModules -> do
            detailsResult <- Details.load Reporting.json scope root
            case detailsResult of
              Left err ->
                pure (Left (DetailsFailedToLoad err))
              Right originalDetails -> do
                result <- Build.fromExposed Reporting.json root originalDetails Build.IgnoreDocs elmModules
                case result of
                  Left err ->
                    pure (Left (BuildProblem err))
                  Right _ -> do
                    newDetailsResult <- Details.load Reporting.json scope root

                    case newDetailsResult of
                      Left err ->
                        pure (Left (DetailsFailedToLoad err))
                      Right details ->
                        pure (Right details)

{- Get exposed modules or just a list of every module -}

-- -- Convert a file path to an Elm module name based on a root directory
-- toElmModuleName :: FilePath -> FilePath -> ModuleName.Raw
-- toElmModuleName root file =
--     let relativePath = Path.makeRelative root file
--         withoutExtension = Path.dropExtension relativePath
--     in
--     Name.fromChars (replaceDirectorySeparatorWithDot withoutExtension)

-- -- Convert a file path to an Elm module name based on a root directory
-- replaceDirectorySeparatorWithDot :: FilePath -> String
-- replaceDirectorySeparatorWithDot =
--     let
--         replaceSeparator c = if c == Path.pathSeparator then '.' else c
--     in
--     map replaceSeparator

-- data ModuleListError
--     = OutlineError Exit.Outline
--     | NoModulesFoundForApp
--     | NoModulesFoundForPackage

-- getAllElmModules :: FilePath -> IO (Either ModuleListError (NE.List ModuleName.Raw))
-- getAllElmModules root =
--   let
--      toNonEmpty onFail list =
--         case list of
--           [] -> Left onFail
--           (top:remain) -> Right (NE.List top remain)
--   in do
--   elmJsonResult <- Elm.Outline.read root
--   case elmJsonResult of
--       Left outlineExit ->
--         pure (Left (OutlineError outlineExit))

--       Right (Elm.Outline.App appOutline) ->
--         let
--             srcDirs = fmap (Elm.Outline.toAbsolute root) (Elm.Outline._app_source_dirs appOutline)

--             findModuleNames srcDir =
--                 fmap
--                   (fmap (toElmModuleName srcDir))
--                   (findAllElmFiles srcDir)

--         in do
--         allNestedFiles <- Control.Monad.mapM findModuleNames srcDirs
--         pure (toNonEmpty NoModulesFoundForApp (List.concat allNestedFiles))

--       Right (Elm.Outline.Pkg pkgOutline) ->
--         let
--            exposed = Elm.Outline._pkg_exposed pkgOutline
--         in
--         case exposed of
--           Elm.Outline.ExposedList exposedList ->
--             pure (toNonEmpty NoModulesFoundForPackage exposedList)

--           Elm.Outline.ExposedDict keyValueList ->
--             let
--                exposedList = List.concatMap snd keyValueList
--             in
--             pure (toNonEmpty NoModulesFoundForPackage exposedList)

-- findAllElmFiles :: FilePath -> IO [FilePath]
-- findAllElmFiles dir = do
--     contents <- Dir.getDirectoryContents dir
--     let paths = map (dir `Path.combine`) $ filter (`notElem` [".", ".."]) contents
--     files <- Control.Monad.filterM Dir.doesFileExist paths
--     dirs <- Control.Monad.filterM Dir.doesDirectoryExist paths
--     let elmFiles = filter (\f -> Path.takeExtension f == ".elm") files
--     elmFilesInDirs <- fmap concat $ mapM findAllElmFiles dirs
--     return $ elmFiles ++ elmFilesInDirs

-- findPorts :: FilePath -> IO (Either Exit.Outline [FilePath])
-- findPorts root = do
--   elmJsonResult <- Elm.Outline.read root
--   case elmJsonResult of
--       Left outlineExit ->
--         pure (Left outlineExit)

--       Right (Elm.Outline.App appOutline) ->
--           let
--             srcDirs = fmap (Elm.Outline.toAbsolute root) (Elm.Outline._app_source_dirs appOutline)
--         in do
--         allNestedFiles <- Control.Monad.mapM findPortFilePaths srcDirs
--         pure (Right (List.concat allNestedFiles))

--       Right (Elm.Outline.Pkg pkgOutline) ->
--         pure (Right [])

-- findPortFilePaths :: FilePath -> IO [FilePath]
-- findPortFilePaths dir = do
--     contents <- Dir.getDirectoryContents dir
--     let paths = map (dir `Path.combine`) $ filter (`notElem` [".", ".."]) contents
--     files <- Control.Monad.filterM Dir.doesFileExist paths
--     dirs <- Control.Monad.filterM Dir.doesDirectoryExist paths
--     elmFiles <- Control.Monad.filterM
--                     (\f ->
--                         if Path.takeExtension f == ".elm" then
--                           isPortModule f
--                         else
--                           pure False
--                     ) files
--     elmFilesInDirs <- fmap concat $ mapM findPortFilePaths dirs
--     return $ elmFiles ++ elmFilesInDirs

-- isPortModule :: FilePath -> IO Bool
-- isPortModule path = do
--     handle <- System.IO.openFile path System.IO.ReadMode
--     firstFour <- readFirst handle 4 ""
--     System.IO.hClose handle
--     pure (firstFour == "port")

-- readFirst :: System.IO.Handle -> Int -> String -> IO String
-- readFirst handle n existing =
--     if n <= 0 then
--         pure existing
--     else do
--       char <- System.IO.hGetChar handle
--       readFirst handle (n - 1) (existing ++ [char])
