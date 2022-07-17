{-# LANGUAGE ScopedTypeVariables #-}

module Ext.CompileProxy where

{- This is a proxy for all compilation related functions
   that ensures we can transparently swap compilation providers/methods
   (i.e. Disk vs MemoryCached)
 -}

import Control.Concurrent.MVar
import Ext.Common
import Ext.CompileMode (getMode, CompileMode(..))
import Json.Encode ((==>))
import qualified AST.Source as Src
import qualified BackgroundWriter as BW
import qualified Build
import qualified Canonicalize.Module as Canonicalize
import qualified Compile
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.NonEmptyList as NE
import qualified Elm.Details as Details
import qualified Elm.Interface as I
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified Ext.CompileHelpers.Disk
import qualified Ext.CompileHelpers.Generic as CompileHelpers
import qualified Ext.CompileHelpers.Generic as CompileHelpers
import qualified Ext.CompileHelpers.Memory
import qualified Ext.FileProxy as File
import qualified Json.Encode as Encode
import qualified Optimize.Module as Optimize
import qualified Parse.Module as Parse
import qualified Reporting
import qualified Reporting.Error
import qualified Reporting.Error.Syntax
import qualified Reporting.Exit as Exit
import qualified Reporting.Result
import qualified Reporting.Task as Task
import qualified Reporting.Warning as Warning
import qualified Stuff
import qualified System.Directory as Dir
import qualified Watchtower.Compile.Classic
import qualified Watchtower.Compile.MemoryCached
import System.IO.Unsafe (unsafePerformIO)

import StandaloneInstances


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
      pure result
    Memory -> do
      (t, label, result) <- Ext.Common.track_ ("🧠 memcached " ++ identifier) $ ioMemory
      addToAggregate Memory t label
      summary <- aggregateSummary
      Ext.Common.debug $ "📊 " <> summary
      pure result
    Race -> do
      results <- Ext.Common.race identifier [ ("🧠 memcached " ++ identifier, ioMemory) , ("🎻 classic   " ++ identifier, ioDisk) ]
      results & zip [Memory, Disk] & mapM_ (\(m, (t, l, r)) -> addToAggregate m t (l ++ " " ++ identifier))
      summary <- aggregateSummary
      Ext.Common.debug $ "📊 " <> summary
      (results !! 1) & (\(_,_,x) -> x) & pure


-- Interfaces


compileToJson :: FilePath -> NE.List FilePath -> IO (Either Encode.Value Encode.Value)
compileToJson root paths = do
  res <- modeRunner "compileToJson"
    (Watchtower.Compile.Classic.compileToJson root paths)
    (Watchtower.Compile.MemoryCached.compileToJson root paths)
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
warnings :: FilePath -> FilePath -> IO (Either () (Src.Module, [ Warning.Warning ]))
warnings root path =
  Dir.withCurrentDirectory root $ do
    ifaces <- allInterfaces [path]
    source <- File.readUtf8 path
    case Parse.fromByteString Parse.Application source of
      Right srcModule ->
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
                      let (optWarnings, _) = Reporting.Result.run $ Optimize.optimize annotations canModule
                      pure (Right (srcModule, canWarnings <> optWarnings))

      Left err ->
        pure (Left ())

  --  case snd $ R.run $ Optimize.optimize annotations canonical of
  --   Right localGraph ->
  --     Right localGraph

  --   Left errors ->
  --     Left (E.BadMains (Localizer.fromModule modul) errors)




-- Helpers



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
            \\nError in task, please report this.\
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

