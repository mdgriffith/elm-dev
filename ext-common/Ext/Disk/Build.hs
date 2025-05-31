{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE BangPatterns, GADTs, OverloadedStrings #-}
module Ext.Disk.Build
  ( fromPaths
  )
  where


{-  Mirrors Build.hs and Ext.MemoryCached.Build.



-}


import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Monad (filterM, mapM_, sequence_)
import qualified Data.ByteString as B
import qualified Data.Char as Char
import qualified Data.Graph as Graph
import qualified Data.List as List
import qualified Data.Map.Utils as Map
import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!))
import qualified Data.Maybe as Maybe
import qualified Data.Name as Name
import qualified Data.NonEmptyList as NE
import qualified Data.OneOrMore as OneOrMore
import qualified Data.Set as Set
import qualified System.Directory as Dir
import qualified System.FilePath as FP
import System.FilePath ((</>), (<.>))

import qualified AST.Canonical as Can
import qualified AST.Source as Src
import qualified AST.Optimized as Opt
import qualified Compile
import qualified Elm.Details as Details
import qualified Elm.Docs as Docs
import qualified Elm.Interface as I
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Outline as Outline
import qualified Elm.Package as Pkg
import qualified Ext.FileProxy as File
import qualified Json.Encode as E
import qualified Parse.Module as Parse
import qualified Reporting
import qualified Reporting.Annotation as A
import qualified Reporting.Error as Error
import qualified Reporting.Error.Docs as EDocs
import qualified Reporting.Error.Syntax as Syntax
import qualified Reporting.Error.Import as Import
import qualified Reporting.Exit as Exit
import qualified Reporting.Render.Type.Localizer as L
import qualified Ext.CompileHelpers.Generic as CompileHelpers
import qualified Stuff
import qualified Build
import Build (Module(..), Artifacts(..), CachedInterface(..), Root(..))
import qualified Modify



-- FROM PATHS




fromPaths :: Reporting.Style -> FilePath -> Details.Details -> NE.List FilePath -> IO (Either Exit.BuildProblem Artifacts)
fromPaths style root details paths =
  Reporting.trackBuild style $ \key ->
  do  env <- Build.makeEnv key root details

      elroots <- Build.findRoots env paths
      case elroots of
        Left problem ->
          return (Left (Exit.BuildProjectProblem problem))

        Right lroots ->
          do  -- crawl
              dmvar <- Details.loadInterfaces root details
              smvar <- newMVar Map.empty
              srootMVars <- traverse (Build.fork . Build.crawlRoot env smvar) lroots
              sroots <- traverse readMVar srootMVars
              statuses <- traverse readMVar =<< readMVar smvar

              midpoint <- checkMidpointAndRoots dmvar statuses sroots
              case midpoint of
                Left problem ->
                  return (Left (Exit.BuildProjectProblem problem))

                Right foreigns ->
                  do  -- compile
                      rmvar <- newEmptyMVar
                      resultsMVars <- Build.forkWithKey (checkModule env foreigns rmvar) statuses
                      putMVar rmvar resultsMVars
                      rrootMVars <- traverse (Build.fork . checkRoot env resultsMVars) sroots
                      results <- traverse readMVar resultsMVars
                      Build.writeDetails root details results
                      Build.toArtifacts env foreigns results <$> traverse readMVar rrootMVars







checkModule :: Build.Env -> Build.Dependencies -> MVar Build.ResultDict -> ModuleName.Raw -> Build.Status -> IO Build.Result
checkModule env@(Build.Env _ root projectType _ _ _ _) foreigns resultsMVar name status =
  case status of
    Build.SCached local@(Details.Local path time deps hasMain lastChange lastCompile) ->
      do  results <- readMVar resultsMVar
          depsStatus <- Build.checkDeps root results deps lastCompile
          case depsStatus of
            Build.DepsChange ifaces ->
              do  source <- File.readUtf8 path
                  case Parse.fromByteString projectType source of
                    Right modul -> compile env (Build.DocsNeed False) local source ifaces modul
                    Left err ->
                      return $ Build.RProblem $
                        Error.Module name path time source (Error.BadSyntax err)

            Build.DepsSame _ _ ->
              do  mvar <- newMVar Build.Unneeded
                  return (Build.RCached hasMain lastChange mvar)

            Build.DepsBlock ->
              return Build.RBlocked

            Build.DepsNotFound problems ->
              do  source <- File.readUtf8 path
                  return $ Build.RProblem $ Error.Module name path time source $
                    case Parse.fromByteString projectType source of
                      Right (Src.Module _ _ _ imports _ _ _ _ _) ->
                         Error.BadImports (Build.toImportErrors env results imports problems)

                      Left err ->
                        Error.BadSyntax err

    Build.SChanged local@(Details.Local path time deps _ _ lastCompile) source modul@(Src.Module _ _ _ imports _ _ _ _ _) docsNeed ->
      do  results <- readMVar resultsMVar
          depsStatus <- Build.checkDeps root results deps lastCompile
          case depsStatus of
            Build.DepsChange ifaces ->
              compile env docsNeed local source ifaces modul

            Build.DepsSame same cached ->
              do  maybeLoaded <- Build.loadInterfaces root same cached
                  case maybeLoaded of
                    Nothing     -> return Build.RBlocked
                    Just ifaces -> compile env docsNeed local source ifaces modul

            Build.DepsBlock ->
              return Build.RBlocked

            Build.DepsNotFound problems ->
              return $ Build.RProblem $ Error.Module name path time source $
                Error.BadImports (Build.toImportErrors env results imports problems)

    Build.SBadImport importProblem ->
      return (Build.RNotFound importProblem)

    Build.SBadSyntax path time source err ->
      return $ Build.RProblem $ Error.Module name path time source $
        Error.BadSyntax err

    Build.SForeign home ->
      case foreigns ! ModuleName.Canonical home name of
        I.Public iface -> return (Build.RForeign iface)
        I.Private _ _ _ -> error $ "mistakenly seeing private interface for " ++ Pkg.toChars home ++ " " ++ ModuleName.toChars name

    Build.SKernel ->
      return Build.RKernel


-- CHECK PROJECT


checkMidpoint :: MVar (Maybe Build.Dependencies) -> Map.Map ModuleName.Raw Build.Status -> IO (Either Exit.BuildProjectProblem Build.Dependencies)
checkMidpoint dmvar statuses =
  case Build.checkForCycles statuses of
    Nothing ->
      do  maybeForeigns <- readMVar dmvar
          case maybeForeigns of
            Nothing -> return (Left Exit.BP_CannotLoadDependencies)
            Just fs -> return (Right fs)

    Just (NE.List name names) ->
      do  _ <- readMVar dmvar
          return (Left (Exit.BP_Cycle name names))


checkMidpointAndRoots :: MVar (Maybe Build.Dependencies) -> Map.Map ModuleName.Raw Build.Status -> NE.List Build.RootStatus -> IO (Either Exit.BuildProjectProblem Build.Dependencies)
checkMidpointAndRoots dmvar statuses sroots =
  case Build.checkForCycles statuses of
    Nothing ->
      case Build.checkUniqueRoots statuses sroots of
        Nothing ->
          do  maybeForeigns <- readMVar dmvar
              case maybeForeigns of
                Nothing -> return (Left Exit.BP_CannotLoadDependencies)
                Just fs -> return (Right fs)

        Just problem ->
          do  _ <- readMVar dmvar
              return (Left problem)

    Just (NE.List name names) ->
      do  _ <- readMVar dmvar
          return (Left (Exit.BP_Cycle name names))





-- COMPILE MODULE





compile :: Build.Env -> Build.DocsNeed -> Details.Local -> B.ByteString -> Map.Map ModuleName.Raw I.Interface -> Src.Module -> IO Build.Result
compile (Build.Env key root projectType _ buildID _ _) docsNeed (Details.Local path time deps main lastChange _) source ifaces modul =
  let
    pkg = Build.projectTypeToPkg projectType
  in
  case CompileHelpers.compile pkg ifaces modul of
    Right (Compile.Artifacts canonical annotations objects) -> do
      case Build.makeDocs docsNeed canonical of
        Left err ->
          return $ Build.RProblem $
            Error.Module (Src.getName modul) path time source (Error.BadDocs err)

        Right docs ->
          do  let name = Src.getName modul
              let iface = I.fromModule pkg canonical annotations
              let elmi = Stuff.elmi root name
              File.writeBinary (Stuff.elmo root name) objects
              maybeOldi <- File.readBinary elmi
              case maybeOldi of
                Just oldi | oldi == iface ->
                  do  -- iface should be fully forced by equality check
                      Reporting.report key Reporting.BDone
                      let local = Details.Local path time deps main lastChange buildID
                      return (Build.RSame local iface objects docs)

                _ ->
                  do  -- iface may be lazy still
                      File.writeBinary elmi iface
                      Reporting.report key Reporting.BDone
                      let local = Details.Local path time deps main buildID buildID
                      return (Build.RNew local iface objects docs)

    Left err ->
      return $ Build.RProblem $
        Error.Module (Src.getName modul) path time source err


checkRoot :: Build.Env -> Build.ResultDict -> Build.RootStatus -> IO Build.RootResult
checkRoot env@(Build.Env _ root _ _ _ _ _) results rootStatus =
  case rootStatus of
    Build.SInside name ->
      return (Build.RInside name)

    Build.SOutsideErr err ->
      return (Build.ROutsideErr err)

    Build.SOutsideOk local@(Details.Local path time deps _ _ lastCompile) source modul@(Src.Module _ _ _ imports _ _ _ _ _) ->
      do  depsStatus <- Build.checkDeps root results deps lastCompile
          case depsStatus of
            Build.DepsChange ifaces ->
              compileOutside env local source ifaces modul

            Build.DepsSame same cached ->
              do  maybeLoaded <- Build.loadInterfaces root same cached
                  case maybeLoaded of
                    Nothing     -> return Build.ROutsideBlocked
                    Just ifaces -> compileOutside env local source ifaces modul

            Build.DepsBlock ->
              return Build.ROutsideBlocked

            Build.DepsNotFound problems ->
              return $ Build.ROutsideErr $ Error.Module (Src.getName modul) path time source $
                  Error.BadImports (Build.toImportErrors env results imports problems)


compileOutside :: Build.Env -> Details.Local -> B.ByteString -> Map.Map ModuleName.Raw I.Interface -> Src.Module -> IO Build.RootResult
compileOutside (Build.Env key _ projectType _ _ _ _) (Details.Local path time _ _ _ _) source ifaces modul =
  let
    pkg = Build.projectTypeToPkg projectType
    name = Src.getName modul
  in
  case Compile.compile pkg ifaces modul of
    Right (Compile.Artifacts dirtyCanonical annotations objects) -> do
      let canonical = Modify.update dirtyCanonical
      Reporting.report key Reporting.BDone
      return $ Build.ROutsideOk name (I.fromModule pkg canonical annotations) objects

    Left errors ->
      return $ Build.ROutsideErr $ Error.Module name path time source errors

