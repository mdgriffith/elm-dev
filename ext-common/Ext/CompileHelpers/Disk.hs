module Ext.CompileHelpers.Disk
  ( compile
  , compileToDocs
  , compileToDocsCached
  , allPackageArtifacts
  )
where

import Control.Concurrent.MVar
import Control.Monad (liftM2, unless)
import Ext.Common
import Json.Encode ((==>))
import qualified Data.Map
-- import Data.Map.Strict ((!))
import qualified System.IO
import qualified Ext.Log
import qualified AST.Source as Src
import qualified Data.ByteString as B
import qualified Ext.FileProxy as File
import qualified BackgroundWriter as BW
import qualified Data.Map.Utils as Map
import qualified Data.Map.Strict as Map
import qualified AST.Optimized as Opt
import qualified Data.Set as Set
import qualified Elm.ModuleName as ModuleName
import qualified Reporting.Annotation as A
import qualified Data.Name as Name
import qualified Compile
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BSL
import qualified Data.NonEmptyList as NE
import qualified Ext.CompileHelpers.Generic as CompileHelpers
import qualified Build
import qualified Elm.Package as Pkg
import qualified Elm.Interface as I
import qualified Elm.Details as Details
import qualified Elm.Docs
import qualified Json.Encode as Encode
import qualified Reporting
import qualified Reporting.Error as Error
import qualified Reporting.Exit as Exit
import qualified Parse.Module as Parse
import qualified Reporting.Task as Task
import qualified System.Directory as Dir
import System.IO.Unsafe (unsafePerformIO)
import qualified Stuff
import qualified Generate.Html as Html
import qualified Generate
import qualified Data.ByteString as BS
import qualified Reporting.Error.Import as Import
import qualified Ext.Dev.Docs
import qualified Ext.Disk.Build

import Ext.Sanity

compile :: FilePath -> NE.List FilePath -> CompileHelpers.Flags -> IO (Either Exit.Reactor CompileHelpers.CompilationResult)
compile root paths flags@(CompileHelpers.Flags mode output) = do
  Dir.withCurrentDirectory root $
    BW.withScope $ \scope -> Stuff.withRootLock root $
      Task.run $ do
          let compilationFlags = CompileHelpers.compilationModsFromFlags mode
          details <- Task.eio Exit.ReactorBadDetails $ Details.load Reporting.silent scope root
          artifacts <- Task.eio Exit.ReactorBadBuild $ Ext.Disk.Build.fromPaths compilationFlags Reporting.silent root details paths
          
          CompileHelpers.generate root details mode artifacts output


compileToDocs :: FilePath -> NE.List ModuleName.Raw -> IO (Either Exit.Reactor Elm.Docs.Documentation)
compileToDocs root allModuleNames = do
  Dir.withCurrentDirectory root $
    BW.withScope $ \scope -> Stuff.withRootLock root $
      Task.run $ do
        details <- Task.eio Exit.ReactorBadDetails $ Details.load Reporting.silent scope root
        
        Task.eio Exit.ReactorBadBuild $ 
            docsFromExposed Reporting.silent root details Build.KeepDocs 
              allModuleNames



compileToDocsCached :: FilePath -> NE.List ModuleName.Raw -> Details.Details -> IO (Either Exit.Reactor Elm.Docs.Documentation)
compileToDocsCached root allModuleNames details = do
  Dir.withCurrentDirectory root $
    BW.withScope $ \scope -> Stuff.withRootLock root $
      Task.run $ 
        Task.eio Exit.ReactorBadBuild $ 
            docsFromExposed Reporting.silent root details Build.KeepDocs 
              allModuleNames



{- Appropriated from worker/src/Artifacts.hs
   WARNING: does not load any user code!!!
-}
allPackageArtifacts :: FilePath ->  IO CompileHelpers.Artifacts
allPackageArtifacts root =
  BW.withScope $ \scope ->
  do  --debug "Loading allDeps"
      let style = Reporting.silent
      result <- Details.load style scope root
      case result of
        Left _ ->
          error $ "Ran into some problem loading elm.json\nTry running `elm make` in: " ++ root

        Right details ->
          do  omvar <- Details.loadObjects root details
              imvar <- Details.loadInterfaces root details
              mdeps <- readMVar imvar
              mobjs <- readMVar omvar
              case liftM2 (,) mdeps mobjs of
                Nothing ->
                  error $ "Ran into some weird problem loading elm.json\nTry running `elm make` in: " ++ root

                Just (deps, objs) ->
                  return $ CompileHelpers.Artifacts (CompileHelpers.toInterfaces deps) objs





{- DOCS FROM EXPOSED

This mimics Build.fromExposed, but
  1. Does not write any files
  2. Generates docs with the rased constraints put forth in `Ext.Dev.Docs`

-}

docsFromExposed :: Reporting.Style -> FilePath -> Details.Details -> Build.DocsGoal docs -> NE.List ModuleName.Raw -> IO (Either Exit.BuildProblem Elm.Docs.Documentation)
docsFromExposed style root details docsGoal exposed@(NE.List e es) =
  Reporting.trackBuild style $ \key ->
  do  
      env <- Build.makeEnv key root details
      dmvar <- Details.loadInterfaces root details

      -- crawl
      mvar <- newEmptyMVar
      let docsNeed = Build.toDocsNeed docsGoal
      roots <- Map.fromKeysA (Build.fork . Build.crawlModule env mvar docsNeed) (e:es)
      putMVar mvar roots
      mapM_ readMVar roots
      statuses <- traverse readMVar =<< readMVar mvar

      -- compile
      midpoint <- Build.checkMidpoint dmvar statuses
      case midpoint of
        Left problem ->
          return (Left (Exit.BuildProjectProblem problem))

        Right foreigns ->
          do  
              rmvar <- newEmptyMVar
              resultMVars <- Build.forkWithKey (checkModule env foreigns rmvar) statuses
              putMVar rmvar resultMVars
              results <- traverse readMVar resultMVars
              -- Skip writing
              -- writeDetails root details results
              -- finalizeExposed root docsGoal exposed results
              -- Build.finalizeDocs
              return $ Right (Map.mapMaybe toDocs results)



type DocsResultDict =
  Map.Map ModuleName.Raw (MVar DocsResult)


{-| Roughly the same as Build.Result, but only on docs.

-}
data DocsResult
  = RNew !Details.Local !I.Interface !Opt.LocalGraph !(Maybe Elm.Docs.Module)
  | RSame !Details.Local !I.Interface !Opt.LocalGraph !(Maybe Elm.Docs.Module)
  | RCached Bool Details.BuildID (MVar Build.CachedInterface)
  | RNotFound Import.Problem
  | RProblem Error.Module
  | RBlocked
  | RForeign I.Interface
  | RKernel



toDocs :: DocsResult -> Maybe Elm.Docs.Module
toDocs result =
  case result of
    RNew  _ _ _ d -> d
    RSame _ _ _ d -> d
    RCached _ _ _ -> Nothing
    RNotFound _   -> Nothing
    RProblem _    -> Nothing
    RBlocked      -> Nothing
    RForeign _    -> Nothing
    RKernel       -> Nothing


checkModule :: Build.Env -> Build.Dependencies -> MVar DocsResultDict -> ModuleName.Raw -> Build.Status -> IO DocsResult
checkModule env@(Build.Env _ root projectType _ _ _ _) foreigns resultsMVar name status = do 
  case status of
    Build.SCached local@(Details.Local path time deps hasMain lastChange lastCompile) ->
      do  results <- readMVar resultsMVar
          depsStatus <- checkDeps root results deps lastCompile
          case depsStatus of
            DepsChange ifaces ->
              do  source <- File.readUtf8 path
                  case Parse.fromByteString projectType source of
                    Right modul -> compileSingleToDocs env (Build.DocsNeed False) local source ifaces modul
                    Left err ->
                      return $ RProblem $
                        Error.Module name path time source (Error.BadSyntax err)

            DepsSame _ _ ->
              do  mvar <- newMVar Build.Unneeded
                  return (RCached hasMain lastChange mvar)

            DepsBlock ->
              return RBlocked

            DepsNotFound problems ->
              do  source <- File.readUtf8 path
                  return $ RProblem $ Error.Module name path time source $
                    case Parse.fromByteString projectType source of
                      Right (Src.Module _ _ _ imports _ _ _ _ _) ->
                         Error.BadImports (toImportErrors env results imports problems)

                      Left err ->
                        Error.BadSyntax err

    Build.SChanged local@(Details.Local path time deps _ _ lastCompile) source modul@(Src.Module _ _ _ imports _ _ _ _ _) docsNeed ->
      do  results <- readMVar resultsMVar
          depsStatus <- checkDeps root results deps lastCompile
          case depsStatus of
            DepsChange ifaces ->
              compileSingleToDocs env docsNeed local source ifaces modul

            DepsSame same cached ->
              do  maybeLoaded <- Build.loadInterfaces root same cached
                  case maybeLoaded of
                    Nothing     -> return RBlocked
                    Just ifaces -> compileSingleToDocs env docsNeed local source ifaces modul

            DepsBlock ->
              return RBlocked

            DepsNotFound problems ->
              return $ RProblem $ Error.Module name path time source $
                Error.BadImports (toImportErrors env results imports problems)

    Build.SBadImport importProblem ->
      return (RNotFound importProblem)

    Build.SBadSyntax path time source err ->
      return $ RProblem $ Error.Module name path time source $
        Error.BadSyntax err

    Build.SForeign home ->
      case foreigns ! ModuleName.Canonical home name of
        I.Public iface -> return (RForeign iface)
        I.Private _ _ _ -> error $ "mistakenly seeing private interface for " ++ Pkg.toChars home ++ " " ++ ModuleName.toChars name

    Build.SKernel ->
      return RKernel



compileSingleToDocs :: Build.Env -> Build.DocsNeed -> Details.Local -> B.ByteString -> Map.Map ModuleName.Raw I.Interface -> Src.Module -> IO DocsResult
compileSingleToDocs (Build.Env key root projectType _ buildID _ _) docsNeed (Details.Local path time deps main lastChange _) source ifaces modul =
  let
    pkg = Build.projectTypeToPkg projectType
  in
  case Compile.compile pkg ifaces modul of
    Right (artifacts@(Compile.Artifacts canonical annotations objects)) ->
      case Ext.Dev.Docs.fromArtifacts artifacts of
        Left err ->
          return $ RProblem $
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
                      return (RSame local iface objects (Just docs))

                _ ->
                  do  -- iface may be lazy still
                      File.writeBinary elmi iface
                      Reporting.report key Reporting.BDone
                      let local = Details.Local path time deps main buildID buildID
                      return (RNew local iface objects (Just docs))
      -- return RKernel

    Left err ->
      return $ RProblem $
        Error.Module (Src.getName modul) path time source err





-- CHECK DEPS - Copied from Build.hs


data DepsStatus
  = DepsChange (Map.Map ModuleName.Raw I.Interface)
  | DepsSame [Dep] [CDep]
  | DepsBlock
  | DepsNotFound (NE.List (ModuleName.Raw, Import.Problem))


checkDeps :: FilePath -> DocsResultDict -> [ModuleName.Raw] -> Details.BuildID -> IO DepsStatus
checkDeps root results deps lastCompile =
  checkDepsHelp root results deps [] [] [] [] False 0 lastCompile


type Dep = (ModuleName.Raw, I.Interface)
type CDep = (ModuleName.Raw, MVar Build.CachedInterface)


checkDepsHelp :: FilePath -> DocsResultDict -> [ModuleName.Raw] -> [Dep] -> [Dep] -> [CDep] -> [(ModuleName.Raw,Import.Problem)] -> Bool -> Details.BuildID -> Details.BuildID -> IO DepsStatus
checkDepsHelp root results deps new same cached importProblems isBlocked lastDepChange lastCompile =
  case deps of
    dep:otherDeps ->
      do  result <- readMVar (results ! dep)
          case result of
            RNew (Details.Local _ _ _ _ lastChange _) iface _ _ ->
              checkDepsHelp root results otherDeps ((dep,iface) : new) same cached importProblems isBlocked (max lastChange lastDepChange) lastCompile

            RSame (Details.Local _ _ _ _ lastChange _) iface _ _ ->
              checkDepsHelp root results otherDeps new ((dep,iface) : same) cached importProblems isBlocked (max lastChange lastDepChange) lastCompile

            RCached _ lastChange mvar ->
              checkDepsHelp root results otherDeps new same ((dep,mvar) : cached) importProblems isBlocked (max lastChange lastDepChange) lastCompile

            RNotFound prob ->
              checkDepsHelp root results otherDeps new same cached ((dep,prob) : importProblems) True lastDepChange lastCompile

            RProblem _ ->
              checkDepsHelp root results otherDeps new same cached importProblems True lastDepChange lastCompile

            RBlocked ->
              checkDepsHelp root results otherDeps new same cached importProblems True lastDepChange lastCompile

            RForeign iface ->
              checkDepsHelp root results otherDeps new ((dep,iface) : same) cached importProblems isBlocked lastDepChange lastCompile

            RKernel ->
              checkDepsHelp root results otherDeps new same cached importProblems isBlocked lastDepChange lastCompile


    [] ->
      case reverse importProblems of
        p:ps ->
          return $ DepsNotFound (NE.List p ps)

        [] ->
          if isBlocked then
            return $ DepsBlock

          else if null new && lastDepChange <= lastCompile then
            return $ DepsSame same cached

          else
            do  maybeLoaded <- Build.loadInterfaces root same cached
                case maybeLoaded of
                  Nothing     -> return DepsBlock
                  Just ifaces -> return $ DepsChange $ Map.union (Map.fromList new) ifaces




-- TO IMPORT ERROR


toImportErrors :: Build.Env -> DocsResultDict -> [Src.Import] -> NE.List (ModuleName.Raw, Import.Problem) -> NE.List Import.Error
toImportErrors (Build.Env _ _ _ _ _ locals foreigns) results imports problems =
  let
    knownModules =
      Set.unions
        [ Map.keysSet foreigns
        , Map.keysSet locals
        , Map.keysSet results
        ]

    unimportedModules =
      Set.difference knownModules (Set.fromList (map Src.getImportName imports))

    regionDict =
      Map.fromList (map (\(Src.Import (A.At region name) _ _) -> (name, region)) imports)

    toError (name, problem) =
      Import.Error (regionDict ! name) name unimportedModules problem
  in
  fmap toError problems

