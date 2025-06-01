-- Clone of builder/src/Build.hs modified to use MemoryCached.*

{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE BangPatterns, GADTs, OverloadedStrings #-}
module Ext.MemoryCached.Build
  ( fromPathsMemoryCached
  , bustArtifactsCache
  )
  where


import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import System.IO.Unsafe (unsafePerformIO)
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

import qualified Elm.Docs as Docs
import qualified Elm.Interface as I
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Outline as Outline
import qualified Elm.Package as Pkg

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
import qualified Stuff
import qualified Ext.Log

-- File caching helpers
import qualified Ext.MemoryCached.Details as Details
import qualified Ext.CompileHelpers.Generic as CompileHelpers
import qualified Ext.FileCache as File

import qualified Build
import Prelude hiding (log)
import Build (Module(..), Artifacts(..), CachedInterface(..), Root(..))

import StandaloneInstances

debug =
  Ext.Log.log Ext.Log.MemoryCache




{-# NOINLINE artifactCache #-}
artifactCache :: MVar (Maybe Artifacts)
artifactCache = unsafePerformIO $ newMVar Nothing

bustArtifactsCache = modifyMVar_ artifactCache (\_ -> pure Nothing)


fromPathsMemoryCached :: CompileHelpers.CompilationFlags -> Reporting.Style -> FilePath -> Details.Details -> NE.List FilePath -> IO (Either Exit.BuildProblem Artifacts)
fromPathsMemoryCached flags style root details paths = do
  artifaceCacheM <- readMVar artifactCache
  case artifaceCacheM of
    Just artifacts -> do
      debug $ "🎯 artifacts cache hit"
      pure $ Right artifacts
    Nothing -> do
      debug $ "❌ artifacts cache miss"
      modifyMVar artifactCache (\_ -> do
          artifactsR <- fromPathsMemoryCached_ flags style root details paths
          case artifactsR of
            Right artifacts -> do
              pure (Just artifacts, artifactsR)
            _ ->
              pure (Nothing, artifactsR)
        )


fromPathsMemoryCached_ :: CompileHelpers.CompilationFlags -> Reporting.Style -> FilePath -> Details.Details -> NE.List FilePath -> IO (Either Exit.BuildProblem Artifacts)
fromPathsMemoryCached_ flags style root details paths =
  Reporting.trackBuild style $ \key ->
  do  env <- Build.makeEnv key root details
      elroots <- findRoots env paths
      case elroots of
        Left problem ->
          return (Left (Exit.BuildProjectProblem problem))

        Right lroots ->
          do  -- crawl
              -- log "lroots" (show lroots)
              dmvar <- Details.loadInterfaces root details
              smvar <- newMVar Map.empty
              srootMVars <- traverse (Build.fork . crawlRoot env smvar) lroots
              sroots <- traverse readMVar srootMVars
              -- log "sroots" (show sroots)
              statuses <- traverse readMVar =<< readMVar smvar

              midpoint <- Build.checkMidpointAndRoots dmvar statuses sroots
              case midpoint of
                Left problem ->
                  return (Left (Exit.BuildProjectProblem problem))

                Right foreigns ->
                  do  -- compile
                      rmvar <- newEmptyMVar
                      -- debug $ "statuses: " <> show statuses
                      resultsMVars <- Build.forkWithKey (checkModule flags env foreigns rmvar) statuses

                      putMVar rmvar resultsMVars
                      rrootMVars <- traverse (Build.fork . checkRoot flags env resultsMVars) sroots
                      results <- traverse readMVar resultsMVars


                      -- Needs to NOT be Build.writeDetails, because it's writing the cached version
                      writeDetails root details results
                      x <- Build.toArtifacts env foreigns results <$> traverse readMVar rrootMVars
                      -- case x of
                      --   Right v ->
                      --     debug $ show v
                      --   _ -> pure ()
                      pure x




-- GET ROOT NAMES


getRootNames :: Artifacts -> NE.List ModuleName.Raw
getRootNames (Build.Artifacts _ _ roots _) =
  fmap getRootName roots


getRootName :: Root -> ModuleName.Raw
getRootName root =
  case root of
    Inside  name     -> name
    Outside name _ _ -> name



-- CRAWL


crawlDeps :: Build.Env -> MVar Build.StatusDict -> [ModuleName.Raw] -> a -> IO a
crawlDeps env mvar deps blockedValue =
  do  statusDict <- takeMVar mvar
      let depsDict = Map.fromKeys (\_ -> ()) deps
      let newsDict = Map.difference depsDict statusDict
      statuses <- Map.traverseWithKey crawlNew newsDict
      putMVar mvar (Map.union statuses statusDict)
      mapM_ readMVar statuses
      return blockedValue
  where
    crawlNew name () = Build.fork (crawlModule env mvar (Build.DocsNeed False) name)


crawlModule :: Build.Env -> MVar Build.StatusDict -> Build.DocsNeed -> ModuleName.Raw -> IO Build.Status
crawlModule env@(Build.Env _ root projectType srcDirs buildID locals foreigns) mvar docsNeed name =
  do  let fileName = ModuleName.toFilePath name <.> "elm"
      -- debug $ "crawlModule:" <> fileName
      paths <- filterM File.exists (map (`Build.addRelative` fileName) srcDirs)

      case paths of
        [path] ->
          case Map.lookup name foreigns of
            Just (Details.Foreign dep deps) ->
              return $ Build.SBadImport $ Import.Ambiguous path [] dep deps

            Nothing ->
              do  newTime <- File.getTime path
                  case Map.lookup name locals of
                    Nothing ->
                      crawlFile env mvar docsNeed name path newTime buildID

                    Just local@(Details.Local oldPath oldTime deps _ lastChange _) -> do
                      -- debug $ "old vs new times on " <> path <> " : " <> show (oldTime, newTime)
                      if path /= oldPath || oldTime /= newTime || Build.needsDocs docsNeed
                        then crawlFile env mvar docsNeed name path newTime lastChange
                        else crawlDeps env mvar deps (Build.SCached local)

        p1:p2:ps ->
          return $ Build.SBadImport $ Import.AmbiguousLocal (FP.makeRelative root p1) (FP.makeRelative root p2) (map (FP.makeRelative root) ps)

        [] ->
          case Map.lookup name foreigns of
            Just (Details.Foreign dep deps) ->
              case deps of
                [] ->
                  return $ Build.SForeign dep

                d:ds ->
                  return $ Build.SBadImport $ Import.AmbiguousForeign dep d ds

            Nothing ->
              if Name.isKernel name && Parse.isKernel projectType then
                do  exists <- File.exists ("src" </> ModuleName.toFilePath name <.> "js")
                    return $ if exists then Build.SKernel else Build.SBadImport Import.NotFound
              else
                return $ Build.SBadImport Import.NotFound


crawlFile :: Build.Env -> MVar Build.StatusDict -> Build.DocsNeed -> ModuleName.Raw -> FilePath -> File.Time -> Details.BuildID -> IO Build.Status
crawlFile env@(Build.Env _ root projectType _ buildID _ _) mvar docsNeed expectedName path time lastChange =
  do  source <- File.readUtf8 (root </> path)

      case Parse.fromByteString projectType source of
        Left err ->
          return $ Build.SBadSyntax path time source err

        Right modul@(Src.Module maybeActualName _ _ imports values _ _ _ _) ->
          case maybeActualName of
            Nothing ->
              return $ Build.SBadSyntax path time source (Syntax.ModuleNameUnspecified expectedName)

            Just name@(A.At _ actualName) ->
              if expectedName == actualName then
                let
                  deps = map Src.getImportName imports
                  local = Details.Local path time deps (any Build.isMain values) lastChange buildID
                in
                crawlDeps env mvar deps (Build.SChanged local source modul docsNeed)
              else
                return $ Build.SBadSyntax path time source (Syntax.ModuleNameMismatch expectedName name)





-- CHECK MODULE


checkModule :: CompileHelpers.CompilationFlags -> Build.Env -> Build.Dependencies -> MVar Build.ResultDict -> ModuleName.Raw -> Build.Status -> IO Build.Result
checkModule flags env@(Build.Env _ root projectType _ _ _ _) foreigns resultsMVar name status =
 do
  case status of
    Build.SCached local@(Details.Local path time deps hasMain lastChange lastCompile) ->
      do  results <- readMVar resultsMVar
          depsStatus <- checkDeps root results deps lastCompile
          case depsStatus of
            Build.DepsChange ifaces ->
              do  source <- File.readUtf8 path
                  -- debug $ "checkModule:SCached:DepsChange " ++ show name
                  case Parse.fromByteString projectType source of
                    Right modul -> compile flags env (Build.DocsNeed False) local source ifaces modul
                    Left err ->
                      return $ Build.RProblem $
                        Error.Module name path time source (Error.BadSyntax err)

            Build.DepsSame _ _ ->
              do  mvar <- newMVar Build.Unneeded
                  -- debug $ "checkModule:SCached:DepsSame " ++ show name
                  return (Build.RCached hasMain lastChange mvar)

            Build.DepsBlock ->
             do
              -- debug $ "checkModule:SCached:DepsBlock " ++ show name
              return Build.RBlocked

            Build.DepsNotFound problems ->
              do  source <- File.readUtf8 path
                  -- debug $ "checkModule:SCached:DepsNotFound " ++ show name
                  return $ Build.RProblem $ Error.Module name path time source $
                    case Parse.fromByteString projectType source of
                      Right (Src.Module _ _ _ imports _ _ _ _ _) ->
                         Error.BadImports (Build.toImportErrors env results imports problems)

                      Left err ->
                        Error.BadSyntax err

    Build.SChanged local@(Details.Local path time deps _ _ lastCompile) source modul@(Src.Module _ _ _ imports _ _ _ _ _) docsNeed ->
      do  results <- readMVar resultsMVar
          depsStatus <- checkDeps root results deps lastCompile
          case depsStatus of
            Build.DepsChange ifaces ->
             do
              -- debug $ "checkModule:SChanged:DepsChange " ++ show name
              compile flags env docsNeed local source ifaces modul

            Build.DepsSame same cached ->
              do  -- debug $ "checkModule:SChanged:DepsSame " ++ show name
                  maybeLoaded <- loadInterfaces root same cached
                  case maybeLoaded of
                    Nothing     -> return Build.RBlocked
                    Just ifaces -> compile flags env docsNeed local source ifaces modul

            Build.DepsBlock ->
             do
              -- debug $ "checkModule:SChanged:DepsBlock " ++ show name
              return Build.RBlocked

            Build.DepsNotFound problems ->
             do
              -- debug $ "checkModule:SChanged:DepsNotFound " ++ show name
              return $ Build.RProblem $ Error.Module name path time source $
                Error.BadImports (Build.toImportErrors env results imports problems)

    Build.SBadImport importProblem ->
     do
      -- debug $ "checkModule:SBadImport " ++ show name
      return (Build.RNotFound importProblem)

    Build.SBadSyntax path time source err ->
     do
      -- debug $ "checkModule:SBadSyntax " ++ show name
      return $ Build.RProblem $ Error.Module name path time source $
        Error.BadSyntax err

    Build.SForeign home ->
     do
      -- debug $ "checkModule:SForeign " ++ show name
      case foreigns ! ModuleName.Canonical home name of
        I.Public iface -> return (Build.RForeign iface)
        I.Private _ _ _ -> error $ "mistakenly seeing private interface for " ++ Pkg.toChars home ++ " " ++ ModuleName.toChars name

    Build.SKernel ->
     do
      -- debug $ "checkModule:SKernel " ++ show name
      return Build.RKernel



-- CHECK DEPS

checkDeps :: FilePath -> Build.ResultDict -> [ModuleName.Raw] -> Details.BuildID -> IO Build.DepsStatus
checkDeps root results deps lastCompile =
  checkDepsHelp root results deps [] [] [] [] False 0 lastCompile

checkDepsHelp :: FilePath -> Build.ResultDict -> [ModuleName.Raw] -> [Build.Dep] -> [Build.Dep] -> [Build.CDep] -> [(ModuleName.Raw,Import.Problem)] -> Bool -> Details.BuildID -> Details.BuildID -> IO Build.DepsStatus
checkDepsHelp root results deps new same cached importProblems isBlocked lastDepChange lastCompile =
  case deps of
    dep:otherDeps ->
      do  result <- readMVar (results ! dep)
          case result of
            Build.RNew (Details.Local _ _ _ _ lastChange _) iface _ _ ->
              checkDepsHelp root results otherDeps ((dep,iface) : new) same cached importProblems isBlocked (max lastChange lastDepChange) lastCompile

            Build.RSame (Details.Local _ _ _ _ lastChange _) iface _ _ ->
              checkDepsHelp root results otherDeps new ((dep,iface) : same) cached importProblems isBlocked (max lastChange lastDepChange) lastCompile

            Build.RCached _ lastChange mvar ->
              checkDepsHelp root results otherDeps new same ((dep,mvar) : cached) importProblems isBlocked (max lastChange lastDepChange) lastCompile

            Build.RNotFound prob ->
              checkDepsHelp root results otherDeps new same cached ((dep,prob) : importProblems) True lastDepChange lastCompile

            Build.RProblem _ ->
              checkDepsHelp root results otherDeps new same cached importProblems True lastDepChange lastCompile

            Build.RBlocked ->
              checkDepsHelp root results otherDeps new same cached importProblems True lastDepChange lastCompile

            Build.RForeign iface ->
              checkDepsHelp root results otherDeps new ((dep,iface) : same) cached importProblems isBlocked lastDepChange lastCompile

            Build.RKernel ->
              checkDepsHelp root results otherDeps new same cached importProblems isBlocked lastDepChange lastCompile


    [] ->
      case reverse importProblems of
        p:ps ->
          return $ Build.DepsNotFound (NE.List p ps)

        [] ->
          if isBlocked then
            return $ Build.DepsBlock

          else if null new && lastDepChange <= lastCompile then
            return $ Build.DepsSame same cached

          else
            -- loadInterfaces calls the cached version
            do  maybeLoaded <- loadInterfaces root same cached
                case maybeLoaded of
                  Nothing     -> return Build.DepsBlock
                  Just ifaces -> return $ Build.DepsChange $ Map.union (Map.fromList new) ifaces




-- -- LOAD CACHED INTERFACES


loadInterfaces :: FilePath -> [Build.Dep] -> [Build.CDep] -> IO (Maybe (Map.Map ModuleName.Raw I.Interface))
loadInterfaces root same cached =
  do  loading <- traverse (Build.fork . loadInterface root) cached
      maybeLoaded <- traverse readMVar loading
      case sequence maybeLoaded of
        Nothing ->
          return Nothing

        Just loaded ->
          return $ Just $ Map.union (Map.fromList loaded) (Map.fromList same)


loadInterface :: FilePath -> Build.CDep -> IO (Maybe Build.Dep)
loadInterface root (name, ciMvar) =
  do  cachedInterface <- takeMVar ciMvar
      case cachedInterface of
        Build.Corrupted ->
          do  putMVar ciMvar cachedInterface
              return Nothing

        Build.Loaded iface ->
          do  putMVar ciMvar cachedInterface
              return (Just (name, iface))

        Build.Unneeded ->
          do  maybeIface <- File.readBinary (Stuff.elmi root name)
              case maybeIface of
                Nothing ->
                  do  putMVar ciMvar Build.Corrupted
                      return Nothing

                Just iface ->
                  do  putMVar ciMvar (Build.Loaded iface)
                      return (Just (name, iface))




-- COMPILE MODULE


compile :: CompileHelpers.CompilationFlags -> Build.Env -> Build.DocsNeed -> Details.Local -> B.ByteString -> Map.Map ModuleName.Raw I.Interface -> Src.Module -> IO Build.Result
compile flags (Build.Env key root projectType _ buildID _ _) docsNeed (Details.Local path time deps main lastChange _) source ifaces modul = do
  let pkg = Build.projectTypeToPkg projectType
  case CompileHelpers.compile flags pkg ifaces modul of
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

-- WRITE DETAILS


writeDetails :: FilePath -> Details.Details -> Map.Map ModuleName.Raw Build.Result -> IO ()
writeDetails root (Details.Details time outline buildID locals foreigns extras) results =
  File.writeBinary (Stuff.details root) $
    Details.Details time outline buildID (Map.foldrWithKey Build.addNewLocal locals results) foreigns extras


-- FINALIZE EXPOSED


-- DOCS


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
------ AFTER THIS, EVERYTHING IS ABOUT HANDLING MODULES GIVEN BY FILEPATH ------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------



-- FIND ROOT


findRoots :: Build.Env -> NE.List FilePath -> IO (Either Exit.BuildProjectProblem (NE.List Build.RootLocation))
findRoots env paths =
  do  mvars <- traverse (Build.fork . getRootInfo env) paths
      einfos <- traverse readMVar mvars
      return $ checkRoots =<< sequence einfos


checkRoots :: NE.List Build.RootInfo -> Either Exit.BuildProjectProblem (NE.List Build.RootLocation)
checkRoots infos =
  let
    toOneOrMore loc@(Build.RootInfo absolute _ _) =
      (absolute, OneOrMore.one loc)

    fromOneOrMore loc locs =
      case locs of
        [] -> Right ()
        loc2:_ -> Left (Exit.BP_MainPathDuplicate (Build._relative loc) (Build._relative loc2))
  in
  fmap (\_ -> fmap Build._location infos) $
    traverse (OneOrMore.destruct fromOneOrMore) $
      Map.fromListWith OneOrMore.more $ map toOneOrMore (NE.toList infos)



-- -- ROOT INFO

getRootInfo :: Build.Env -> FilePath -> IO (Either Exit.BuildProjectProblem Build.RootInfo)
getRootInfo env path =
  do  exists <- File.exists path
      if exists
        then getRootInfoHelp env path =<< Dir.canonicalizePath path
        else return (Left (Exit.BP_PathUnknown path))


getRootInfoHelp :: Build.Env -> FilePath -> FilePath -> IO (Either Exit.BuildProjectProblem Build.RootInfo)
getRootInfoHelp (Build.Env _ _ _ srcDirs _ _ _) path absolutePath =
  let
    (dirs, file) = FP.splitFileName absolutePath
    (final, ext) = FP.splitExtension file
  in
  if ext /= ".elm"
  then
    return $ Left $ Exit.BP_WithBadExtension path
  else
    let
      absoluteSegments = FP.splitDirectories dirs ++ [final]
    in
    case Maybe.mapMaybe (Build.isInsideSrcDirByPath absoluteSegments) srcDirs of
      [] ->
        return $ Right $ Build.RootInfo absolutePath path (Build.LOutside path)

      [(_, Right names)] ->
        do  let name = Name.fromChars (List.intercalate "." names)
            matchingDirs <- filterM (isInsideSrcDirByName names) srcDirs
            case matchingDirs of
              d1:d2:_ ->
                do  let p1 = Build.addRelative d1 (FP.joinPath names <.> "elm")
                    let p2 = Build.addRelative d2 (FP.joinPath names <.> "elm")
                    return $ Left $ Exit.BP_RootNameDuplicate name p1 p2

              _ ->
                return $ Right $ Build.RootInfo absolutePath path (Build.LInside name)

      [(s, Left names)] ->
        return $ Left $ Exit.BP_RootNameInvalid path s names

      (s1,_):(s2,_):_ ->
        return $ Left $ Exit.BP_WithAmbiguousSrcDir path s1 s2



isInsideSrcDirByName :: [String] -> Build.AbsoluteSrcDir -> IO Bool
isInsideSrcDirByName names srcDir =
  File.exists (Build.addRelative srcDir (FP.joinPath names <.> "elm"))


isGoodName :: [Char] -> Bool
isGoodName name =
  case name of
    [] ->
      False

    char:chars ->
      Char.isUpper char && all (\c -> Char.isAlphaNum c || c == '_') chars


-- -- INVARIANT: Dir.canonicalizePath has been run on both inputs
--
dropPrefix :: [FilePath] -> [FilePath] -> Maybe [FilePath]
dropPrefix roots paths =
  case roots of
    [] ->
      Just paths

    r:rs ->
      case paths of
        []   -> Nothing
        p:ps -> if r == p then dropPrefix rs ps else Nothing



-- -- CRAWL ROOTS


crawlRoot :: Build.Env -> MVar Build.StatusDict -> Build.RootLocation -> IO Build.RootStatus
crawlRoot env@(Build.Env _ _ projectType _ buildID _ _) mvar root =
  case root of
    Build.LInside name ->
      do  statusMVar <- newEmptyMVar
          statusDict <- takeMVar mvar
          putMVar mvar (Map.insert name statusMVar statusDict)
          putMVar statusMVar =<< crawlModule env mvar (Build.DocsNeed False) name
          return (Build.SInside name)

    Build.LOutside path ->
      do  time <- File.getTime path
          source <- File.readUtf8 path
          case Parse.fromByteString projectType source of
            Right modul@(Src.Module _ _ _ imports values _ _ _ _) ->
              do  let deps = map Src.getImportName imports
                  let local = Details.Local path time deps (any Build.isMain values) buildID buildID
                  crawlDeps env mvar deps (Build.SOutsideOk local source modul)

            Left syntaxError ->
              return $ Build.SOutsideErr $
                Error.Module "???" path time source (Error.BadSyntax syntaxError)



-- CHECK ROOTS

checkRoot :: CompileHelpers.CompilationFlags -> Build.Env -> Build.ResultDict -> Build.RootStatus -> IO Build.RootResult
checkRoot flags env@(Build.Env _ root _ _ _ _ _) results rootStatus =
  case rootStatus of
    Build.SInside name ->
      return (Build.RInside name)

    Build.SOutsideErr err ->
      return (Build.ROutsideErr err)

    Build.SOutsideOk local@(Details.Local path time deps _ _ lastCompile) source modul@(Src.Module _ _ _ imports _ _ _ _ _) ->
      do  depsStatus <- checkDeps root results deps lastCompile
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


compileOutside :: CompileHelpers.CompilationFlags -> Build.Env -> Details.Local -> B.ByteString -> Map.Map ModuleName.Raw I.Interface -> Src.Module -> IO Build.RootResult
compileOutside flags (Build.Env key _ projectType _ _ _ _) (Details.Local path time _ _ _ _) source ifaces modul =
  let
    pkg = Build.projectTypeToPkg projectType
    name = Src.getName modul
  in
  case CompileHelpers.compile flags pkg ifaces modul of
    Right (Compile.Artifacts canonical annotations objects) -> do
     
      Reporting.report key Reporting.BDone
      return $ Build.ROutsideOk name (I.fromModule pkg canonical annotations) objects

    Left errors ->
      return $ Build.ROutsideErr $ Error.Module name path time source errors


