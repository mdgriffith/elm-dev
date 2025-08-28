{-# LANGUAGE OverloadedStrings #-}
module Ext.Test.Install
  ( installTestDependency
  , InstallResult(..)
  ) where

import qualified Data.Map as Map

import qualified Deps.Solver as Solver
import qualified Elm.Constraint as Con
import qualified Elm.Outline as Outline
import qualified Elm.Package as Pkg
import qualified Reporting.Exit as Exit
import qualified Reporting.Task as Task
import qualified Stuff


data InstallResult
  = SuccessfullyInstalled
  | AlreadyInstalled
  deriving (Eq, Show)


-- | Install a package into test-dependencies of the nearest elm.json
installTestDependency :: Pkg.Name -> IO (Either Exit.Install InstallResult)
installTestDependency pkg = do
  maybeRoot <- Stuff.findRoot
  case maybeRoot of
    Nothing ->
      return (Left Exit.InstallNoOutline)
    Just root -> Task.run $ do
      env <- Task.eio Exit.InstallBadRegistry $ Solver.initEnv
      oldOutline <- Task.eio Exit.InstallBadOutline $ Outline.read root
      case oldOutline of
        Outline.App outline -> do
          changes <- makeAppPlan env pkg outline
          applyNonInteractive root oldOutline changes
        Outline.Pkg outline -> do
          changes <- makePkgPlan env pkg outline
          applyNonInteractive root oldOutline changes


-- Internal: compute a plan that adds to test-dependencies for application
makeAppPlan :: Solver.Env -> Pkg.Name -> Outline.AppOutline -> Task.Task Exit.Install Changes
makeAppPlan (Solver.Env cache _ connection registry) pkg outline@(Outline.AppOutline _ _ direct indirect testDirect testIndirect) =
  if Map.member pkg testDirect || Map.member pkg testIndirect
    then return AlreadyInstalledPlan
    else do
      -- Try to add dependency to app via solver, but we will place it into test-dependencies
      result <- Task.io $ Solver.addToApp cache connection registry pkg outline
      case result of
        Solver.Ok (Solver.AppSolution _old new _appWithDeps) -> do
          -- Move the newly added package from direct/indirect deps to test deps
          let isNew name = Map.notMember name (Map.union direct indirect)
          let testDirect' = Map.filterWithKey (\k _ -> isNew k) new
          let depsDirect' = Map.filterWithKey (\k _ -> not (isNew k)) new
          let depsTransitive' = Map.difference depsDirect' direct
          let outline' = outline
                { Outline._app_deps_direct = depsDirect'
                , Outline._app_deps_indirect = depsTransitive'
                , Outline._app_test_direct = Map.union testDirect testDirect'
                , Outline._app_test_indirect = Map.difference new (Map.union depsDirect' (Map.union depsTransitive' (Map.union testDirect (Map.union testIndirect testDirect'))))
                }
          return (ChangesPlan (Outline.App outline'))
        Solver.NoSolution -> Task.throw (Exit.InstallNoOnlineAppSolution pkg)
        Solver.NoOfflineSolution -> Task.throw (Exit.InstallNoOfflineAppSolution pkg)
        Solver.Err exit -> Task.throw (Exit.InstallHadSolverTrouble exit)


-- Internal: compute a plan that adds to test-dependencies for package projects
makePkgPlan :: Solver.Env -> Pkg.Name -> Outline.PkgOutline -> Task.Task Exit.Install Changes
makePkgPlan (Solver.Env cache _ connection registry) pkg outline@(Outline.PkgOutline name summary license version exposed deps test elmConstraint) =
  if Map.member pkg test
    then return AlreadyInstalledPlan
    else do
      -- Verify constraints adding the dependency
      let old = Map.union deps test
      let cons = Map.insert pkg Con.anything old
      result <- Task.io $ Solver.verify cache connection registry cons
      case result of
        Solver.Ok solution -> do
          let Solver.Details vsn _ = solution Map.! pkg
          let con = Con.untilNextMajor vsn
          let newAll = Map.insert pkg con old
          let test' = Map.insert pkg con test
          let outline' = Outline.Pkg (Outline.PkgOutline name summary license version exposed deps test' elmConstraint)
          return (ChangesPlan outline')
        Solver.NoSolution -> Task.throw (Exit.InstallNoOnlinePkgSolution pkg)
        Solver.NoOfflineSolution -> Task.throw (Exit.InstallNoOfflinePkgSolution pkg)
        Solver.Err exit -> Task.throw (Exit.InstallHadSolverTrouble exit)


data Changes
  = AlreadyInstalledPlan
  | ChangesPlan Outline.Outline


applyNonInteractive :: FilePath -> Outline.Outline -> Changes -> Task.Task Exit.Install InstallResult
applyNonInteractive root _oldOutline changes =
  case changes of
    AlreadyInstalledPlan -> Task.io (return AlreadyInstalled)
    ChangesPlan newOutline -> do
      Task.eio Exit.InstallBadDetails $ do
        Outline.write root newOutline
        return (Right ())
      Task.io (return SuccessfullyInstalled)


