{-# LANGUAGE OverloadedStrings #-}
module Ext.Install
  ( InstallResult(..)
  , installDependency
  ) where

import qualified Data.Map as Map
import System.FilePath ((</>))
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Builder as BB

import qualified Deps.Solver as Solver
import qualified Elm.Constraint as Con
import qualified Elm.Outline as Outline
import qualified Elm.Package as Pkg
import qualified Json.Encode as JE
import qualified Reporting.Exit as Exit
import qualified Reporting.Task as Task
import qualified Stuff
import qualified Ext.FileProxy


data InstallResult
  = SuccessfullyInstalled
  | AlreadyInstalled
  deriving (Eq, Show)


-- | Non-interactive installer for normal dependencies (like `elm install`),
-- writing to both the virtual FS and the physical disk.
installDependency :: Pkg.Name -> IO (Either Exit.Install InstallResult)
installDependency pkg = do
  maybeRoot <- Stuff.findRoot
  case maybeRoot of
    Nothing ->
      return (Left Exit.InstallNoOutline)
    Just root -> Task.run $ do
      env <- Task.eio Exit.InstallBadRegistry $ Solver.initEnv
      oldOutline <- Task.eio Exit.InstallBadOutline $ Outline.read root
      case oldOutline of
        Outline.App outline -> do
          let direct = Outline._app_deps_direct outline
          if Map.member pkg direct
            then Task.io (return AlreadyInstalled)
            else do
              let Solver.Env cache _ connection registry = env
              addResult <- Task.io $ Solver.addToApp cache connection registry pkg outline
              case addResult of
                Solver.Ok (Solver.AppSolution _ _ app') -> do
                  let newOutline = Outline.App app'
                  Task.eio Exit.InstallBadDetails $ do
                    let path = root </> "elm.json"
                    JE.write path (Outline.encode newOutline)
                    let builder = JE.encode (Outline.encode newOutline) <> BB.char7 '\n'
                    Ext.FileProxy.writeUtf8AllTheWayToDisk path (LBS.toStrict (BB.toLazyByteString builder))
                    return (Right ())
                  Task.io (return SuccessfullyInstalled)
                Solver.NoSolution ->
                  Task.throw (Exit.InstallNoOnlineAppSolution pkg)
                Solver.NoOfflineSolution ->
                  Task.throw (Exit.InstallNoOfflineAppSolution pkg)
                Solver.Err exit ->
                  Task.throw (Exit.InstallHadSolverTrouble exit)

        Outline.Pkg (Outline.PkgOutline name summary license version exposed deps test elmConstraint) -> do
          if Map.member pkg deps
            then Task.io (return AlreadyInstalled)
            else do
              let Solver.Env cache _ connection registry = env
              let old = Map.union deps test
              let cons = Map.insert pkg Con.anything old
              verifyResult <- Task.io $ Solver.verify cache connection registry cons
              case verifyResult of
                Solver.Ok solution -> do
                  let Solver.Details vsn _ = solution Map.! pkg
                  let con = Con.untilNextMajor vsn
                  let deps' = Map.insert pkg con deps
                  let newOutline = Outline.Pkg (Outline.PkgOutline name summary license version exposed deps' test elmConstraint)
                  Task.eio Exit.InstallBadDetails $ do
                    let path = root </> "elm.json"
                    JE.write path (Outline.encode newOutline)
                    let builder = JE.encode (Outline.encode newOutline) <> BB.char7 '\n'
                    Ext.FileProxy.writeUtf8AllTheWayToDisk path (LBS.toStrict (BB.toLazyByteString builder))
                    return (Right ())
                  Task.io (return SuccessfullyInstalled)
                Solver.NoSolution ->
                  Task.throw (Exit.InstallNoOnlinePkgSolution pkg)
                Solver.NoOfflineSolution ->
                  Task.throw (Exit.InstallNoOfflinePkgSolution pkg)
                Solver.Err exit ->
                  Task.throw (Exit.InstallHadSolverTrouble exit)



