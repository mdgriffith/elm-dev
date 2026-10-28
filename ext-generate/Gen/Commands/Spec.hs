{-# LANGUAGE OverloadedStrings #-}

module Gen.Commands.Spec (setup) where

import qualified Control.Exception as Exception
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Deps.Solver as Solver
import qualified Elm.Constraint as Con
import qualified Elm.Outline as Outline
import qualified Elm.Package as Pkg
import qualified Ext.DependencyManager as Dependencies
import qualified Ext.Test.Compile as TestCompile
import qualified Ext.Test.Spec as Spec
import qualified Gen.Generate as Generate
import qualified Reporting.Exit as Exit
import qualified System.Directory as Dir
import System.FilePath ((</>))


-- Both CLI and MCP call this function. Installing dependencies is opt-in;
-- ordinary test execution and documentation requests never call it.
setup :: FilePath -> Bool -> IO (Either String String)
setup root installDependencies = do
  result <- Exception.try (setupHelp root installDependencies)
  pure $ case result of
    Left problem -> Left (show (problem :: Exception.IOException))
    Right value -> value

setupHelp :: FilePath -> Bool -> IO (Either String String)
setupHelp root installDependencies = do
  configured <- Spec.sourceDirectories root
  case configured of
    Left problem -> pure (Left problem)
    Right _ -> do
      outline <- Outline.read root
      case outline of
        Left problem -> pure (Left (render (Exit.ReactorBadDetails (Exit.DetailsBadOutline problem))))
        Right (Outline.Pkg _) -> pure (Left "App spec setup requires an application, not a package.")
        Right (Outline.App app) -> do
          let direct = Map.union (Outline._app_deps_direct app) (Outline._app_test_direct app)
              installed = Map.unions [direct, Outline._app_deps_indirect app, Outline._app_test_indirect app]
              required = Spec.requirements
              missing = Map.filterWithKey (\name constraint -> maybe True (not . Con.satisfies constraint) (Map.lookup name installed)) required
              conflicts = Map.filterWithKey (\name constraint -> maybe False (not . Con.satisfies constraint) (Map.lookup name (Outline._app_deps_direct app))) required
          if not (Map.null conflicts) then pure (Left "Bundled spec requirements conflict with production dependencies. Setup will not change production dependency versions.")
          else if not installDependencies && not (Map.null missing) then
            pure (Left ("Spec setup needs these test dependencies:\n" ++ unlines ["  " ++ Pkg.toChars name ++ " " ++ Con.toChars constraint | (name, constraint) <- Map.toList missing]
              ++ "No files were changed. Re-run with --install (MCP: installDependencies: true) to install through the dependency manager."))
          else do
            installed <- if Map.null missing then pure (Right []) else installRequired root app required missing
            case installed of
              Left problem -> pure (Left problem)
              Right installedNames -> do
                generated <- Generate.run root
                case generated of
                  Left problem -> pure (Left problem)
                  Right () -> do
                    hasApp <- Dir.doesFileExist (root </> "elm-stuff/generated/App.elm")
                    if not hasApp then pure (Left "App spec setup requires a generated App.elm.") else do
                      ready <- Spec.readiness root
                      case ready of
                        Left problem -> pure (Left problem)
                        Right () -> do
                          refreshed <- TestCompile.regenerateTestElmJsonPersisted root
                          pure $ case refreshed of
                            Left problem -> Left (render problem)
                            Right () -> Right ("Behavioral specs ready. Spec and Spec.Program are bundled with elm-dev in elm-stuff/generated-tests."
                              ++ "\nApplication source-directories unchanged. "
                              ++ (if null installedNames then "Application elm.json unchanged."
                                  else "Installed test dependencies via the dependency manager: " ++ List.intercalate ", " installedNames)
                              ++ "\nWrite your tests under tests/. See file://guides/behavioral-specs for examples and organization.")

installRequired :: FilePath -> Outline.AppOutline -> Map.Map Pkg.Name Con.Constraint -> Map.Map Pkg.Name Con.Constraint -> IO (Either String [String])
installRequired root app requirements missing = do
  env <- Solver.initEnv
  case env of
    Left problem -> pure (Left (render (Exit.ReactorBadDetails (Exit.DetailsCannotGetRegistry problem))))
    Right (Solver.Env cache _ connection registry) -> do
      let production = Map.map Con.exactly (Map.union (Outline._app_deps_direct app) (Outline._app_deps_indirect app))
          tests = Map.map Con.exactly (Outline._app_test_direct app)
          constraints = Map.union production (Map.union requirements tests)
      solution <- Solver.verify cache connection registry constraints
      case solution of
        Solver.Ok details -> do
          let requests =
                [ Dependencies.PackageRequirement name (Dependencies.Exact version)
                | name <- Map.keys missing
                , Just (Solver.Details version _) <- [Map.lookup name details]
                ]
          planned <- Dependencies.planInstallRequirements root Dependencies.Test requests
          case planned of
            Left problem -> pure (Left ("Cannot plan spec dependencies: " ++ dependencyError problem))
            Right plan -> do
              applied <- Dependencies.applyPlan root plan
              pure $ case applied of
                Left problem -> Left ("Cannot install spec dependencies: " ++ dependencyError problem)
                Right () -> Right (map Pkg.toChars (Map.keys missing))
        Solver.NoSolution -> pure (Left (render (Exit.ReactorBadDetails Exit.DetailsNoSolution)))
        Solver.NoOfflineSolution -> pure (Left (render (Exit.ReactorBadDetails Exit.DetailsNoOfflineSolution)))
        Solver.Err problem -> pure (Left (render (Exit.ReactorBadDetails (Exit.DetailsSolverProblem problem))))

render :: Exit.Reactor -> String
render = Exit.toString . Exit.reactorToReport

dependencyError :: Dependencies.Error -> String
dependencyError problem = case problem of
  Dependencies.RegistryProblem err -> render (Exit.ReactorBadDetails (Exit.DetailsCannotGetRegistry err))
  Dependencies.OutlineProblem err -> render (Exit.ReactorBadDetails (Exit.DetailsBadOutline err))
  Dependencies.NoSolution -> render (Exit.ReactorBadDetails Exit.DetailsNoSolution)
  Dependencies.NoOfflineSolution -> render (Exit.ReactorBadDetails Exit.DetailsNoOfflineSolution)
  Dependencies.SolverProblem err -> render (Exit.ReactorBadDetails (Exit.DetailsSolverProblem err))
  Dependencies.VerificationProblem err -> render (Exit.ReactorBadDetails err)
  Dependencies.TestRefreshProblem err -> render err
  Dependencies.UnsavedChanges -> "elm.json has unsaved changes; save it before installing dependencies."
  Dependencies.StalePlan -> "elm.json changed while planning; retry setup."
  _ -> "The dependency manager rejected the installation; inspect the application's dependency constraints."
