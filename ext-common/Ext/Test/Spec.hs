{-# LANGUAGE OverloadedStrings #-}

-- Read-only spec configuration and readiness. This module never installs a
-- dependency or writes an application manifest.
module Ext.Test.Spec
  ( sourceDirectories, canonicalSources, requirements, readiness
  , checkEntrypoints, resolveModule, applicationSources, programTestPackage
  ) where

import qualified AST.Source as Src
import qualified Control.Monad as Monad
import qualified Data.Aeson as JSON
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Name as Name
import qualified Data.NonEmptyList as NE
import qualified Data.Set as Set
import qualified Data.Utf8 as Utf8
import qualified Elm.Constraint as Con
import qualified Elm.Outline as Outline
import qualified Elm.Package as Pkg
import qualified Elm.Version as V
import qualified Ext.FileProxy as File
import qualified Parse.Module as Parse
import qualified Reporting.Exit as Exit
import qualified System.Directory as Dir
import qualified System.FilePath as FP
import System.FilePath ((</>))


programTestPackage :: Pkg.Name
programTestPackage = Pkg.toName (Utf8.fromChars "avh4") "elm-program-test"

-- Dependencies of the Spec modules bundled with this elm-dev executable.
-- The ordinary browser/HTTP/time packages are already required by generated apps.
requirements :: Map.Map Pkg.Name Con.Constraint
requirements = Map.fromList
  [ (programTestPackage, Con.untilNextMajor (V.Version 4 0 1))
  , (Pkg.toName (Utf8.fromChars "elm-explorations") "test", Con.untilNextMajor (V.Version 2 2 0))
  ]

-- Test-only directories are compared by physical identity, including symlinks.
canonicalSources :: [FilePath] -> IO [FilePath]
canonicalSources paths = List.nub <$> traverse Dir.canonicalizePath paths

sourceDirectories :: FilePath -> IO (Either String [FilePath])
sourceDirectories root = do
  let path = root </> "elm.spec.json"
  exists <- Dir.doesFileExist path
  if not exists then pure (Right []) else do
    bytes <- File.readUtf8 path
    case JSON.eitherDecodeStrict bytes :: Either String (Map.Map String [FilePath]) of
      Left problem -> pure (Left ("Invalid " ++ path ++ ": " ++ problem))
      Right config -> do
        let paths = map (absolute root) (Map.findWithDefault [] "source-directories" config)
        missing <- Monad.filterM (fmap not . Dir.doesDirectoryExist) paths
        if not (null missing) then pure (Left ("Missing test source directories: " ++ List.intercalate ", " missing))
        else do
          resolved <- canonicalSources paths
          bundled <- Dir.canonicalizePath (root </> "elm-stuff/generated-tests")
          conflicts <- Monad.filterM (\dir -> do
            core <- Dir.doesFileExist (dir </> "Spec.elm")
            runtime <- Dir.doesFileExist (dir </> "Spec/Program.elm")
            pure ((core || runtime) && dir /= bundled)) resolved
          pure $ if null conflicts then Right resolved
            else Left ("Spec and Spec.Program are bundled with elm-dev. Remove obsolete external Spec source links from elm.spec.json: " ++ List.intercalate ", " conflicts)

absolute :: FilePath -> FilePath -> FilePath
absolute root path = if FP.isAbsolute path then path else root </> path

applicationSources :: FilePath -> IO (Either String [FilePath])
applicationSources root = do
  result <- Outline.read root
  pure $ case result of
    Left problem -> Left (Exit.toString (Exit.reactorToReport (Exit.ReactorBadDetails (Exit.DetailsBadOutline problem))))
    Right (Outline.App app) -> Right (map (Outline.toAbsolute root) (NE.toList (Outline._app_source_dirs app)))
    Right (Outline.Pkg _) -> Right [root </> "src"]

readiness :: FilePath -> IO (Either String ())
readiness root = do
  sources <- sourceDirectories root
  outline <- Outline.read root
  case (sources, outline) of
    (Left problem, _) -> pure (Left problem)
    (_, Left problem) -> pure (Left (Exit.toString (Exit.reactorToReport (Exit.ReactorBadDetails (Exit.DetailsBadOutline problem)))))
    (Right dirs, Right (Outline.App app)) -> do
      let direct = Map.union (Outline._app_deps_direct app) (Outline._app_test_direct app)
          installed = Map.unions [direct, Outline._app_deps_indirect app, Outline._app_test_indirect app]
      bundled <- Dir.canonicalizePath (root </> "elm-stuff/generated-tests")
      conflicts <- Monad.filterM (\dir -> do
        ownsSpec <- Dir.doesFileExist (dir </> "Spec.elm")
        pure (ownsSpec && dir /= bundled)) dirs
      files <- traverse Dir.doesFileExist [bundled </> "Spec.elm", bundled </> "Spec/Program.elm"]
      let missing =
            [ Pkg.toChars package ++ " (requires " ++ Con.toChars constraint ++ maybe "; missing" (("; installed " ++) . V.toChars) (Map.lookup package installed) ++ ")"
            | (package, constraint) <- Map.toList requirements
            , maybe True (not . Con.satisfies constraint) (Map.lookup package installed)
            ]
          problems =
            (if null conflicts then [] else ["Spec is bundled with elm-dev. Remove external Spec source links from elm.spec.json: " ++ List.intercalate ", " conflicts])
            ++ (if and files then [] else ["Bundled Spec and Spec.Program support needs generation."])
            ++ (if null missing then [] else ["Missing or incompatible spec dependencies:\n  " ++ List.intercalate "\n  " missing])
      pure (if null problems then Right () else Left (guidance root (List.intercalate "\n" problems)))
    (_, Right (Outline.Pkg _)) -> pure (Right ())

guidance :: FilePath -> String -> String
guidance root problem =
  "Behavioral spec setup is incomplete for " ++ root ++ ":\n" ++ problem
    ++ "\nRun explicit setup: elm-dev test init --install"
    ++ "\nMCP: add { kind: \"spec\", installDependencies: true }."
    ++ "\nSpec and Spec.Program are supplied by elm-dev; no elm-spec package or sibling checkout is needed."
    ++ "\nThis check has not changed the application's elm.json. Do not add generated-tests to its source-directories."

resolveModule :: [FilePath] -> Name.Name -> IO (Maybe FilePath)
resolveModule sources name = do
  let relative = map (\c -> if c == '.' then FP.pathSeparator else c) (Name.toChars name) ++ ".elm"
  candidates <- Monad.filterM Dir.doesFileExist (map (</> relative) sources)
  pure $ case candidates of
    path:_ -> Just path
    [] -> Nothing

-- Walk only the selected entrypoints' local import graph. Ordinary unit tests
-- do not acquire a spec dependency just because the generator emitted a harness.
checkEntrypoints :: FilePath -> [FilePath] -> IO (Either String ())
checkEntrypoints root paths = do
  configured <- sourceDirectories root
  production <- applicationSources root
  case (configured, production) of
    (Left problem, _) -> pure (Left problem)
    (_, Left problem) -> pure (Left problem)
    (Right extra, Right sources) -> do
      candidate <- readiness root
      let alreadyReady = Just candidate
      case alreadyReady of
        Just (Right ()) -> pure (Right ())
        _ -> do
          let allSources = [root </> "tests", root </> "elm-stuff/generated-tests", root </> "elm-stuff/elm-dev-test"] ++ extra ++ sources
          usesSpec <- walk allSources Set.empty paths
          if usesSpec then maybe (readiness root) pure alreadyReady else pure (Right ())
  where
    walk _ _ [] = pure False
    walk sources visited (path:remaining) = do
      canonical <- Dir.canonicalizePath path
      if Set.member canonical visited then walk sources visited remaining else do
        exists <- Dir.doesFileExist canonical
        if not exists then walk sources (Set.insert canonical visited) remaining else do
          bytes <- File.readUtf8 canonical
          case Parse.fromByteString Parse.Application bytes of
            Left _ -> walk sources (Set.insert canonical visited) remaining -- compiler reports syntax errors
            Right modul -> do
              let imports = map Src.getImportName (Src._imports modul)
                  isSpec name = Name.toChars name `elem` ["Spec", "Spec.Program", "App.Spec"]
              if any isSpec imports then pure True else do
                resolved <- traverse (resolveModule sources) imports
                walk sources (Set.insert canonical visited) ([file | Just file <- resolved] ++ remaining)
