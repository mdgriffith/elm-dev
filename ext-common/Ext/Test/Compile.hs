module Ext.Test.Compile
  ( compileRunner
  , compile
  , regenerateTestElmJson
  , regenerateTestElmJsonPersisted
  , prepare
  ) where

import qualified BackgroundWriter
import qualified Data.Aeson as JSON
import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.NonEmptyList as NE
import qualified Data.Set as Set
import qualified Ext.FileProxy as File
import qualified Elm.Package as Pkg
import qualified Elm.Outline as Outline
import qualified Elm.Version as V
import qualified Elm.Constraint as Con
import qualified Ext.CompileHelpers.Generic as CompileHelpers
import qualified Ext.CompileProxy as CompileProxy
import qualified Reporting.Exit as Exit
import qualified System.Directory as Dir
import System.FilePath ((</>))
import qualified System.FilePath as FP
import qualified Deps.Solver as Solver
import qualified Ext.Test.Generate as Generate
import qualified Ext.Test.Spec as Spec
import qualified Control.Exception as Exception
import qualified System.Environment as Env
import qualified System.Exit as SystemExit
import qualified System.IO as IO
import qualified System.IO.Temp as Temp
import qualified System.Process as Process


-- Ensure a test elm.json exists under root/elm-stuff/elm-dev-test based on project's elm.json.
-- Uses FileProxy-backed IO via Outline.read/write underneath.
generateTestJsonIfNone :: FilePath -> IO (Either Exit.Reactor ())
generateTestJsonIfNone root = do
  let testRoot = Generate.generatedDir root
  Dir.createDirectoryIfMissing True testRoot
  let testElmJsonPath = testRoot </> "elm.json"
  exists <- Dir.doesFileExist testElmJsonPath
  inputs <- testManifestInputs root
  let inputsPath = testRoot </> "inputs.hash"
  hasInputs <- Dir.doesFileExist inputsPath
  previous <- if hasInputs then BS.readFile inputsPath else pure BS.empty
  let stale = not exists || inputs /= previous
  if stale then regenerateTestElmJsonPersisted root else pure (Right ())


-- Include absence/presence as well as contents so removing local-source config
-- or creating the generated test directory also refreshes an existing manifest.
testManifestInputs :: FilePath -> IO BS.ByteString
testManifestInputs root = do
  manifest <- File.readUtf8 (root </> "elm.json")
  configured <- Dir.doesFileExist (root </> "elm.spec.json")
  spec <- if configured then File.readUtf8 (root </> "elm.spec.json") else pure BS.empty
  resolved <- Spec.sourceDirectories root >>= either (ioError . userError) pure
  generated <- Dir.doesDirectoryExist (root </> "elm-stuff/generated-tests")
  let contract = [(Pkg.toChars name, Con.toChars constraint) | (name, constraint) <- Map.toList Spec.requirements]
  pure (BS8.pack (SHA.showDigest (SHA.sha1
    (LBS.fromStrict manifest <> JSON.encode (configured, generated, resolved, contract) <> LBS.fromStrict spec))))

-- Always (re)generate the test elm.json under root/elm-stuff/elm-dev-test from project's elm.json at root.
regenerateTestElmJson :: FilePath -> IO (Either Exit.Reactor ())
regenerateTestElmJson root = do
  let testRoot = Generate.generatedDir root
  Dir.createDirectoryIfMissing True testRoot
  outlineR <- Outline.read root
  case outlineR of
    Left outlineErr ->
      pure (Left (Exit.ReactorBadDetails (Exit.DetailsBadOutline outlineErr)))
    Right outline -> do
      case outline of
        Outline.Pkg pkgOutline -> do
          appOutlineR <- pkgOutlineToAppOutline pkgOutline
          case appOutlineR of
            Left details -> pure (Left (Exit.ReactorBadDetails details))
            Right appOutline -> do
              writeTestElmJson testRoot appOutline
              pure (Right ())
        Outline.App appOutline -> do
          writeTestElmJson testRoot appOutline
          pure (Right ())


regenerateTestElmJsonPersisted :: FilePath -> IO (Either Exit.Reactor ())
regenerateTestElmJsonPersisted root = do
  result <- regenerateTestElmJson root
  case result of
    Left problem -> pure (Left problem)
    Right () -> do
      let path = Generate.generatedDir root </> "elm.json"
      bytes <- File.readUtf8 path
      File.writeUtf8AllTheWayToDisk path bytes
      inputs <- testManifestInputs root
      File.writeUtf8AllTheWayToDisk (Generate.generatedDir root </> "inputs.hash") inputs
      pure (Right ())


-- For getting compilation status quickly
compile :: FilePath -> NE.List FilePath -> IO (Either Exit.Reactor ())
compile root entrypoints = do
  BackgroundWriter.withScope $ \scope -> do
    let testRoot = Generate.generatedDir root
    genR <- generateTestJsonIfNone root
    case genR of
      Left err -> pure (Left err)
      Right () -> do
        Dir.withCurrentDirectory testRoot $ do
          let flags = CompileHelpers.Flags CompileHelpers.Dev CompileHelpers.NoOutput CompileHelpers.DebuggerNone
          let rewrittenEntrypoints = fmap (rewriteEntrypoint root) entrypoints
          (eitherCompiled, _info) <- CompileProxy.compile testRoot rewrittenEntrypoints flags Nothing
          case eitherCompiled of
            Left err -> pure (Left err)
            Right _ -> pure (Right ())


compileRunner :: FilePath -> NE.List FilePath -> IO (Either String BS.ByteString)
compileRunner root entrypoints = do
  let testRoot = Generate.generatedDir root
  genR <- prepare root (NE.toList entrypoints)
  case genR of
    Left err -> pure (Left err)
    Right () ->
      Temp.withSystemTempFile "elm-dev-test-runner.js" $ \outputPath outputHandle -> do
        IO.hClose outputHandle
        executable <- Env.getExecutablePath
        environment <- Env.getEnvironment
        compilerRts <- Env.lookupEnv "ELM_DEV_TEST_COMPILER_GHCRTS"
        let relativeEntrypoints = map (FP.makeRelative testRoot) (NE.toList entrypoints)
            -- Compilation of a whole app plus program-test needs more heap than
            -- the lightweight test worker. Do not inherit its 1GiB runtime cap.
            -- This is an isolated batch compiler with persisted inputs, not the
            -- editor's in-memory analysis session. Reuse disk build artifacts.
            compilerEnvironment = [("GHCRTS", maybe "-M2048m -N2" id compilerRts), ("MODE", "disk")]
              ++ filter (\(key, _) -> key /= "GHCRTS" && key /= "MODE") environment
            process =
              (Process.proc executable ("make" : relativeEntrypoints ++ ["--output=" ++ outputPath]))
                 { Process.cwd = Just testRoot
                 , Process.std_in = Process.NoStream
                 , Process.env = Just compilerEnvironment
                }
        (exitCode, stdout, stderr) <- Process.readCreateProcessWithExitCode process ""
        case exitCode of
          SystemExit.ExitSuccess -> Right <$> BS.readFile outputPath
          SystemExit.ExitFailure code ->
            let details = if null stderr then stdout else stderr
            in pure (Left ("Test runner compilation exited with code " ++ show code ++ if null details then "" else ":\n" ++ details))





-- Shared by CLI, MCP test execution, and test-context documentation.
prepare :: FilePath -> [FilePath] -> IO (Either String ())
prepare root paths = do
  checked <- Spec.checkEntrypoints root paths
  case checked of
    Left problem -> pure (Left problem)
    Right () -> do
      result <- Exception.try (generateTestJsonIfNone root)
      pure $ case result of
        Left problem -> Left (show (problem :: Exception.IOException))
        Right (Left problem) -> Left (Exit.toString (Exit.reactorToReport problem))
        Right (Right ()) -> Right ()


-- Write only the derived manifest. Never mutate root/elm.json here.
writeTestElmJson :: FilePath -> Outline.AppOutline -> IO ()
writeTestElmJson testRoot (Outline.AppOutline _ srcDirs depsDirect depsIndirect testDirect testIndirect) = do
  let root = FP.takeDirectory (FP.takeDirectory testRoot)
  configured <- Spec.sourceDirectories root
  extraSources <- either (ioError . userError) pure configured
  generatedTests <- Dir.doesDirectoryExist (root </> "elm-stuff/generated-tests")
  let rewriteSrcDir srcDir =
        case srcDir of
          Outline.AbsoluteSrcDir dir -> Outline.AbsoluteSrcDir dir
          Outline.RelativeSrcDir dir -> Outline.RelativeSrcDir ("../../" </> dir)
  let candidates = fmap rewriteSrcDir (NE.toList srcDirs)
                       ++ [ Outline.RelativeSrcDir ("../../" </> "tests")
                          , Outline.RelativeSrcDir (".")
                          ]
                       ++ (if generatedTests then [Outline.RelativeSrcDir "../generated-tests"] else [])
                       ++ map Outline.AbsoluteSrcDir extraSources
  identities <- traverse (Dir.canonicalizePath . Outline.toAbsolute testRoot) candidates
  let newSrcDirsList = map fst (List.nubBy (\a b -> snd a == snd b) (zip candidates identities))
  let newSrcDirs = case newSrcDirsList of
        [] -> error "No source directories found in elm.json"
        (d:ds) -> NE.List d ds
  let randomPkg = Pkg.toName Pkg.elm "random"
  -- Build constraints from existing pinned versions and ask the solver to include json/random.
  let allPinned = Map.unions [depsDirect, depsIndirect, testDirect, testIndirect]
  let baseConstraints = Map.map Con.exactly allPinned
  let constraintsWithRequired =
        Map.insertWith (\_ pinned -> pinned) randomPkg Con.anything $
        Map.insertWith (\_ pinned -> pinned) Pkg.json Con.anything baseConstraints
      directNames = Set.fromList (Map.keys depsDirect ++ Map.keys testDirect ++ Map.keys Spec.requirements ++ [Pkg.json, randomPkg])
      writeResolved solutionVersions = do
        let newDirect = Map.filterWithKey (\name _ -> Set.member name directNames) solutionVersions
            newIndirect = Map.difference solutionVersions newDirect
        Outline.write testRoot (Outline.App (Outline.AppOutline V.compiler newSrcDirs newDirect newIndirect Map.empty Map.empty))
  -- Normal applications already carry a fully pinned dependency closure. Reuse
  -- it exactly; a test run should not resolve a new version of an installed package.
  if all (`Map.member` allPinned) [Pkg.json, randomPkg] then writeResolved allPinned
  else do
    envR <- Solver.initEnv
    case envR of
      Left problem -> failDetails (Exit.DetailsCannotGetRegistry problem)
      Right (Solver.Env cache _ connection registry) -> do
        verifyR <- Solver.verify cache connection registry constraintsWithRequired
        case verifyR of
          Solver.Ok detailsMap -> writeResolved (Map.map (\(Solver.Details version _) -> version) detailsMap)
          Solver.NoSolution -> failDetails Exit.DetailsNoSolution
          Solver.NoOfflineSolution -> failDetails Exit.DetailsNoOfflineSolution
          Solver.Err problem -> failDetails (Exit.DetailsSolverProblem problem)
  where
    failDetails problem = ioError (userError (Exit.toString (Exit.reactorToReport (Exit.ReactorBadDetails problem))))


-- Convert a package outline to an application outline with pinned dependency versions
pkgOutlineToAppOutline :: Outline.PkgOutline -> IO (Either Exit.Details Outline.AppOutline)
pkgOutlineToAppOutline (Outline.PkgOutline _name _summary _license _version _exposed deps testDeps elmConstraint) = do
  if Con.goodElm elmConstraint
    then do
      envR <- Solver.initEnv
      case envR of
        Left regProblem ->
          pure (Left (Exit.DetailsCannotGetRegistry regProblem))
        Right (Solver.Env cache _ connection registry) -> do
          let allConstraints = Map.union deps testDeps
          verifyR <- Solver.verify cache connection registry allConstraints
          case verifyR of
            Solver.Ok detailsMap -> do
              let solution = Map.map (\(Solver.Details vsn _) -> vsn) detailsMap
              let direct = Map.intersection solution deps
              let testDirectAll = Map.intersection solution testDeps
              let testDirect = Map.difference testDirectAll direct
              let indirect = Map.difference (Map.difference solution direct) testDirect
              let testIndirect = Map.empty
              let srcDirs = NE.List (Outline.RelativeSrcDir "src") []
              let appOutline = Outline.AppOutline V.compiler srcDirs direct indirect testDirect testIndirect
              pure (Right appOutline)
            Solver.NoSolution -> pure (Left Exit.DetailsNoSolution)
            Solver.NoOfflineSolution -> pure (Left Exit.DetailsNoOfflineSolution)
            Solver.Err exit -> pure (Left (Exit.DetailsSolverProblem exit))
    else
      pure (Left (Exit.DetailsBadElmInPkg elmConstraint))


rewriteEntrypoint :: FilePath -> FilePath -> FilePath
rewriteEntrypoint root path =
  let rel = FP.makeRelative root path
  in ("../../" </> rel)
