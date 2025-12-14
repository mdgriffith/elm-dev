module Ext.Test.Compile
  ( compileForDiscovery
  , compileRunner
  , compile
  , regenerateTestElmJson
  ) where

import qualified BackgroundWriter
import qualified Control.Concurrent.MVar as MVar
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.NonEmptyList as NE
import           Control.Applicative ((<|>))
import qualified Data.Set as Set
import qualified Ext.FileProxy as File
import qualified Elm.Interface as I
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified Elm.Outline as Outline
import qualified Elm.Version as V
import qualified Elm.Constraint as Con
import qualified Ext.CompileHelpers.Generic as CompileHelpers
import qualified Ext.CompileProxy as CompileProxy
import qualified Reporting
import qualified Reporting.Exit as Exit
import qualified System.Directory as Dir
import System.FilePath ((</>))
import qualified System.FilePath as FP
import qualified Deps.Solver as Solver
import qualified Ext.Test.Generate as Generate


-- Ensure a test elm.json exists under root/elm-stuff/elm-dev-test based on project's elm.json.
-- Uses FileProxy-backed IO via Outline.read/write underneath.
generateTestJsonIfNone :: FilePath -> IO (Either Exit.Reactor ())
generateTestJsonIfNone root = do
  let testRoot = Generate.generatedDir root
  Dir.createDirectoryIfMissing True testRoot
  let testElmJsonPath = testRoot </> "elm.json"
  exists <- Dir.doesFileExist testElmJsonPath
  if exists
    then pure (Right ())
    else regenerateTestElmJson root

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


compileForDiscovery :: FilePath -> NE.List FilePath -> IO (Either Exit.Reactor (Map.Map ModuleName.Raw I.Interface))
compileForDiscovery root entrypoints = do
  BackgroundWriter.withScope $ \scope -> do
    let testRoot = Generate.generatedDir root
    genR <- generateTestJsonIfNone root
    case genR of
      Left err -> pure (Left err)
      Right () -> do
        Dir.withCurrentDirectory testRoot $ do
          let flags = CompileHelpers.Flags CompileHelpers.Dev CompileHelpers.NoOutput
          let rewrittenEntrypoints = fmap (rewriteEntrypoint root) entrypoints
          (eitherCompiled, _info) <- CompileProxy.compile testRoot rewrittenEntrypoints flags Nothing
          case eitherCompiled of
            Left err -> pure (Left err)
            Right _compiled -> do
              ifacesR <- CompileProxy.allInterfaces testRoot rewrittenEntrypoints
              case ifacesR of
                Left err -> pure (Left err)
                Right ifaces -> pure (Right ifaces)

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
          let flags = CompileHelpers.Flags CompileHelpers.Dev (CompileHelpers.NoOutput)
          let rewrittenEntrypoints = fmap (rewriteEntrypoint root) entrypoints
          (eitherCompiled, _info) <- CompileProxy.compile testRoot rewrittenEntrypoints flags Nothing
          case eitherCompiled of
            Left err -> pure (Left err)
            Right _ -> pure (Right ())


compileRunner :: FilePath -> NE.List FilePath -> IO (Either Exit.Reactor CompileHelpers.CompilationResult)
compileRunner root entrypoints = do
  BackgroundWriter.withScope $ \scope -> do
    let testRoot = Generate.generatedDir root
    genR <- generateTestJsonIfNone root
    case genR of
      Left err -> pure (Left err)
      Right () -> do
        Dir.withCurrentDirectory testRoot $ do
          let flags = CompileHelpers.Flags CompileHelpers.Dev (CompileHelpers.OutputTo CompileHelpers.Js)
          let rewrittenEntrypoints = fmap (rewriteEntrypoint root) entrypoints
          (eitherCompiled, _info) <- CompileProxy.compile testRoot rewrittenEntrypoints flags Nothing
          case eitherCompiled of
            Left err -> pure (Left err)
            Right result -> pure (Right result)





-- Write a rewritten elm.json for tests under testRoot
writeTestElmJson :: FilePath -> Outline.AppOutline -> IO ()
writeTestElmJson testRoot (Outline.AppOutline _ srcDirs depsDirect depsIndirect testDirect testIndirect) = do
  let rewriteSrcDir srcDir =
        case srcDir of
          Outline.AbsoluteSrcDir dir -> Outline.AbsoluteSrcDir dir
          Outline.RelativeSrcDir dir -> Outline.RelativeSrcDir ("../../" </> dir)
  let newSrcDirsList = fmap rewriteSrcDir (NE.toList srcDirs)
                      ++ [ Outline.RelativeSrcDir ("../../" </> "tests")
                         , Outline.RelativeSrcDir (".")
                         ]
  let newSrcDirs = case newSrcDirsList of
        [] -> error "No source directories found in elm.json"
        (d:ds) -> NE.List d ds
  let randomPkg = Pkg.toName Pkg.elm "random"
  -- Build constraints from existing pinned versions and ask the solver to include json/random.
  let allPinned = Map.unions [depsDirect, depsIndirect, testDirect, testIndirect]
  let baseConstraints = Map.map Con.exactly allPinned
  let constraintsWithRequired =
        Map.insert randomPkg Con.anything $
        Map.insert Pkg.json Con.anything baseConstraints
  envR <- Solver.initEnv
  case envR of
    Left _regProblem -> do
      -- Fallback: keep original deps layout (still includes json requirement check elsewhere)
      let mergedDirect0 = Map.union depsDirect testDirect
      let mergedIndirect = Map.union depsIndirect testIndirect
      let jsonVersion =
            Maybe.fromMaybe (V.Version 1 1 3)
              ( Map.lookup Pkg.json mergedDirect0
                <|> Map.lookup Pkg.json mergedIndirect
              )
      let randomVersion =
            Maybe.fromMaybe V.one
              ( Map.lookup randomPkg mergedDirect0
                <|> Map.lookup randomPkg mergedIndirect
              )
      let mergedDirect =
            Map.insert Pkg.json jsonVersion
            (Map.insert randomPkg randomVersion mergedDirect0)
      let newOutline = Outline.App (Outline.AppOutline V.compiler newSrcDirs mergedDirect mergedIndirect Map.empty Map.empty)
      Outline.write testRoot newOutline
    Right (Solver.Env cache _ connection registry) -> do
      verifyR <- Solver.verify cache connection registry constraintsWithRequired
      case verifyR of
        Solver.Ok detailsMap -> do
          let solutionVersions = Map.map (\(Solver.Details v _) -> v) detailsMap
          -- Put test dependencies into normal dependencies for the test elm.json
          let directNames =
                Set.unions
                  [ Set.fromList (Map.keys depsDirect)
                  , Set.fromList (Map.keys testDirect)
                  , Set.fromList [Pkg.json, randomPkg]
                  ]
          let newDirect = Map.filterWithKey (\k _ -> Set.member k directNames) solutionVersions
          let newIndirect = Map.difference solutionVersions newDirect
          let newOutline = Outline.App (Outline.AppOutline V.compiler newSrcDirs newDirect newIndirect Map.empty Map.empty)
          Outline.write testRoot newOutline
        Solver.NoSolution -> do
          -- Fallback to previous approach if solver cannot find a solution
          let mergedDirect0 = Map.union depsDirect testDirect
          let mergedIndirect = Map.union depsIndirect testIndirect
          let jsonVersion =
                Maybe.fromMaybe (V.Version 1 1 3)
                  ( Map.lookup Pkg.json mergedDirect0
                    <|> Map.lookup Pkg.json mergedIndirect
                  )
          let randomVersion =
                Maybe.fromMaybe V.one
                  ( Map.lookup randomPkg mergedDirect0
                    <|> Map.lookup randomPkg mergedIndirect
                  )
          let mergedDirect =
                Map.insert Pkg.json jsonVersion
                (Map.insert randomPkg randomVersion mergedDirect0)
          let newOutline = Outline.App (Outline.AppOutline V.compiler newSrcDirs mergedDirect mergedIndirect Map.empty Map.empty)
          Outline.write testRoot newOutline
        Solver.NoOfflineSolution -> do
          -- Same fallback as above
          let mergedDirect0 = Map.union depsDirect testDirect
          let mergedIndirect = Map.union depsIndirect testIndirect
          let jsonVersion =
                Maybe.fromMaybe (V.Version 1 1 3)
                  ( Map.lookup Pkg.json mergedDirect0
                    <|> Map.lookup Pkg.json mergedIndirect
                  )
          let randomVersion =
                Maybe.fromMaybe V.one
                  ( Map.lookup randomPkg mergedDirect0
                    <|> Map.lookup randomPkg mergedIndirect
                  )
          let mergedDirect =
                Map.insert Pkg.json jsonVersion
                (Map.insert randomPkg randomVersion mergedDirect0)
          let newOutline = Outline.App (Outline.AppOutline V.compiler newSrcDirs mergedDirect mergedIndirect Map.empty Map.empty)
          Outline.write testRoot newOutline
        Solver.Err _exit -> do
          -- Same fallback as above
          let mergedDirect0 = Map.union depsDirect testDirect
          let mergedIndirect = Map.union depsIndirect testIndirect
          let jsonVersion =
                Maybe.fromMaybe (V.Version 1 1 3)
                  ( Map.lookup Pkg.json mergedDirect0
                    <|> Map.lookup Pkg.json mergedIndirect
                  )
          let randomVersion =
                Maybe.fromMaybe V.one
                  ( Map.lookup randomPkg mergedDirect0
                    <|> Map.lookup randomPkg mergedIndirect
                  )
          let mergedDirect =
                Map.insert Pkg.json jsonVersion
                (Map.insert randomPkg randomVersion mergedDirect0)
          let newOutline = Outline.App (Outline.AppOutline V.compiler newSrcDirs mergedDirect mergedIndirect Map.empty Map.empty)
          Outline.write testRoot newOutline


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



