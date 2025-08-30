module Ext.Test.Compile
  ( compileForDiscovery
  , compileRunner
  ) where

import qualified BackgroundWriter
import qualified Control.Concurrent.MVar as MVar
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.NonEmptyList as NE
import qualified Elm.Interface as I
import qualified Elm.ModuleName as ModuleName
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


compileForDiscovery :: FilePath -> NE.List FilePath -> IO (Either Exit.Reactor (Map.Map ModuleName.Raw I.Interface))
compileForDiscovery root entrypoints = do
  BackgroundWriter.withScope $ \scope -> do
    let testRoot = Generate.generatedDir root
    Dir.createDirectoryIfMissing True testRoot
    outlineR <- Outline.read root
    case outlineR of
      Left outlineErr -> pure (Left (Exit.ReactorBadDetails (Exit.DetailsBadOutline outlineErr)))
      Right outline -> do
        case outline of
          Outline.Pkg pkgOutline -> do
            appOutlineR <- pkgOutlineToAppOutline pkgOutline
            case appOutlineR of
              Left details -> pure (Left (Exit.ReactorBadDetails details))
              Right appOutline -> do
                writeTestElmJson testRoot appOutline
                Dir.withCurrentDirectory testRoot $ do
                  let flags = CompileHelpers.Flags CompileHelpers.Dev CompileHelpers.NoOutput
                  let rewrittenEntrypoints = fmap (rewriteEntrypoint root) entrypoints
                  compiledR <- CompileProxy.compile testRoot rewrittenEntrypoints flags
                  case compiledR of
                    Left err -> pure (Left err)
                    Right (_compiled, _info) -> do
                      ifacesR <- CompileProxy.allInterfaces testRoot rewrittenEntrypoints
                      case ifacesR of
                        Left err -> pure (Left err)
                        Right ifaces -> pure (Right ifaces)
          Outline.App appOutline -> do
            writeTestElmJson testRoot appOutline
            Dir.withCurrentDirectory testRoot $ do
              let flags = CompileHelpers.Flags CompileHelpers.Dev CompileHelpers.NoOutput
              let rewrittenEntrypoints = fmap (rewriteEntrypoint root) entrypoints
              compiledR <- CompileProxy.compile testRoot rewrittenEntrypoints flags
              case compiledR of
                Left err -> pure (Left err)
                Right (_compiled, _info) -> do
                  ifacesR <- CompileProxy.allInterfaces testRoot rewrittenEntrypoints
                  case ifacesR of
                    Left err -> pure (Left err)
                    Right ifaces -> pure (Right ifaces)


compileRunner :: FilePath -> NE.List FilePath -> IO (Either Exit.Reactor CompileHelpers.CompilationResult)
compileRunner root entrypoints = do
  BackgroundWriter.withScope $ \scope -> do
    let testRoot = root </> "elm-stuff" </> "elm-dev-test"
    Dir.createDirectoryIfMissing True testRoot
    outlineR <- Outline.read root
    case outlineR of
      Left outlineErr -> pure (Left (Exit.ReactorBadDetails (Exit.DetailsBadOutline outlineErr)))
      Right outline -> do
        case outline of
          Outline.Pkg pkgOutline -> do
            appOutlineR <- pkgOutlineToAppOutline pkgOutline
            case appOutlineR of
              Left details -> pure (Left (Exit.ReactorBadDetails details))
              Right appOutline -> do
                writeTestElmJson testRoot appOutline
                Dir.withCurrentDirectory testRoot $ do
                  let flags = CompileHelpers.Flags CompileHelpers.Dev (CompileHelpers.OutputTo CompileHelpers.Js)
                  let rewrittenEntrypoints = fmap (rewriteEntrypoint root) entrypoints
                  compiledR <- CompileProxy.compile testRoot rewrittenEntrypoints flags
                  case compiledR of
                    Left err -> pure (Left err)
                    Right (result, _info) -> pure (Right result)
          Outline.App appOutline -> do
            writeTestElmJson testRoot appOutline
            Dir.withCurrentDirectory testRoot $ do
              let flags = CompileHelpers.Flags CompileHelpers.Dev (CompileHelpers.OutputTo CompileHelpers.Js)
              let rewrittenEntrypoints = fmap (rewriteEntrypoint root) entrypoints
              compiledR <- CompileProxy.compile testRoot rewrittenEntrypoints flags
              case compiledR of
                Left err -> pure (Left err)
                Right (result, _info) -> pure (Right result)





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
  let mergedDirect = Map.union depsDirect testDirect
  let mergedIndirect = Map.union depsIndirect testIndirect
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



