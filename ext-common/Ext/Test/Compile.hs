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
import qualified Ext.CompileHelpers.Generic as CompileHelpers
import qualified Ext.CompileProxy as CompileProxy
import qualified Reporting
import qualified Reporting.Exit as Exit
import qualified System.Directory as Dir
import System.FilePath ((</>))
import qualified System.FilePath as FP


compileForDiscovery :: FilePath -> [FilePath] -> IO (Either Exit.Reactor (Map.Map ModuleName.Raw I.Interface))
compileForDiscovery root entrypoints = do
  BackgroundWriter.withScope $ \scope -> do
    let testRoot = root </> "elm-stuff" </> "elm-dev-test"
    Dir.createDirectoryIfMissing True testRoot
    outlineR <- Outline.read root
    case outlineR of
      Left outlineErr -> pure (Left (Exit.ReactorBadDetails (Exit.DetailsBadOutline outlineErr)))
      Right outline -> do
        case outline of
          Outline.Pkg _ ->
            -- Not yet supported: compiling tests for packages via generated app outline
            pure (Left (Exit.ReactorBadDetails Exit.DetailsHandEditedDependencies))
          Outline.App appOutline -> do
            writeTestElmJson testRoot appOutline
            Dir.withCurrentDirectory testRoot $ do
              let flags = CompileHelpers.Flags CompileHelpers.Dev CompileHelpers.NoOutput
              let rewrittenEntrypoints = fmap (rewriteEntrypoint root) entrypoints
              compiledR <- CompileProxy.compile testRoot (toNonEmpty rewrittenEntrypoints) flags
              case compiledR of
                Left err -> pure (Left err)
                Right (_compiled, _info) -> do
                  ifacesR <- CompileProxy.allInterfaces testRoot (toNonEmpty rewrittenEntrypoints)
                  case ifacesR of
                    Left err -> pure (Left err)
                    Right ifaces -> pure (Right ifaces)


compileRunner :: FilePath -> [FilePath] -> IO (Either Exit.Reactor CompileHelpers.CompilationResult)
compileRunner root entrypoints = do
  BackgroundWriter.withScope $ \scope -> do
    let testRoot = root </> "elm-stuff" </> "elm-dev-test"
    Dir.createDirectoryIfMissing True testRoot
    outlineR <- Outline.read root
    case outlineR of
      Left outlineErr -> pure (Left (Exit.ReactorBadDetails (Exit.DetailsBadOutline outlineErr)))
      Right outline -> do
        case outline of
          Outline.Pkg _ ->
            pure (Left (Exit.ReactorBadDetails Exit.DetailsHandEditedDependencies))
          Outline.App appOutline -> do
            writeTestElmJson testRoot appOutline
            Dir.withCurrentDirectory testRoot $ do
              let flags = CompileHelpers.Flags CompileHelpers.Dev (CompileHelpers.OutputTo CompileHelpers.Js)
              let rewrittenEntrypoints = fmap (rewriteEntrypoint root) entrypoints
              compiledR <- CompileProxy.compile testRoot (toNonEmpty rewrittenEntrypoints) flags
              case compiledR of
                Left err -> pure (Left err)
                Right (result, _info) -> pure (Right result)


toNonEmpty :: [a] -> NE.List a
toNonEmpty xs = case xs of
  [] -> error "toNonEmpty called with empty list"
  (y:ys) -> NE.List y ys


-- Write a rewritten elm.json for tests under testRoot
writeTestElmJson :: FilePath -> Outline.AppOutline -> IO ()
writeTestElmJson testRoot (Outline.AppOutline _ srcDirs depsDirect depsIndirect testDirect testIndirect) = do
  let rewriteSrcDir srcDir =
        case srcDir of
          Outline.AbsoluteSrcDir dir -> Outline.AbsoluteSrcDir dir
          Outline.RelativeSrcDir dir -> Outline.RelativeSrcDir ("../../" </> dir)
  let newSrcDirsList = fmap rewriteSrcDir (NE.toList srcDirs)
                      ++ [ Outline.RelativeSrcDir ("../../" </> "tests")
                         , Outline.RelativeSrcDir ("../../" </> "elm-stuff" </> "generated")
                         ]
  let newSrcDirs = case newSrcDirsList of
        [] -> error "No source directories found in elm.json"
        (d:ds) -> NE.List d ds
  let mergedDirect = Map.union depsDirect testDirect
  let mergedIndirect = Map.union depsIndirect testIndirect
  let newOutline = Outline.App (Outline.AppOutline V.compiler newSrcDirs mergedDirect mergedIndirect Map.empty Map.empty)
  Outline.write testRoot newOutline


rewriteEntrypoint :: FilePath -> FilePath -> FilePath
rewriteEntrypoint root path =
  let rel = FP.makeRelative root path
  in ("../../" </> rel)



