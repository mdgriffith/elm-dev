module Ext.CompileHelpers.Disk where


import qualified System.Directory as Dir
import Prelude hiding (lookup)


import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.NonEmptyList as NE
import qualified Data.OneOrMore as OneOrMore
import System.IO.Unsafe (unsafePerformIO)
import Control.Concurrent.MVar
import Control.Monad (liftM2, unless)

import qualified System.Environment as Env
import qualified System.Process
import qualified Text.Show.Unicode
import qualified Data.Text as T
import Data.Text (Text)


import qualified AST.Optimized as Opt
import qualified AST.Canonical as Can
import qualified AST.Source as Src
import qualified Elm.Interface as I
import qualified Elm.Details as Details
import qualified Elm.Package as Pkg
import qualified Elm.ModuleName as ModuleName
import qualified Compile
import qualified Parse.Module as Parse
import qualified BackgroundWriter as BW
import qualified Ext.FileProxy as File
import qualified Reporting
import qualified Reporting.Exit as Exit
import qualified Reporting.Task as Task
import qualified Reporting.Error
import qualified Reporting.Error.Syntax
import qualified System.Exit
import qualified Build
import qualified Stuff
import System.FilePath as FP ((</>), joinPath, splitDirectories, takeDirectory)
import StandaloneInstances

import Ext.Common
import qualified Ext.CompileHelpers.Generic as CompileHelpers



{- Appropriated from worker/src/Artifacts.hs
   WARNING: does not load any user code!!!
-}
allDepArtifacts :: IO CompileHelpers.Artifacts
allDepArtifacts =
  BW.withScope $ \scope ->
  do  --debug "Loading allDeps"
      let style = Reporting.silent
      root <- getProjectRoot
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



