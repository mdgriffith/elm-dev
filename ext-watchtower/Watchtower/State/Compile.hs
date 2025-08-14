module Watchtower.State.Compile (compile) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LBS
import qualified Data.NonEmptyList as NE
import qualified Ext.CompileHelpers.Generic as CompileHelpers
import qualified Ext.CompileProxy as CompileProxy
import qualified Ext.Dev.Project
import qualified Ext.Dev.Project as Project
import qualified Ext.Sentry as Sentry
import qualified Control.Concurrent.STM as STM
import qualified Gen.Generate
import Json.Encode ((==>))
import qualified Json.Encode as Json
import qualified Reporting.Exit as Exit
import qualified System.Directory as Dir (withCurrentDirectory)
import qualified Watchtower.Live.Client as Client


compile :: CompileHelpers.Flags -> Client.ProjectCache -> [FilePath] -> IO (Either Client.Error CompileHelpers.CompilationResult)
compile flags projCache@(Client.ProjectCache proj@(Ext.Dev.Project.Project projectRoot elmJsonRoot entrypoints) docsInfo mCompileResult) files = do
  Dir.withCurrentDirectory projectRoot $ do
    -- First run code generation
    codegenResult <- Gen.Generate.run
    case codegenResult of
      Right () -> do
        let filesToCompile = NE.append files entrypoints
        -- Then run compilation
        compilationResult <- CompileProxy.compile elmJsonRoot entrypoints flags

        -- Update the compilation result TVar
        let newResult = case compilationResult of
              Right result -> Client.Success result
              Left exit -> Client.Error (Client.ReactorError exit)
        STM.atomically $ STM.writeTVar mCompileResult newResult

        pure $ case compilationResult of
          Right result -> Right result
          Left exit -> Left (Client.ReactorError exit)
      Left err -> do
        -- Update compile result TVar with the error
        STM.atomically $ STM.writeTVar mCompileResult (Client.Error (Client.GenerationError err))
        pure $ Left (Client.GenerationError err)
