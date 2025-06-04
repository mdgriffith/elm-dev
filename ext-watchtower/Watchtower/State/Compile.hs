module Watchtower.State.Compile (compile, MakeOptions, Error(..)) where

import qualified Ext.CompileProxy as CompileProxy
import qualified Ext.Dev.Project as Project
import qualified Watchtower.Live.Client as Client
import qualified Ext.Sentry as Sentry
import qualified Gen.Generate
import qualified Ext.Dev.Project
import qualified Ext.CompileHelpers.Generic as CompileHelpers
import qualified Reporting.Exit as Exit
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Builder as B

import qualified Json.Encode as Json
import Json.Encode ((==>))

data MakeOptions = MakeOptions
  { _makeDebug :: Maybe Bool
  , _makeOptimize :: Maybe Bool
  }

data Error
  = ReactorError Exit.Reactor
  | GenerationError String

compile :: CompileHelpers.Flags -> Client.ProjectCache -> IO (Either Error CompileHelpers.CompilationResult)
compile flags projCache@(Client.ProjectCache proj@(Ext.Dev.Project.Project projectRoot elmJsonRoot entrypoints) projectCompilationCache) = do
  -- First run code generation
  codegenResult <- Gen.Generate.run
  case codegenResult of
    Right () -> do
      -- Then run compilation
      compilationResult <- CompileProxy.compile elmJsonRoot entrypoints flags

      -- Update the compilation result in Sentry
      let eitherStatusJson = case compilationResult of
            Right result -> Right (Json.object [ "compiled" ==> Json.bool True ])
            Left exit -> Left (Exit.toJson (Exit.reactorToReport exit))
      
      -- record status   
      Sentry.updateCompileResult projectCompilationCache $ pure eitherStatusJson
      
      -- unpack the compilation result and call Sentry.updateJsOutput if the result was js.
      case compilationResult of
        Right (CompileHelpers.CompiledJs js) -> Sentry.updateJsOutput projectCompilationCache (pure $ BS.toStrict $ B.toLazyByteString js)
        _ -> pure ()

      pure $ case compilationResult of
        Right result -> Right result
        Left exit -> Left (ReactorError exit)

    Left err -> do
      -- Update Sentry with the error
      Sentry.updateCompileResult projectCompilationCache $ pure $ Left (Json.chars err)
      pure $ Left (GenerationError err)

