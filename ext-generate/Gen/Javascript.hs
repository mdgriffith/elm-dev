{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Gen.Javascript where

import qualified Language.Haskell.TH
import qualified Data.FileEmbed
import System.Process (readCreateProcess, shell, StdStream(..), withCreateProcess, waitForProcess, std_in, std_out)
import Control.Exception (try, SomeException)
import qualified Data.ByteString as BS
import System.IO.Temp (withSystemTempFile)
import System.FilePath ((</>))
import System.Exit (ExitCode(..))
import System.IO (hClose, hGetContents)

-- | Load a file at compile time
generatorJs :: BS.ByteString
generatorJs =
     $(Data.FileEmbed.bsToExp =<< 
        Language.Haskell.TH.runIO 
            (BS.readFile ("ext-generate" </> "generator" </> "dist" </> "run.js"))
    )

-- | Execute embedded JavaScript using Bun

run :: BS.ByteString -> BS.ByteString ->  IO (Either String String)
run jsCode input = withSystemTempFile "embedded.js" $ \tempPath handle -> do
    -- Write the embedded code to a temporary file
    BS.hPut handle jsCode
    -- Execute using Bun and pass input through stdin
    let process = (shell $ "bun " ++ tempPath) { std_in = CreatePipe, std_out = CreatePipe }
    result <- try $ withCreateProcess process $ \(Just stdin) (Just stdout) _ ph -> do
        BS.hPut stdin input
        hClose stdin
        output <- hGetContents stdout
        exitCode <- waitForProcess ph
        case exitCode of
            ExitSuccess -> return output
            ExitFailure code -> return $ "Process exited with code: " ++ show code
    case result of
        Left err -> return $ Left $ "Error executing script: " ++ show (err :: SomeException)
        Right output -> return $ Right output