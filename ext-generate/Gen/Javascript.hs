{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Gen.Javascript where

import qualified Language.Haskell.TH
import qualified Data.FileEmbed
import System.Process (readCreateProcess, shell, StdStream(..), withCreateProcess, waitForProcess, std_in, std_out)
import Control.Exception (try, SomeException)
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
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
    hClose handle  -- Close the handle after writing
    -- Execute using Bun and pass input through stdin
    let process = shell $ "node " ++ tempPath
    result <- try $ readCreateProcess process (UTF8.toString input)
    case result of
        Left err -> return $ Left $ "Error executing script: " ++ show (err :: SomeException)
        Right output -> return $ Right output


-- Dynamically adjusted by build.sh to make sure haskell doesn't bamboozle us.
version :: String
version = "0ccd1ebd5bfc04b8"
