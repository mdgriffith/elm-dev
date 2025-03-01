{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Gen.Javascript where

import qualified Language.Haskell.TH
import qualified Data.FileEmbed
import System.Process (readCreateProcess, shell)
import Control.Exception (try, SomeException)
import qualified Data.ByteString as BS
import System.IO.Temp (withSystemTempFile)
import System.FilePath ((</>))

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
    -- Execute using Bun
    result <- try $ readCreateProcess (shell $ "bun " ++ tempPath) ""
    case result of
        Left err -> return $ Left $ "Error executing script: " ++ show (err :: SomeException)
        Right output -> return $ Right output