{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}

module Gen.Javascript where

import Control.Exception (IOException, try)
import qualified Data.Char as Char
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.FileEmbed
import qualified Language.Haskell.TH
import System.FilePath ((</>))
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import qualified System.Exit as Exit
import qualified System.Process

-- | Error type for JavaScript execution
data RunError
  = ThreadKilled
  | Other String
  deriving (Show, Eq)

-- | Load a file at compile time
generatorJs :: BS.ByteString
generatorJs =
  $( Data.FileEmbed.bsToExp
       =<< Language.Haskell.TH.runIO
        (BS.readFile ("ext-generate" </> "Gen" </> "js" </> "run.js"))
   )

interactiveJs :: BS.ByteString
interactiveJs =
  $( Data.FileEmbed.bsToExp
       =<< Language.Haskell.TH.runIO
        (BS.readFile ("ext-generate" </> "Gen" </> "js" </> "interactive-run.js"))
   )

-- | Execute embedded JavaScript using Node
run :: BS.ByteString -> BS.ByteString -> IO (Either RunError String)
run jsCode input = withSystemTempFile "embedded.js" $ \tempPath handle -> do
  -- Write the embedded code to a temporary file
  BS.hPut handle jsCode
  hClose handle -- Close the handle after writing
  -- Launch Node directly so cancellation targets the runtime rather than a shell.
  let process = System.Process.proc "node" [tempPath]
  result <- try $ System.Process.readCreateProcessWithExitCode process (UTF8.toString input)
  case result of
    Left err -> return $ Left $ Other $ "Error executing script: " ++ show (err :: IOException)
    Right (Exit.ExitSuccess, output, stderr)
      | all Char.isSpace output ->
          return $ Left $ Other $
            "JavaScript process exited successfully but produced no output"
              ++ if null stderr then "" else ":\n" ++ stderr
      | otherwise -> return $ Right output
    Right (Exit.ExitFailure code, _, stderr) ->
      return $ Left $ Other $
        "JavaScript process exited with code " ++ show code
          ++ if null stderr then "" else ":\n" ++ stderr

-- Dynamically adjusted by build.sh to make sure haskell doesn't bamboozle us.
version :: String
version = "db5fe28083a7607a"
