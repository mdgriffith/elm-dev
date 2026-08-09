{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}

module Gen.Javascript where

import Control.Exception (IOException, try)
import Control.Concurrent (threadDelay)
import qualified Data.Char as Char
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.FileEmbed
import qualified Language.Haskell.TH
import System.FilePath ((</>))
import System.IO (IOMode(ReadMode, WriteMode), hClose, withBinaryFile)
import System.IO.Temp (withSystemTempDirectory, withSystemTempFile)
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
  result <- try $ withSystemTempDirectory "embedded-stdio" $ \stdioDir -> do
    let inputPath = stdioDir </> "stdin"
        outputPath = stdioDir </> "stdout"
        errorPath = stdioDir </> "stderr"
    BS.writeFile inputPath input
    exitCode <-
      withBinaryFile inputPath ReadMode $ \inputHandle ->
        withBinaryFile outputPath WriteMode $ \outputHandle ->
          withBinaryFile errorPath WriteMode $ \errorHandle -> do
            let process = (System.Process.proc "node" [tempPath])
                  { System.Process.std_in = System.Process.UseHandle inputHandle
                  , System.Process.std_out = System.Process.UseHandle outputHandle
                  , System.Process.std_err = System.Process.UseHandle errorHandle
                  }
            System.Process.withCreateProcess process $ \_ _ _ processHandle ->
              waitForProcessInterruptibly processHandle
    output <- UTF8.toString <$> BS.readFile outputPath
    stderr <- UTF8.toString <$> BS.readFile errorPath
    pure (exitCode, output, stderr)
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


waitForProcessInterruptibly :: System.Process.ProcessHandle -> IO Exit.ExitCode
waitForProcessInterruptibly processHandle = do
  maybeExit <- System.Process.getProcessExitCode processHandle
  case maybeExit of
    Just exitCode -> pure exitCode
    Nothing -> threadDelay 10000 >> waitForProcessInterruptibly processHandle

-- Dynamically adjusted by build.sh to make sure haskell doesn't bamboozle us.
version :: String
version = "aee791e077ac8d63"
