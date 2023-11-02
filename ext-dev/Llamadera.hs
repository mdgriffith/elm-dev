{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Llamadera (formatHaskellValue, hindentPrintValue) where

{- Helpers extracted from Lamdera
-}

import Control.Monad (unless)
import Data.Text (Text)
import Ext.Common
import qualified Data.Text as T
import qualified System.Process
import qualified Text.Show.Unicode
import System.IO.Unsafe (unsafePerformIO)

{-|

Useful when trying to understand AST values or just unknown values in general.

Requires hindent to be installed; try stack install hindent

Most conveniently used like so;

{-# LANGUAGE BangPatterns #-}

let
  !_ = formatHaskellValue "some sensible label" (blah) :: IO ()
in
blah

The bang pattern forces evaluation and you don't have to worry about the type-context, i.e. not being in IO.

-}
formatHaskellValue label v =
  unsafePerformIO $ do
    hindentPrintValue label v
    pure $ pure ()


hindentPrintValue :: Show a => Text -> a -> IO a
hindentPrintValue label v = do
  let
    input = Text.Show.Unicode.ushow v

  -- if Prelude.length input > 10000
  --   then
  --     atomicPutStrLn $ "❌SKIPPED display, value show > 10,000 chars, here's a clip:\n" <> Prelude.take 1000 input
  --   else do
  (exit, stdout, stderr) <- System.Process.readProcessWithExitCode "hindent" ["--line-length","150"] input
  if Prelude.length stderr > 0
    then
      atomicPutStrLn $
        "\n🔶--------------------------------------------------------------------------------"
          <> T.unpack label
          <> "\n"
          <> stderr
          <> "\n📥 for input: \n"
          <> input

    else
      atomicPutStrLn $
        "\n🔶--------------------------------------------------------------------------------"
          <> T.unpack label
          <> "\n"
          <> stdout

  pure v


hindentFormatValue :: Show a => a -> Text
hindentFormatValue v =
  unsafePerformIO $ do
    (exit, stdout, stderr) <- System.Process.readProcessWithExitCode "hindent" ["--line-length","150"] (Text.Show.Unicode.ushow v)
    pure $ T.pack stdout


-- Inversion of `unless` that runs IO only when condition is True
onlyWhen :: Monad f => Bool -> f () -> f ()
onlyWhen condition io =
  unless (not condition) io


withDefault :: a -> Maybe a -> a
withDefault default_ m =
  case m of
    Just v -> v
    Nothing -> default_
