{-# LANGUAGE OverloadedStrings #-}

module Watchtower where

import Terminal (Command(..), noArgs, Summary(..), flag, Parser(..), (|--), flags)
import Terminal.Helpers
import Text.Read (readMaybe)
import qualified Text.PrettyPrint.ANSI.Leijen as P
import qualified Watchtower.Server



data Flags =
  Flags
    { _port :: Maybe Int
    }


start :: Terminal.Command
start =
  let
    summary =
      "The watchtower development experience dreams are made of."

    details =
      "The `start` command starts the watchtower server on your computer:"

    example =
      reflow
        "After running that command, watchtower is listening at <http://localhost:9000>\
        \ and ready to be connected to."

    reactorFlags =
      flags Flags
        |-- flag "port" port_ "The port of the watchtower server (default: 9000)"
  in
  Terminal.Command "start" (Common summary) details example noArgs reactorFlags run


port_ :: Parser Int
port_ =
  Parser
    { _singular = "port"
    , _plural = "ports"
    , _parser = readMaybe
    , _suggest = \_ -> return []
    , _examples = \_ -> return ["9000"]
    }


reflow :: String -> P.Doc
reflow string =
  P.fillSep $ map P.text $ words string


run :: () -> Flags -> IO ()
run _ (Flags maybePort) =
  Watchtower.Server.serve (Watchtower.Server.Flags maybePort)
