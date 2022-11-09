{-# LANGUAGE OverloadedStrings #-}

module Watchtower where

import Terminal
import Text.Read (readMaybe)
import qualified Text.PrettyPrint.ANSI.Leijen as P
import qualified Watchtower.Server


data Flags =
  Flags
    { _port :: Maybe Int
    }


main :: IO ()
main =
  Terminal.app intro outro
    [  start
    ]



start :: Terminal.Command
start =
  let
    summary =
      "The Elm development experience dreams are made of."

    details =
      "The `start` command starts the Elm Dev server on your computer:"

    example =
      reflow
        "After running that command, watchtower is listening at <http://localhost:4747>\
        \ and ready to be connected to."

    serverFlags =
      flags Flags
        |-- flag "port" port_ "The port of the Elm Dev server (default: 4747)"
  in
  Terminal.Command "start" (Common summary) details example (optional dir) serverFlags run


intro :: P.Doc
intro =
  P.vcat
    [ P.fillSep
        ["Hi,","thank","you","for","trying","out"
        ,P.green "Elm Dev."
        ," I hope you like it!"
        ]
    , ""

    ]


outro :: P.Doc
outro =
  P.fillSep $ map P.text $ words $
    "Happy hacking!"


port_ :: Parser Int
port_ =
  Parser
    { _singular = "port"
    , _plural = "ports"
    , _parser = readMaybe
    , _suggest = \_ -> return []
    , _examples = \_ -> return ["4747"]
    }


reflow :: String -> P.Doc
reflow string =
  P.fillSep $ map P.text $ words string


run :: Maybe FilePath -> Watchtower.Flags -> IO ()
run maybeRoot (Watchtower.Flags maybePort) =
  Watchtower.Server.serve maybeRoot (Watchtower.Server.Flags maybePort)



dir :: Parser FilePath
dir =
  Parser
    { _singular = "elm project directory"
    , _plural = "elm project directories"
    , _parser = parseDir
    , _suggest = \_ -> return []
    , _examples = exampleProjectDir
    }


parseDir :: String -> Maybe FilePath
parseDir chars =
  Just chars



exampleProjectDir :: String -> IO [String]
exampleProjectDir _ =
  return ["/path/to/my/project" ]

