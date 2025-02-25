module Gen.Commands (initialize, make, add, customize) where

import qualified Terminal
import Terminal ((!), (?), (...))
import Data.Text (Text)
import qualified Text.PrettyPrint.ANSI.Leijen as P
import qualified Terminal.Helpers
import qualified Elm.ModuleName
import qualified Gen.Javascript


-- INIT COMMAND
initialize :: Terminal.Command
initialize =
  let
    summary =
      "Create a new Elm project"

    details =
      "The `init` command sets up a new Elm project with the recommended structure."

    example =
      reflow
        "Run this command in an empty directory to create a new Elm project."

    initFlags = Terminal.noFlags
    initArgs = Terminal.noArgs
  in
  Terminal.Command "init" (Terminal.Common summary) details example initArgs initFlags runInit


-- MAKE COMMAND
make :: Terminal.Command
make =
  let
    summary =
      "Build and compile your Elm project"

    details =
      "The `make` command compiles your Elm project."

    example =
      reflow
        "Run this command to compile your Elm project."

    makeFlags = Terminal.noFlags
    makeArgs = Terminal.noArgs
  in
  Terminal.Command "make" (Terminal.Common summary) details example makeArgs makeFlags runMake


-- ADD COMMAND
add :: Terminal.Command
add =
  let
    summary =
      "Add new components to your project"

    details =
      "The `add` command helps you add new pages, stores, effects, docs, or themes."

    example =
      reflow
        "Use this command with one of: page, store, effect, docs, theme"

    addFlags = Terminal.noFlags
    addArgs = Terminal.oneOf
      [ Terminal.require2 (\_ -> AddPage) (exactly "page") Terminal.Helpers.elmModule
      , Terminal.require2 (\_ -> AddStore) (exactly "store") Terminal.Helpers.elmModule
      , Terminal.require2 (\_ -> AddEffect) (exactly "effect") Terminal.Helpers.elmModule
      , Terminal.require1 (\_ -> AddDocs) (exactly "docs")
      , Terminal.require1 (\_ -> AddTheme) (exactly "theme")
      ]
  in
  Terminal.Command "add" (Terminal.Common summary) details example addArgs addFlags runAdd

-- TYPES
data AddCommand
  = AddPage Elm.ModuleName.Raw
  | AddStore Elm.ModuleName.Raw
  | AddEffect Elm.ModuleName.Raw
  | AddDocs
  | AddTheme 


exactly :: String -> Terminal.Parser ()
exactly str =
  Terminal.Parser
    { Terminal._singular = "type"
    , Terminal._plural = "types"
    , Terminal._parser = parseExact str
    , Terminal._suggest = \_ -> return [ str ]
    , Terminal._examples = \_ -> return [str]
    , Terminal._choices = Nothing
    }


parseExact :: String -> String -> Maybe ()
parseExact target found =
  if target == found then 
    Just ()
  else 
    Nothing





-- CUSTOMIZE COMMAND
customize :: Terminal.Command
customize =
  let
    summary =
      "Customize project components"

    details =
      "The `customize` command allows you to customize components given an Elm module name."

    example =
      reflow
        "Run this command with an optional Elm module name (e.g., 'My.Module')"

    customizeFlags = Terminal.noFlags
    customizeArgs = Terminal.oneOrMoreWith Customize (Terminal.Helpers.elmModule)
  in
  Terminal.Command "customize" (Terminal.Common summary) details example customizeArgs customizeFlags runCustomize


data CustomizeCommand
  = Customize Elm.ModuleName.Raw [Elm.ModuleName.Raw]


-- RUNNERS (These need to be implemented)
runInit :: () -> () -> IO ()
runInit () () = 
    do  putStrLn "Initializing project..."

runMake :: () -> () -> IO ()
runMake () () =
    do  putStrLn "Initializing project..."

runAdd :: AddCommand -> () -> IO ()
runAdd cmd () =
    do  putStrLn "Initializing project..."

runCustomize :: CustomizeCommand -> () -> IO ()
runCustomize maybeModule () =
    do  putStrLn "Initializing project..."



reflow :: String -> P.Doc
reflow string =
  P.fillSep $ map P.text $ words string


