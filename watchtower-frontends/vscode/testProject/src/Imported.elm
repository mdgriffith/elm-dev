module Imported exposing (..)

{-| -}

import Html as Ui
import NarrativeEngine.Syntax.NarrativeParser
import NarrativeEngine.Syntax.RuleParser


otherThing : Ui.Html msg
otherThing =
    Ui.text "Hello world!"


type MyType inner
    = MyValue inner
