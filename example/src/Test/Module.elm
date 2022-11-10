module Test.Module exposing (InvalidReason(..), Reason(..), myOtherFunction)


type Reason
    = Custom
    | Equality String String
    | Comparison String String
      -- Expected, actual, (index of problem, expected element, actual element)
    | ListDiff (List String) (List String)
      {- I don't think we need to show the diff twice with + and - reversed. Just show it after the main vertical bar.
         "Extra" and "missing" are relative to the actual value.
      -}
    | CollectionDiff
        { expected : String
        , actual : String
        , extra : List String
        , missing : List String
        }
    | TODO
    | Invalid InvalidReason


type InvalidReason
    = EmptyList
    | NonpositiveFuzzCount
    | InvalidFuzzer
    | BadDescription
    | DuplicatedName


import Html
import Html.Attributes as Attr
import Html.Events exposing (..)
import Json.Decode as JD exposing (string)
import Json.Encode exposing (int)
import Maybe
import Result


myOtherFunction : String -> Html.Html msg
myOtherFunction incoming =
    let
        carl = 5
    in
    Html.div
        (List.map identity [ Attr.id "carl" ])
        [ Html.text (incoming ++ ", the Best")
        ]



