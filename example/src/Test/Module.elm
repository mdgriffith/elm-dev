module Test.Module exposing (myOtherFunction)


import Html
import Html.Attributes as Attr
import Html.Events exposing (..)
import Json.Decode as JD exposing (string)
import Json.Encode exposing (int)


myOtherFunction : String -> Html.Html msg
myOtherFunction incoming =
    Html.div
        (List.map identity [ Attr.id "carl" ])
        [ Html.text (incoming ++ ", the Best") ]



