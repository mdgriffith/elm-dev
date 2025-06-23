module WebComponents.Elm exposing (elm)

{-| Elm-embedded custom element for embedding compiled Elm code from elm-dev server.
-}

import Html
import Html.Attributes as Attr


{-| Create an elm-embedded custom element with the given properties
-}
elm :
    { baseUrl : String
    , filepath : String
    , cwd : String
    }
    -> Html.Html msg
elm { baseUrl, filepath, cwd } =
    Html.node "elm-embedded"
        [ Attr.id "elm-embedded-container"
        , Attr.attribute "base-url" baseUrl
        , Attr.attribute "filepath" filepath
        , Attr.attribute "cwd" cwd
        , Attr.style "display" "block"
        , Attr.style "width" "100%"
        , Attr.style "height" "100%"
        ]
        []
