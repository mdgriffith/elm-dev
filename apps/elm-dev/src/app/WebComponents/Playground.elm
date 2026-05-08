module WebComponents.Playground exposing (playground)

{-| Elm-playground custom element for compiling and running Elm source snippets.
-}

import Html
import Html.Attributes as Attr


{-| Create an elm-playground custom element with the given properties
-}
playground :
    { baseUrl : String
    , projectRoot : String
    , elmSource : String
    , filePath : String
    }
    -> Html.Html msg
playground { baseUrl, projectRoot, elmSource, filePath } =
    Html.node "elm-playground"
        [ Attr.attribute "elm-source" elmSource
        , Attr.attribute "baseurl" baseUrl
        , Attr.attribute "project-root" projectRoot
        , Attr.attribute "filepath" filePath
        , Attr.style "display" "block"
        , Attr.style "width" "100%"
        , Attr.style "height" "100%"
        ]
        []

