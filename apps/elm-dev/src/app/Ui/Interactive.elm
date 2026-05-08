module Ui.Interactive exposing
    ( init
    , Model, Msg, update
    , view
    )

{-|

@docs init
@docs Model, Msg, update

@docs view

-}

import Html
import Html.Attributes
import Http
import Json.Decode
import Json.Encode


type alias Model =
    { projectRoot : String
    , moduleName : String
    , elmCode : Maybe String
    , error : Maybe String
    }


type Msg
    = CodeReceived (Result Http.Error String)
    | DataUpdated String


init : { projectRoot : String, moduleName : String } -> ( Model, Cmd Msg )
init { projectRoot, moduleName } =
    ( { projectRoot = projectRoot
      , moduleName = moduleName
      , elmCode = Nothing
      , error = Nothing
      }
    , getCode projectRoot moduleName
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CodeReceived result ->
            case result of
                Ok code ->
                    ( { model | elmCode = Just code, error = Nothing }
                    , Cmd.none
                    )

                Err error ->
                    ( { model | error = Just (httpErrorToString error) }
                    , Cmd.none
                    )

        DataUpdated data ->
            ( model, Cmd.none )


view : Model -> Html.Html Msg
view model =
    case model.elmCode of
        Just code ->
            Html.div
                []
                [ Html.node "elm-dev"
                    [ Html.Attributes.attribute "code" code
                    , Html.Attributes.attribute "data"
                        (Json.Encode.encode 0
                            (Json.Encode.string "")
                        )
                    ]
                    []
                ]

        Nothing ->
            case model.error of
                Just error ->
                    Html.div [] [ Html.text ("Error: " ++ error) ]

                Nothing ->
                    Html.div [] [ Html.text "Loading..." ]


getCode : String -> String -> Cmd Msg
getCode projectRoot moduleName =
    Http.get
        { url = "/interactive/make?root=" ++ projectRoot ++ "&module=" ++ moduleName
        , expect = Http.expectString CodeReceived
        }


httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.BadUrl url ->
            "Bad URL: " ++ url

        Http.Timeout ->
            "Request timed out"

        Http.NetworkError ->
            "Network error"

        Http.BadStatus status ->
            "Bad status: " ++ String.fromInt status

        Http.BadBody body ->
            "Bad body: " ++ body
