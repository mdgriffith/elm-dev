port module Main exposing
    ( main
    , messageReceiver
    , sendMessage
    )

{-| -}

import Browser
import Html as What
import Imported
import Result



-- import Imported


{-| Here are my flags!!
-}
type alias Flags =
    { flag : String }


{-| What is this??
-}
main : Program Flags Model Msg
main =
    Browser.document
        { init = \{ flag } -> ( { flag = flag }, Cmd.none )
        , view =
            \model ->
                { title = "test"
                , body = [ What.text model.flag ]
                }
        , update = update
        , subscriptions =
            subscriptions
        }


type alias Model =
    { flag : String }


{-| HERE IS MY COMMENT
-}
type Msg
    = NoOp
    | Received String


update msg model =
    case msg of
        NoOp ->
            ( model, sendMessage model.flag )

        Received _ ->
            ( model, Cmd.none )


port sendMessage : String -> Cmd msg


port messageReceiver : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    messageReceiver Received


type alias Otherthing =
    { a : String
    , b : Int
    }


type alias Thing var =
    { notANextOne : Bool
    , aThird : var
    }


what : String
what =
    let
        notUsed =
            "whoops"
    in
    "Other thing!"


anotherTHing =
    43


howAboutARecord =
    { notANextOne = True
    , aThird = what
    }


andWhatBoutHere : String
andWhatBoutHere =
    "Whaaasat?"


gnome =
    Imported.otherThing
