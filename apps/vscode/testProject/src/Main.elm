port module Main exposing
    ( main
    , messageReceiver
    , sendMessage
    )

{-| -}

import Alias
import Browser
import Html as What
import Imported
import Result
import Ui.Arbor


type alias Regions view =
    { main : view
    , nav : Maybe view
    , detail : List view
    }


{-| Here are my flags!!
-}
type alias Flags one =
    { flag : one
    , innerRecord : InnerRecord
    , inner : String
    , other : Bool
    }


type alias InnerRecord =
    { test : Bool }


{-| What is this??
-}
main : Program (Flags String) Model Msg
main =
    Browser.document
        { init =
            \flag ->
                ( { flag = flag
                  , value = Alias.test { test = True }
                  }
                , sendMessage "Yoo"
                )
        , view =
            \model ->
                { title = "test"
                , body = [ What.text model.flag.flag ]
                }
        , update = update
        , subscriptions =
            subscriptions
        }



-- main =
--     Ui.Arbor.main


type alias Model =
    { flag : Flags String
    , value : Alias.Alias InnerRecord
    }


{-| HERE IS MY COMMENT
-}
type Msg
    = NoOp
    | Received String
    | OtherReceived InnerRecord


update msg model =
    case msg of
        NoOp ->
            ( model, sendMessage model.flag.flag )

        Received _ ->
            ( model, Imported.sendOtherMessage "pls" )

        OtherReceived _ ->
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
