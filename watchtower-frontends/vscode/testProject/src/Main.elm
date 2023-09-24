module Main exposing (main)

import Html as What
import Imported
import Result



-- import Imported


{-| What is this??
-}
main =
    Imported.otherThing


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



-- gnome =
--     Imported.otherThing
