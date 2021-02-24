module Model exposing
    ( Model
    , Msg(..)
    )

import Editor
import Elm
import Json.Decode as Decode
import Json.Encode
import Ports


type alias Model =
    { active :
        Maybe Editor.Editor
    , visible : List Editor.Editor
    , workspace :
        List
            { name : String
            , path : String
            }
    , projects : List Elm.Project
    }


type Msg
    = Incoming (Result Decode.Error Ports.Incoming)
    | GoTo Elm.File Elm.Problem
