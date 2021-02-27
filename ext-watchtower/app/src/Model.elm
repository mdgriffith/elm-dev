module Model exposing
    ( Model
    , Msg(..)
    , Viewing(..)
    )

import Editor
import Elm
import Json.Decode as Decode
import Json.Encode
import Ports


type alias Model =
    { active : Maybe Editor.Editor
    , visible : List Editor.Editor
    , projects : List Elm.Status
    , projectsVersion : Int

    -- local UI state
    , viewing : Viewing
    }


type Viewing
    = Overview


type Msg
    = Incoming (Result Decode.Error Ports.Incoming)
    | EditorGoTo Elm.File Elm.Problem
    | View Viewing
