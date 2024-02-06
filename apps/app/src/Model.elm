module Model exposing
    ( Model
    , Msg(..)
    , Viewing(..)
    )

import Dict exposing (Dict)
import Editor
import Elm.ProjectStatus
import Http
import Json.Decode as Decode
import Json.Encode
import Ports
import Question
import Set exposing (Set)
import Time


type alias Model =
    { server : Ports.Server

    -- editor
    , active : Maybe Editor.Editor
    , visible : List Editor.Editor
    , projects : List Elm.ProjectStatus.Status
    , projectsVersion : Int

    -- local UI state
    , viewing : Viewing
    , now : Maybe Time.Posix
    , lastUpdated : Maybe Time.Posix

    -- per-file information
    , warnings : Dict FilePath (List Ports.Warning)
    , errorMenuVisible : Bool
    , errorCodeExpanded : Set Elm.ProjectStatus.CodeReferenceKey
    , callgraph : Dict FilePath (List Ports.CallGraphNode)
    , facts : Dict FilePath (List Ports.Fact)
    }



type alias FilePath =
    String


type Viewing
    = Overview


type Msg
    = Incoming (Result Decode.Error Ports.Incoming)
    | View Viewing
    | AnswerReceived (Result Http.Error Question.Answer)
      -- Editor actions
    | EditorGoTo FilePath Editor.Region
    | EditorFillTypeSignatures FilePath
      --
    | ErrorMenuUpdated Bool
    | ErrorCodeToggled Elm.ProjectStatus.CodeReferenceKey Bool
    | CurrentTime Time.Posix
