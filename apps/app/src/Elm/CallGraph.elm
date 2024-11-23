module Elm.CallGraph exposing
    ( decode
    , Call, CallGraph, CallType(..), Node
    )

{-|

@docs decode

-}

import Dict exposing (Dict)
import Json.Decode as Decode


type alias CallGraph =
    { filepath : String
    , nodes : List Node
    }


type alias Node =
    { id : String
    , recursive : Bool
    , calls : List Call
    , callers : List Call
    }


type alias Call =
    { id : String
    , callType : CallType
    }


type CallType
    = Local
    | TopLevel
    | Foreign
    | Constructor
    | Debug
    | Operator


decode : String -> Decode.Decoder CallGraph
decode filepath =
    Decode.map (CallGraph filepath)
        (Decode.list decodeCallGraphNode)


decodeCallGraphNode : Decode.Decoder Node
decodeCallGraphNode =
    Decode.map4 Node
        (Decode.field "id" Decode.string)
        (Decode.field "recursive" Decode.bool)
        (Decode.field "calls" (Decode.list decodeCall))
        (Decode.field "callers" (Decode.list decodeCall))


decodeCall : Decode.Decoder Call
decodeCall =
    Decode.map2 Call
        (Decode.field "id" Decode.string)
        (Decode.field "callType" decodeCallType)


decodeCallType : Decode.Decoder CallType
decodeCallType =
    enum
        (Dict.fromList
            [ Tuple.pair "local" Local
            , Tuple.pair "top-level" TopLevel
            , Tuple.pair "foreign" Foreign
            , Tuple.pair "constructor" Constructor
            , Tuple.pair "debug" Debug
            , Tuple.pair "operator" Operator
            ]
        )


enum : Dict String val -> Decode.Decoder val
enum vals =
    Decode.string
        |> Decode.andThen
            (\str ->
                case Dict.get str vals of
                    Nothing ->
                        Decode.fail ("Don't recognize " ++ str)

                    Just val ->
                        Decode.succeed val
            )
