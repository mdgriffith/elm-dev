module Editor exposing (..)

{-| -}

import Json.Decode as Decode
import Json.Encode as Encode


type alias Location =
    { file : Path
    , region : Region
    }


type alias Path =
    String


type alias Workspace =
    { name : String
    , path : String
    }


type alias Range =
    { start : Position
    , end : Position
    }


type alias Selection =
    { anchor : Position
    , active : Position
    }


type alias Region =
    { start : Position
    , end : Position
    }


type alias Position =
    { row : Int
    , col : Int
    }


type alias Editor =
    { fileName : String
    , ranges : List Range
    , selections : List Selection
    }


visibleRanges : List Editor -> List Range
visibleRanges editors =
    List.concatMap .ranges editors



{- ENCODERS -}


encodeRegion : Region -> Encode.Value
encodeRegion region =
    Encode.object
        [ ( "start", encodePosition region.start )
        , ( "end", encodePosition region.end )
        ]


encodePosition : Position -> Encode.Value
encodePosition pos =
    Encode.object
        [ ( "row", Encode.int pos.row )
        , ( "col", Encode.int pos.col )
        ]



{- DECODERS -}


decodeEditor : Decode.Decoder Editor
decodeEditor =
    Decode.map3 Editor
        (Decode.field "fileName" Decode.string)
        (Decode.field "ranges" (Decode.list decodeRegion))
        (Decode.field "selections" (Decode.list selection))


decodeWorkspaceFolder =
    Decode.map2 Workspace
        (Decode.field "name" Decode.string)
        (Decode.field "path" Decode.string)


selection =
    Decode.map2 Selection
        (Decode.field "anchor" position)
        (Decode.field "active" position)


rowColPos =
    Decode.map2 Position
        (Decode.field "row" Decode.int)
        (Decode.field "col" Decode.int)


position =
    Decode.map2 Position
        (Decode.field "line" Decode.int)
        (Decode.oneOf
            [ Decode.field "column" Decode.int
            , Decode.field "character" Decode.int
            ]
        )


focus =
    Decode.map2 Range
        (Decode.field "start" rowColPos)
        (Decode.field "end" rowColPos)


decodeRegion =
    Decode.map2 Region
        (Decode.field "start" position)
        (Decode.field "end" position)



{- HELPERS -}


visible : Range -> List Range -> Bool
visible rng viewing =
    List.any (overlap rng) viewing


above : Range -> List Range -> Bool
above rng viewing =
    List.any (aboveRange rng) viewing


below : Range -> List Range -> Bool
below rng viewing =
    List.all (belowRange rng) viewing


overlap one two =
    if one.start.row >= two.start.row && one.start.row <= two.end.row then
        True

    else if one.end.row >= two.start.row && one.end.row <= two.end.row then
        True

    else
        False


belowRange shouldBeBelow two =
    shouldBeBelow.start.row > two.end.row


aboveRange shouldBeAbove two =
    shouldBeAbove.end.row < two.start.row
