module Editor exposing (..)

{-| -}

import Json.Decode as Decode
import Json.Encode as Encode


type alias Editor =
    { filepath : String
    , unsavedChanges : Bool
    , regions : List Range

    -- , selections : List Range
    , active : Bool
    }


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
    { line : Int
    , column : Int
    }


moduleName : Editor -> String
moduleName editor =
    editor.filepath
        |> String.split "/"
        |> List.reverse
        |> List.head
        |> Maybe.withDefault editor.filepath


regionToString : Region -> String
regionToString region =
    String.fromInt region.start.line ++ ":" ++ String.fromInt region.end.line


visibleRanges : List Editor -> List Range
visibleRanges editors =
    List.concatMap .regions editors



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
        [ ( "line", Encode.int pos.line )
        , ( "column", Encode.int pos.column )
        ]



{- DECODERS -}


decodeEditor : Decode.Decoder Editor
decodeEditor =
    Decode.map4 Editor
        (Decode.field "filepath" Decode.string)
        (Decode.field "unsavedChanges" Decode.bool)
        (Decode.field "regions" (Decode.list decodeRegion))
        -- (Decode.field "selections" (Decode.list decodeRegion))
        (Decode.field "active" Decode.bool)


decodeWorkspaceFolder =
    Decode.map2 Workspace
        (Decode.field "name" Decode.string)
        (Decode.field "path" Decode.string)


position =
    Decode.map2 Position
        (Decode.oneOf
            [ Decode.field "line" Decode.int
            , Decode.field "row" Decode.int
            ]
        )
        (Decode.oneOf
            [ Decode.field "column" Decode.int
            , Decode.field "character" Decode.int
            , Decode.field "col" Decode.int
            ]
        )


decodeRegion =
    Decode.map2 Region
        (Decode.field "start" position)
        (Decode.field "end" position)



{- HELPERS -}


visible : Range -> List Range -> Bool
visible rng viewing =
    List.any (\v -> overlap rng v || overlap v rng) viewing


above : Range -> List Range -> Bool
above rng viewing =
    List.any (aboveRange rng) viewing


below : Range -> List Range -> Bool
below rng viewing =
    List.all (belowRange rng) viewing


overlap one two =
    if one.start.line >= two.start.line && one.start.line <= two.end.line then
        True

    else if one.end.line >= two.start.line && one.end.line <= two.end.line then
        True

    else
        False


belowRange shouldBeBelow two =
    shouldBeBelow.start.line > two.end.line


aboveRange shouldBeAbove two =
    shouldBeAbove.end.line < two.start.line
