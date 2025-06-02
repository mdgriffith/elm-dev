module Ui.Interactive.Controls exposing
    ( Controls, Input, InputValue(..)
    , getString, getBool, getInt, getFloat
    , getOneOf, getManyOf
    , encode, decode
    )

{-| This is for tracking the state of an interactive element.

@docs Controls, Input, InputValue

@docs getString, getBool, getInt, getFloat
@docs getOneOf, getManyOf

@docs encode, decode

-}

import Dict exposing (Dict)
import Json.Decode as Decode
import Json.Encode as Encode


type alias Path =
    String


type alias Controls =
    { data : Dict Path Input
    }


type alias Input =
    { name : String
    , path : Path
    , required : Bool
    , input : InputValue
    }


{-| May also have

-- | Array (List InputValue)
-- | Object (Dict Path InputValue)

At some point

-}
type InputValue
    = Str String
    | OneOf OneOfDetails
    | ManyOf (List SelectedItem)
    | Boolean Bool
    | Number NumberDetails


type alias OneOfDetails =
    { options : List String
    , value : Maybe String
    }


type alias SelectedItem =
    { selected : Bool
    , value : String
    }


type alias NumberDetails =
    { min : Maybe Float
    , max : Maybe Float
    , step : Maybe Float
    , integer : Bool
    , value : Maybe Float
    }


getString : Controls -> String -> String
getString controls path =
    case Dict.get path controls.data of
        Just input ->
            case input.input of
                Str str ->
                    str

                _ ->
                    ""

        Nothing ->
            ""


getBool : Controls -> String -> Bool
getBool controls path =
    case Dict.get path controls.data of
        Just input ->
            case input.input of
                Boolean bool ->
                    bool

                _ ->
                    False

        Nothing ->
            False


getInt : Controls -> String -> Maybe Int
getInt controls path =
    case Dict.get path controls.data of
        Just input ->
            case input.input of
                Number details ->
                    Maybe.map round details.value

                _ ->
                    Nothing

        Nothing ->
            Nothing


getFloat : Controls -> String -> Maybe Float
getFloat controls path =
    case Dict.get path controls.data of
        Just input ->
            case input.input of
                Number details ->
                    details.value

                _ ->
                    Nothing

        Nothing ->
            Nothing


getOneOf : Controls -> String -> (String -> Maybe a) -> Maybe a
getOneOf controls path toValue =
    case Dict.get path controls.data of
        Just input ->
            case input.input of
                OneOf details ->
                    details.value
                        |> Maybe.andThen toValue

                _ ->
                    Nothing

        Nothing ->
            Nothing


getManyOf : Controls -> String -> (String -> Maybe a) -> List a
getManyOf controls path toValue =
    case Dict.get path controls.data of
        Just input ->
            case input.input of
                ManyOf details ->
                    List.filterMap
                        (\item ->
                            if item.selected then
                                toValue item.value

                            else
                                Nothing
                        )
                        details

                _ ->
                    []

        Nothing ->
            []



-- Encode


encode : Controls -> Encode.Value
encode controls =
    Encode.object
        [ ( "data", Encode.dict identity encodeInput controls.data )
        ]


encodeInput : Input -> Encode.Value
encodeInput input =
    Encode.object
        [ ( "name", Encode.string input.name )
        , ( "required", Encode.bool input.required )
        , ( "input", encodeInputValue input.input )
        ]


encodeInputValue : InputValue -> Encode.Value
encodeInputValue value =
    case value of
        Str str ->
            Encode.string str

        OneOf details ->
            Encode.object
                [ ( "oneOf"
                  , Encode.object
                        [ ( "value", encodeNullable Encode.string details.value )
                        , ( "options", Encode.list Encode.string details.options )
                        ]
                  )
                ]

        ManyOf options ->
            Encode.object
                [ ( "manyOf"
                  , Encode.list
                        encodeSelectedItem
                        options
                  )
                ]

        Boolean bool ->
            Encode.bool bool

        Number details ->
            Encode.object
                [ ( "number"
                  , Encode.object
                        [ ( "min", encodeNullable Encode.float details.min )
                        , ( "max", encodeNullable Encode.float details.max )
                        , ( "step", encodeNullable Encode.float details.step )
                        , ( "integer", Encode.bool details.integer )
                        , ( "value", encodeNullable Encode.float details.value )
                        ]
                  )
                ]


encodeNullable : (a -> Encode.Value) -> Maybe a -> Encode.Value
encodeNullable toJson maybe =
    case maybe of
        Just value ->
            toJson value

        Nothing ->
            Encode.null


encodeSelectedItem : SelectedItem -> Encode.Value
encodeSelectedItem item =
    Encode.object
        [ ( "selected", Encode.bool item.selected )
        , ( "value", Encode.string item.value )
        ]


decode : Decode.Decoder Controls
decode =
    Decode.map Controls
        (Decode.field "data" (Decode.dict decodeInput))


decodeInput : Decode.Decoder Input
decodeInput =
    Decode.map4 Input
        (Decode.field "name" Decode.string)
        (Decode.field "path" Decode.string)
        (Decode.field "required" Decode.bool)
        (Decode.field "input" decodeInputValue)


decodeInputValue : Decode.Decoder InputValue
decodeInputValue =
    Decode.oneOf
        [ Decode.map Str Decode.string
        , Decode.map OneOf (Decode.field "oneOf" decodeOneOfDetails)
        , Decode.map ManyOf (Decode.field "manyOf" (Decode.list decodeSelectedItem))
        , Decode.map Boolean Decode.bool
        , Decode.map Number (Decode.field "number" decodeNumberDetails)
        ]


decodeOneOfDetails : Decode.Decoder OneOfDetails
decodeOneOfDetails =
    Decode.map2 OneOfDetails
        (Decode.field "options" (Decode.list Decode.string))
        (Decode.field "value" (Decode.maybe Decode.string))


decodeSelectedItem : Decode.Decoder SelectedItem
decodeSelectedItem =
    Decode.map2 SelectedItem
        (Decode.field "selected" Decode.bool)
        (Decode.field "value" Decode.string)


decodeNumberDetails : Decode.Decoder NumberDetails
decodeNumberDetails =
    Decode.map5 NumberDetails
        (Decode.field "min" (Decode.maybe Decode.float))
        (Decode.field "max" (Decode.maybe Decode.float))
        (Decode.field "step" (Decode.maybe Decode.float))
        (Decode.field "integer" Decode.bool)
        (Decode.field "value" (Decode.maybe Decode.float))
