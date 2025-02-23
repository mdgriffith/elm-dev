module Ui.Table.Column exposing
    ( text
    , int, dollars
    , date, dateTime
    )

{-|

@docs text

@docs int, dollars

@docs date, dateTime

-}

import Time
import Ui.Table
import Ui.Theme


{-| Standard header styling.

This isn't exposed for a reason! We want to keep this detail internal to this module so there's one place where headers are styled.

-}
header : String -> Ui.Table.Header msg
header label =
    Ui.Table.cell
        [ Ui.background Ui.Theme.colors.grey100
        , Ui.Font.bold
        ]
        (Ui.text label)


{-| Standard cell styling
-}
cell : List (Ui.Attribute msg) Ui.Element msg -> Ui.Table.Cell msg
cell attrs content =
    Ui.Table.cell attrs
        content


{-| -}
text :
    { header : String
    , toText : data -> String
    }
    -> Ui.Table.Column state data msg
text options =
    Ui.Table.column
        { header = header options.header
        , view =
            \data ->
                cell [] (Ui.text (options.toText data))
        }



{-|-}
int :
    { header : String
    , toInt : data -> Int
    }
    -> Ui.Table.Column state data msg
int options ==
    number 
        { header = options.header
        , toFloat = options.toInt >> toFloat
        , format = { currency = Nothing, decimalPlaces = 0 }
        }


{-|-}
dollars :
    { header : String
    , toFloat : data -> Float
    }
    -> Ui.Table.Column state data msg
dollars options ==
    number 
        { header = options.header
        , toFloat = options.toFloat
        , format = { currency = Just "$", decimalPlaces = 2 }
        }


{- Number cell implementation -}


type alias NumberFormat =
    { currency : Maybe String
    , decimalPlaces : Int
    }


{-| -}
number :
    { header : String
    , toFloat : data -> Float
    , format : NumberFormat
    }
    -> Ui.Table.Column state data msg
number options =
    Ui.Table.column
        { header =
            header options.header
        , view =
            \data ->
                cell
                    [ Ui.Font.alignRight
                    , Ui.Font.variants
                        [ Ui.Font.tabularNumbers ]
                    ]
                    (Ui.text (formatFloat options.format (options.toFloat data)))
        }


formatFloat : NumberFormat -> Float -> String
formatFloat format float =
    case format.currency of
        Just currency ->
            currency ++ floatToString format float

        Nothing ->
            floatToString format float


floatToString : NumberFormat -> Float -> String
floatToString format float =
    if max 0 format.decimalPlaces == 0 then
        String.fromInt (floor float)

    else
        let
            topString =
                String.fromInt (floor float)
                    -- Add commas as a thousands separator
                    -- We could extend the formatter to allow for switching the period and commas
                    -- Which is commonly used in places like France and Germany.
                    |> String.foldr
                        (\char ( count, gathered ) ->
                            if count == 3 then
                                ( 1, Char.toString char ++ "," ++ gathered )

                            else
                                ( count + 1, Char.toString char ++ gathered )
                        )
                        ( 1, "" )
                    |> Tuple.second

            multiplier =
                10 ^ format.decimalPlaces

            tail =
                floor ((float - toFloat (floor float)) * multiplier)
        in
        topString ++ "." ++ String.fromInt tail


{-| -}
type DateFormat
    = Date
    | DateTime


{-| -}
date :
    { header : String
    , toTimeZone : state -> Time.Zone
    , toDate : data -> Time.Posix
    }
    -> Ui.Table.Column state data msg
date options =
    dateCell
        { header = options.header
        , toTimeZone = options.toTimeZone
        , toDate = options.toDate
        , format = Date
        }


{-| -}
dateTime :
    { header : String
    , toTimeZone : state -> Time.Zone
    , toDate : data -> Time.Posix
    }
    -> Ui.Table.Column state data msg
dateTime options =
    dateCell
        { header = options.header
        , toTimeZone = options.toTimeZone
        , toDate = options.toDate
        , format = DateTime
        }



{- Date Cell Implementation -}

dateCell :
    { header : String
    , toTimeZone : state -> Time.Zone
    , toDate : data -> Time.Posix
    , format : DateFormat
    }
    -> Ui.Table.Column state data msg
dateCell options =
    Ui.Table.columnWithState
        { header =
            \state ->
                header options.header
        , view =
            \index state data ->
                cell []
                    (Ui.text (formatDate options.format (options.toTimeZone state) (options.toDate data)))
        }


formatDate : DateFormat -> Time.Zone -> Time.Posix -> String
formatDate dateFormat zone posix =
    case dateFormat of
        Date ->
            toMonthName (Time.toMonth zone time)
                ++ " "
                ++ String.fromInt (Time.toDay zone time)
                ++ ", "
                ++ String.fromInt (Time.toYear zone time)

        DateTime ->
            toMonthName (Time.toMonth zone time)
                ++ " "
                ++ String.fromInt (Time.toDay zone time)
                ++ ", "
                ++ String.fromInt (Time.toYear zone time)
                ++ " "
                ++ String.fromInt (Time.toHour zone time)
                ++ ":"
                ++ String.fromInt (Time.toMinute zone time)
                ++ ":"
                ++ String.fromInt (Time.toSecond zone time)


toMonthName : Time.Month -> String
toMonthName month =
    case month of
        Time.Jan ->
            "Jan"

        Time.Feb ->
            "Feb"

        Time.Mar ->
            "Mar"

        Time.Apr ->
            "Apr"

        Time.May ->
            "May"

        Time.Jun ->
            "Jun"

        Time.Jul ->
            "Jul"

        Time.Aug ->
            "Aug"

        Time.Sep ->
            "Sep"

        Time.Oct ->
            "Oct"

        Time.Nov ->
            "Nov"

        Time.Dec ->
            "Dec"
