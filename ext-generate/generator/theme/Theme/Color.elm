module Theme.Color exposing (..)

import Color
import Parser exposing ((|.), (|=))


type Color
    = Color (Maybe Adjustments) Color.Color
    | Grad String


type Adjustments
    = Lighten Int
    | AtLightness Int


atLightness : Int -> Color.Color -> Color
atLightness amount clr =
    Color (Just (AtLightness amount)) clr


toCssStringBase : Color -> String
toCssStringBase colorVal =
    case colorVal of
        Color _ clr ->
            Color.toCssString clr

        Grad gradient ->
            gradient


toCssString : Color -> String
toCssString colorVal =
    case colorVal of
        Color Nothing clr ->
            Color.toCssString clr

        Color (Just (Lighten amount)) clr ->
            let
                colorString =
                    Color.toCssString clr
            in
            "oklch(from " ++ colorString ++ " calc(l + " ++ String.fromInt amount ++ "%) c h)"

        Color (Just (AtLightness amount)) clr ->
            let
                colorString =
                    Color.toCssString clr
            in
            "oklch(from " ++ colorString ++ " " ++ String.fromInt amount ++ "% c h)"

        Grad gradient ->
            gradient


type Gradient
    = Linear Angle (List Step)
    | Radial Circle Anchor (List Step)
    | Conic Anchor Angle (List ( Angle, Color ))
    | RepeatingLinear Angle (List Step)
    | RepeatingRadial Circle Anchor (List Step)
    | RepeatingConic Anchor Angle (List ( Angle, Color ))


type Circle
    = Circle AnchorEdge
    | Ellipse AnchorEdge


type AnchorEdge
    = ClosestSide
    | ClosestCorner
    | FarthestSide
    | FarthestCorner


type Step
    = Percent Int Color.Color
    | Pixel Int Color.Color


type Anchor
    = Anchor AnchorX AnchorY Int Int


type Angle
    = Angle Float


type AnchorX
    = CenterX
    | Left
    | Right


type AnchorY
    = CenterY
    | Top
    | Bottom


cssParser : Parser.Parser Color
cssParser =
    Parser.oneOf
        [ Parser.map (Color Nothing) parseRgb
        , Parser.map (Color Nothing) parseHex
        , parseGradient
        ]


parseGradient : Parser.Parser Color
parseGradient =
    Parser.chompWhile
        (\_ -> True)
        |> Parser.getChompedString
        |> Parser.map Grad


parseRgb : Parser.Parser Color.Color
parseRgb =
    Parser.succeed Color.rgb255
        |. Parser.symbol "rgb("
        |. Parser.spaces
        |= Parser.int
        |. Parser.spaces
        |. Parser.symbol ","
        |. Parser.spaces
        |= Parser.int
        |. Parser.spaces
        |. Parser.symbol ","
        |. Parser.spaces
        |= Parser.int
        |. Parser.spaces
        |. Parser.symbol ")"


parseHex : Parser.Parser Color.Color
parseHex =
    Parser.succeed Color.rgb255
        |. Parser.symbol "#"
        |= parseHex16
        |= parseHex16
        |= parseHex16


parseHex16 : Parser.Parser Int
parseHex16 =
    Parser.succeed
        (\one two ->
            (one * 16) + two
        )
        |= hex8
        |= hex8


hex8 : Parser.Parser Int
hex8 =
    Parser.oneOf
        [ Parser.succeed 0
            |. Parser.symbol "0"
        , Parser.succeed 1
            |. Parser.symbol "1"
        , Parser.succeed 2
            |. Parser.symbol "2"
        , Parser.succeed 3
            |. Parser.symbol "3"
        , Parser.succeed 4
            |. Parser.symbol "4"
        , Parser.succeed 5
            |. Parser.symbol "5"
        , Parser.succeed 6
            |. Parser.symbol "6"
        , Parser.succeed 7
            |. Parser.symbol "7"
        , Parser.succeed 8
            |. Parser.symbol "8"
        , Parser.succeed 9
            |. Parser.symbol "9"
        , Parser.succeed 10
            |. Parser.oneOf
                [ Parser.symbol "a"
                , Parser.symbol "A"
                ]
        , Parser.succeed 11
            |. Parser.oneOf
                [ Parser.symbol "b"
                , Parser.symbol "B"
                ]
        , Parser.succeed 12
            |. Parser.oneOf
                [ Parser.symbol "c"
                , Parser.symbol "C"
                ]
        , Parser.succeed 13
            |. Parser.oneOf
                [ Parser.symbol "d"
                , Parser.symbol "D"
                ]
        , Parser.succeed 14
            |. Parser.oneOf
                [ Parser.symbol "e"
                , Parser.symbol "E"
                ]
        , Parser.succeed 15
            |. Parser.oneOf
                [ Parser.symbol "f"
                , Parser.symbol "F"
                ]
        ]
