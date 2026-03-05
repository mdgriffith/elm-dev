module Ui.Button exposing
    ( Button, primary, secondary
    , withSmall, withLoading, withWidthFill
    , view, row
    )

{-|

@docs Button, primary, secondary


## Options

@docs withSmall, withLoading, withWidthFill


## Viewing

@docs view, row

-}

import Ui
import Html.Attributes as Attr
import Theme
import Theme.Color


type Button msg
    = Button (Details msg)


type alias Details msg =
    { label : String
    , style : Style
    , size : Size
    , widthFill : Bool
    , corners : Corners
    , onClick : msg
    }


type Size
    = Normal
    | Small


type Corners
    = Rounded
    | RoundedLeft
    | RoundedRight
    | Sharp


type Style
    = Primary
    | Secondary


{-| -}
primary :
    { label : String
    , onClick : msg
    }
    -> Button msg
primary options =
    Button
        { label = options.label
        , onClick = options.onClick
        , style = Primary
        , widthFill = False
        , corners = Rounded
        , size = Normal
        }


{-| -}
secondary :
    { label : String
    , onClick : msg
    }
    -> Button msg
secondary options =
    Button
        { label = options.label
        , onClick = options.onClick
        , style = Secondary
        , widthFill = False
        , corners = Rounded
        , size = Normal
        }


{-| -}
withWidthFill : Button msg -> Button msg
withWidthFill (Button details) =
    Button
        { details
            | widthFill = True
        }


{-| -}
withSmall : Button msg -> Button msg
withSmall (Button details) =
    Button
        { details
            | size = Small
        }


{-| -}
withLoading : Button msg -> Button msg
withLoading button =
    button


{-| -}
withSecondary : Button msg -> Button msg
withSecondary (Button details) =
    Button
        { details
            | style = Secondary
        }


{-| -}
view : Button msg -> Ui.Element msg
view (Button details) =
    Ui.el
        [ Ui.onClick details.onClick
        , Theme.font.body

        -- Variable styles
        , if details.widthFill then
            Ui.width Ui.fill

          else
            Ui.noAttr
        , case details.size of
            Small ->
                Theme.pad 1

            Normal ->
                Theme.pad 2
        , case details.style of
            Primary ->
                Theme.Color.backgroundPrimary

            Secondary ->
                Theme.Color.backgroundSurface
        , case details.style of
            Primary ->
                Theme.Color.textOnBrand

            Secondary ->
                Theme.Color.textDefault
        , case details.corners of
            Rounded ->
                Theme.borderRadius.sm

            RoundedLeft ->
                Ui.htmlAttribute (Attr.style "border-radius" "0 4px 4px 0")

            RoundedRight ->
                Ui.htmlAttribute (Attr.style "border-radius" "4px 0 0 4px")

            Sharp ->
                Ui.noAttr
        ]
        (Ui.text details.label)


{-| -}
row : List (Ui.Attribute msg) -> List (Button msg) -> Ui.Element msg
row attrs buttons =
    let
        buttonCount =
            List.length buttons
    in
    Ui.row attrs
        (List.indexedMap
            (\index (Button details) ->
                Button
                    { details
                        | corners =
                            if index == 0 then
                                RoundedRight

                            else if index == buttonCount - 1 then
                                RoundedLeft

                            else
                                Sharp
                    }
                    |> view
            )
            buttons
        )
