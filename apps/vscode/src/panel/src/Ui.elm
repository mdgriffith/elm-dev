module Ui exposing
    ( anim
    , space, pad
    , background, border, font
    , when, whenJust, precise, rounded, noAttr
    , overrides
    , maybeAttr
    , header, showLive, transition
    )

{-|

@docs anim
@docs space, pad

@docs background, border, font

@docs when, whenJust, precise, rounded, noAttr

@docs overrides

@docs noAttr, maybeAttr

-}

import Element as Ui exposing (Attribute, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html)
import Html.Attributes as Attr
import VSCode.Colors


showLive =
    Html.node "style" [] [ Html.text live ]


live =
    """
#live {
    display:block !important;
}

"""


overrides =
    Html.node "style" [] [ Html.text (stylesheet ++ VSCode.Colors.stylesheet) ]


stylesheet =
    """


html {
    min-height:100%;
}
body {
    background-color: var(--vscode-editor-background);
    color: var(--vscode-editor-foreground);
    /*font-family: "Fira Code" !important; */
    font-family: var(--vscode-editor-font-family);
    font-weight: var(--vscode-editor-font-weight);
    font-size: var(--vscode-editor-font-size);
    margin: 0;
    padding: 0 20px;
    min-height: 100vh;
    display:flex;
    flex-direction: column;
    justify-content: center;
    align-items: flex-start;
}
.base {
    background-color: var(--vscode-editor-background) !important;
    color: var(--vscode-editor-foreground) !important;
    font-family: var(--vscode-editor-font-family) !important;
    font-weight: var(--vscode-editor-font-weight) !important;
    font-size: var(--vscode-editor-font-size) !important;
}

@keyframes blink {
  from {opacity: 1;}
  50%  {opacity: 0.2;}
  100% {opacity: 1;}
}


.info {
    color: var(--vscode-editorInfo-foreground) !important;
}

.warning {
    color: var(--vscode-editorWarning-foreground) !important;
}

.danger {
    color: var(--vscode-editorError-foreground) !important;
}

.success {
    color: var(--vscode-testing-iconPassed) !important;
}

.blink {
    opacity:1;
    animation: blink 250ms linear;
}

.precise {
    white-space: pre !important;
}
.precise * {
    white-space: pre !important;
}

.elmsh {
    color: #f8f8f2;
    margin: 0;
    white-space: pre-wrap !important;
    /* background: #23241f; */
}
.elmsh-hl {
    background: #343434;
}
.elmsh-add {
    background: #003800;
}
.elmsh-del {
    background: #380000;
}
.elmsh-comm {
    color: #75715e;
}
.elmsh1 {
    color: #ae81ff;
}
.elmsh2 {
    color: #e6db74;
}
.elmsh3 {
    color: #f92672;
}
.elmsh4 {
    color: #66d9ef;
}
.elmsh5 {
    color: #a6e22e;
}
.elmsh6 {
    color: #ae81ff;
}
.elmsh7 {
    color: #fd971f;
}
.elmsh-elm-ts,
.elmsh-js-dk,
.elmsh-css-p {
    font-style: italic;
    color: #66d9ef;
}
.elmsh-js-ce {
    font-style: italic;
    color: #a6e22e;
}
.elmsh-css-ar-i {
    font-weight: bold;
    color: #f92672;
}



"""


colors :
    { dark :
        { dark : Ui.Color
        , light : Ui.Color
        , medium : Ui.Color
        }
    , grey :
        { dark : Ui.Color
        , light : Ui.Color
        , medium : Ui.Color
        }
    , primary : Ui.Color
    , white : Ui.Color
    , black : Ui.Color
    }
colors =
    { primary = Ui.rgb 0 0.5 0.25
    , white = Ui.rgb 1 1 1
    , grey =
        { light = Ui.rgb 0.95 0.95 0.95
        , medium = Ui.rgb 0.85 0.85 0.85
        , dark = Ui.rgb 0.55 0.55 0.55
        }
    , dark =
        { light = Ui.rgb 0.45 0.45 0.45
        , medium = Ui.rgb 0.2 0.2 0.2
        , dark = Ui.rgb 0.05 0.05 0.05
        }
    , black = Ui.rgb 0.02 0.02 0.02
    }


spaceValues =
    { zero = 0
    , sm = 5
    , md = 10
    , lg = 15
    , xl = 20
    }


mapSpacing : (a -> b) -> { c | zero : a, lg : a, md : a, sm : a, xl : a } -> { zero : b, lg : b, md : b, sm : b, xl : b }
mapSpacing fn sp =
    { zero = fn sp.zero
    , sm = fn sp.sm
    , md = fn sp.md
    , lg = fn sp.lg
    , xl = fn sp.xl
    }


space =
    mapSpacing
        Ui.spacing
        spaceValues


pad =
    { sm = Ui.padding spaceValues.sm
    , md = Ui.padding spaceValues.md
    , lg = Ui.padding spaceValues.lg
    , xl = Ui.padding spaceValues.xl
    , xy =
        spaceValues
            |> mapSpacing
                (\x ->
                    spaceValues
                        |> mapSpacing
                            (\y ->
                                Ui.paddingXY x y
                            )
                )
    }


rounded =
    { md = Border.rounded 5
    , full = Border.rounded 10000
    }


border =
    { primary = Border.color colors.primary
    , light = Border.color colors.grey.light
    , grey =
        { light = Border.color colors.grey.light
        , medium = Border.color colors.grey.medium
        , dark = Border.color colors.grey.dark
        }
    , dark =
        { light = Border.color colors.dark.light
        , medium = Border.color colors.dark.medium
        , dark = Border.color colors.dark.dark
        }
    }


background =
    { white = Background.color colors.white
    , dark = Background.color colors.dark.dark
    , black = Background.color colors.black
    }


transition =
    Ui.htmlAttribute (Attr.style "transition" "transform 100ms, opacity 100ms")


font =
    { body = Font.size 14
    , cyan =
        Ui.htmlAttribute (Attr.style "color" "cyan")
    , info =
        Ui.htmlAttribute (Attr.class "info")
    , dark =
        { light = Font.color colors.dark.light
        , medium = Font.color colors.dark.medium
        , dark = Font.color colors.dark.dark
        }
    }


anim =
    { blink = Ui.htmlAttribute (Attr.class "blink")
    }


header =
    { two =
        \str ->
            Ui.el [ Font.size 16, Font.bold ] (Ui.text str)
    , three =
        \str ->
            Ui.el [ Font.size 14, Font.bold ] (Ui.text str)
    }


precise : Attribute msg
precise =
    Ui.htmlAttribute (Attr.class "precise")


when : Bool -> Element msg -> Element msg
when condition content =
    if condition then
        content

    else
        Ui.none


whenJust : Maybe a -> (a -> Element msg) -> Element msg
whenJust maybe fn =
    case maybe of
        Nothing ->
            Ui.none

        Just a ->
            fn a


noAttr : Attribute msg
noAttr =
    Ui.htmlAttribute (Attr.class "")


maybeAttr : Maybe (Attribute msg) -> Attribute msg
maybeAttr maybe =
    case maybe of
        Nothing ->
            noAttr

        Just attr ->
            attr
