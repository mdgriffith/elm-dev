module Ui exposing
    ( anim
    , space, pad
    , background, border, font
    , when, whenJust, precise
    , overrides
    , header
    )

{-|

@docs anim
@docs space, pad

@docs background, border, font

@docs when, whenJust, precise

@docs overrides

-}

import Element as Ui exposing (Attribute, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html
import Html.Attributes as Attr


overrides =
    Html.node "style" [] [ Html.text stylesheet ]


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
    /*font-family: "Fira Code" !important; */
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



"""


colors =
    { primary = Ui.rgb 0 0.5 0.25
    , white = Ui.rgb 1 1 1
    , grey =
        { light = Ui.rgb 0.95 0.95 0.95
        , medium = Ui.rgb 0.95 0.95 0.95
        , dark = Ui.rgb 0.95 0.95 0.95
        }
    , dark =
        { light = Ui.rgb 0.05 0.05 0.05
        , medium = Ui.rgb 0.1 0.1 0.1
        , dark = Ui.rgb 0.15 0.15 0.15
        }
    }


spaceValues =
    { sm = 5
    , md = 10
    , lg = 25
    , xl = 50
    }


mapSpacing fn sp =
    { sm = fn sp.sm
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
    { md = Border.rounded 3
    , full = Border.rounded 10000
    }


border =
    { primary = Border.color colors.primary
    , light = Border.color colors.grey.light
    , dark = Border.color colors.dark.dark
    }


background =
    { white = Background.color colors.white
    , dark = Background.color colors.dark.medium
    }


font =
    { body = Font.size 16
    , cyan =
        Ui.htmlAttribute (Attr.style "color" "cyan")
    , info =
        Ui.htmlAttribute (Attr.class "info")
    }


anim =
    { blink = Ui.htmlAttribute (Attr.class "blink")
    }


header =
    { two =
        \str ->
            Ui.el [ Font.size 25, Font.bold ] (Ui.text str)
    , three =
        \str ->
            Ui.el [ Font.size 20, Font.bold ] (Ui.text str)
    }


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
