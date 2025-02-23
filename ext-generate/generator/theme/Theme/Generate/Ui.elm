module Theme.Generate.Ui exposing (generate)

{-| -}

import Color
import Dict
import Elm
import Elm.Annotation
import Elm.Arg
import Elm.Case
import Elm.Op
import Gen.Html
import Gen.Html.Attributes
import Gen.String
import Gen.Ui
import Gen.Ui.Accessibility
import Theme
import Theme.Color
import Theme.Generate.Stylesheet as Style


generate : Theme.Theme -> List Elm.File
generate theme =
    [ stylesheet theme
    , generateElmColorPalette theme
    , generateElmColorTheme theme
    , generateTextElements theme
    , generateTheme theme
    ]


addNamespace : String -> String -> String
addNamespace namespace name =
    if namespace == "" then
        name

    else
        namespace ++ "-" ++ name


generateTheme : Theme.Theme -> Elm.File
generateTheme theme =
    Elm.file [ "Theme" ]
        [ helpers theme
        , typography theme
        , layout theme
        , spacing theme
        , borders theme
        ]


helpers : Theme.Theme -> Elm.Declaration
helpers theme =
    let
        themeList =
            [ Nothing, Just "darkmode" ]
    in
    Elm.group
        [ Elm.customType "Mode"
            (themeList
                |> List.map
                    (\maybe ->
                        case maybe of
                            Nothing ->
                                Elm.variant "Default"

                            Just name ->
                                Elm.variant (capitalize name)
                    )
            )
            |> Elm.exposeConstructor
        , Elm.declaration "setMode"
            (Elm.fn
                (Elm.Arg.varWith "mode" (Elm.Annotation.named [] "Mode"))
                (\mode ->
                    Elm.Case.custom mode
                        (Elm.Annotation.named [] "Mode")
                        (themeList
                            |> List.map
                                (\maybe ->
                                    case maybe of
                                        Nothing ->
                                            Elm.Case.branch (Elm.Arg.customType "Default" ())
                                                (\_ ->
                                                    Gen.Html.Attributes.class
                                                        (theme.namespace ++ "-automode")
                                                )

                                        Just modeName ->
                                            Elm.Case.branch (Elm.Arg.customType (capitalize modeName) ())
                                                (\_ ->
                                                    Gen.Html.Attributes.class
                                                        (theme.namespace ++ "-" ++ modeName)
                                                )
                                )
                        )
                )
            )
            |> Elm.expose
        ]


attrBorderWidthsType target =
    Elm.Annotation.namedWith [] "BorderWidths" [ attrType target ]


type Side
    = All
    | Top
    | Right
    | Bottom
    | Left


sideToString : Side -> String
sideToString side =
    case side of
        All ->
            "all"

        Top ->
            "top"

        Right ->
            "right"

        Bottom ->
            "bottom"

        Left ->
            "left"


border target side =
    Elm.fn (Elm.Arg.varWith "width" Elm.Annotation.int)
        (toBorder target side)


borders : Theme.Theme -> Elm.Declaration
borders theme =
    Elm.group
        [ Elm.alias "BorderWidths"
            (toFieldsType (\_ -> Elm.Annotation.var "item") theme.borderWidths)
        , Elm.declaration "mapBorderWidths"
            (Elm.fn
                (Elm.Arg.var "f")
                (\f ->
                    Elm.record
                        (toFields
                            (\s ->
                                Elm.apply f [ Elm.int s ]
                            )
                            theme.borderWidths
                        )
                )
            )
        , Elm.declaration "borderWidth"
            ([ toFields
                (\int -> toBorder theme.target All (Elm.int int))
                theme.borderWidths
             , [ ( "top"
                 , Elm.apply (Elm.val "mapBorderWidths") [ border theme.target Top ]
                    |> Elm.withType (attrBorderWidthsType theme.target)
                 )
               , ( "right"
                 , Elm.apply (Elm.val "mapBorderWidths") [ border theme.target Right ]
                    |> Elm.withType (attrBorderWidthsType theme.target)
                 )
               , ( "bottom"
                 , Elm.apply (Elm.val "mapBorderWidths") [ border theme.target Bottom ]
                    |> Elm.withType (attrBorderWidthsType theme.target)
                 )
               , ( "left"
                 , Elm.apply (Elm.val "mapBorderWidths") [ border theme.target Left ]
                    |> Elm.withType (attrBorderWidthsType theme.target)
                 )
               ]
             ]
                |> List.concat
                |> Elm.record
            )
            |> Elm.exposeConstructor
        , Elm.declaration "borderRadius"
            ([ toFields (\int -> toBorderRadius theme.target All (Elm.int int))
                theme.borderRadii
             , [ ( "top"
                 , toFields (\radii -> toBorderRadius theme.target Top (Elm.int radii))
                    theme.borderRadii
                    |> Elm.record
                 )
               , ( "right"
                 , toFields (\radii -> toBorderRadius theme.target Right (Elm.int radii))
                    theme.borderRadii
                    |> Elm.record
                 )
               , ( "bottom"
                 , toFields (\radii -> toBorderRadius theme.target Bottom (Elm.int radii))
                    theme.borderRadii
                    |> Elm.record
                 )
               , ( "left"
                 , toFields (\radii -> toBorderRadius theme.target Left (Elm.int radii))
                    theme.borderRadii
                    |> Elm.record
                 )
               ]
             ]
                |> List.concat
                |> Elm.record
            )
            |> Elm.exposeConstructor
        ]


layout : Theme.Theme -> Elm.Declaration
layout theme =
    case theme.target of
        Theme.HTML ->
            Elm.group
                [ Elm.declaration "el"
                    (Elm.fn2
                        (Elm.Arg.var "attrs")
                        (Elm.Arg.var "child")
                        (\attrs child ->
                            Gen.Html.call_.div attrs (Elm.list [ child ])
                        )
                    )
                    |> Elm.exposeConstructor
                , Elm.declaration "row"
                    (Elm.record
                        (toFields
                            (\space ->
                                Elm.fn2
                                    (Elm.Arg.var "attrs")
                                    (Elm.Arg.var "children")
                                    (\attrs children ->
                                        Gen.Html.call_.div
                                            (attrs
                                                |> Elm.Op.cons (toSpacing theme.target space)
                                                |> Elm.Op.cons (Gen.Html.Attributes.style "display" "flex")
                                                |> Elm.Op.cons (Gen.Html.Attributes.style "flex-direction" "row")
                                            )
                                            children
                                    )
                            )
                            theme.spacing
                        )
                    )
                    |> Elm.exposeConstructor
                , Elm.declaration "column"
                    (Elm.record
                        (toFields
                            (\space ->
                                Elm.fn2
                                    (Elm.Arg.var "attrs")
                                    (Elm.Arg.var "children")
                                    (\attrs children ->
                                        Gen.Html.call_.div
                                            (attrs
                                                |> Elm.Op.cons (toSpacing theme.target space)
                                                |> Elm.Op.cons (Gen.Html.Attributes.style "display" "flex")
                                                |> Elm.Op.cons (Gen.Html.Attributes.style "flex-direction" "column")
                                            )
                                            children
                                    )
                            )
                            theme.spacing
                        )
                    )
                    |> Elm.exposeConstructor
                ]

        Theme.ElmUI ->
            Elm.group
                [ Elm.declaration "el"
                    Gen.Ui.values_.el
                    |> Elm.exposeConstructor
                , Elm.declaration "row"
                    (Elm.record
                        (toFields
                            (\space ->
                                Elm.fn2
                                    (Elm.Arg.var "attrs")
                                    (Elm.Arg.var "children")
                                    (\attrs children ->
                                        Gen.Ui.call_.row (Elm.Op.cons (Gen.Ui.spacing space) attrs) children
                                    )
                            )
                            theme.spacing
                        )
                    )
                    |> Elm.exposeConstructor
                , Elm.declaration "column"
                    (Elm.record
                        (toFields
                            (\space ->
                                Elm.fn2
                                    (Elm.Arg.var "attrs")
                                    (Elm.Arg.var "children")
                                    (\attrs children ->
                                        Gen.Ui.call_.column (Elm.Op.cons (Gen.Ui.spacing space) attrs) children
                                    )
                            )
                            theme.spacing
                        )
                    )
                    |> Elm.exposeConstructor
                ]


attrSpacingType target =
    Elm.Annotation.namedWith [] "Spaced" [ attrType target ]


padding : Theme.Target -> Int -> Elm.Expression
padding target int =
    case target of
        Theme.HTML ->
            Gen.Html.Attributes.style "padding" (px int)

        Theme.ElmUI ->
            Gen.Ui.padding int


px : Int -> String
px p =
    String.fromInt p ++ "px"


callPx : Elm.Expression -> Elm.Expression
callPx p =
    Elm.Op.append (Gen.String.call_.fromInt p) (Elm.string "px")


toPaddingXY : Theme.Target -> Elm.Expression -> Elm.Expression -> Elm.Expression
toPaddingXY target x y =
    case target of
        Theme.HTML ->
            Gen.Html.Attributes.call_.style (Elm.string "padding")
                (Elm.Op.append
                    (Elm.Op.append (callPx y)
                        (Elm.string " ")
                    )
                    (callPx x)
                )

        Theme.ElmUI ->
            Gen.Ui.call_.paddingXY x y


padTop : Theme.Target -> Elm.Expression
padTop target =
    case target of
        Theme.HTML ->
            Elm.fn
                (Elm.Arg.var "px")
                (\v ->
                    Gen.Html.Attributes.call_.style
                        (Elm.string "padding-top")
                        (callPx v)
                )

        Theme.ElmUI ->
            Gen.Ui.values_.paddingTop


padBottom : Theme.Target -> Elm.Expression
padBottom target =
    case target of
        Theme.HTML ->
            Elm.fn
                (Elm.Arg.var "px")
                (\v ->
                    Gen.Html.Attributes.call_.style
                        (Elm.string "padding-bottom")
                        (callPx v)
                )

        Theme.ElmUI ->
            Gen.Ui.values_.paddingBottom


padLeft : Theme.Target -> Elm.Expression
padLeft target =
    case target of
        Theme.HTML ->
            Elm.fn
                (Elm.Arg.var "px")
                (\v ->
                    Gen.Html.Attributes.call_.style
                        (Elm.string "padding-left")
                        (callPx v)
                )

        Theme.ElmUI ->
            Gen.Ui.values_.paddingLeft


padRight : Theme.Target -> Elm.Expression
padRight target =
    case target of
        Theme.HTML ->
            Elm.fn
                (Elm.Arg.var "px")
                (\v ->
                    Gen.Html.Attributes.call_.style
                        (Elm.string "padding-right")
                        (callPx v)
                )

        Theme.ElmUI ->
            Gen.Ui.values_.paddingRight


toSpacing : Theme.Target -> Int -> Elm.Expression
toSpacing target int =
    case target of
        Theme.HTML ->
            Gen.Html.Attributes.style "gap" (String.fromInt int ++ "px")

        Theme.ElmUI ->
            Gen.Ui.spacing int


spacing : Theme.Theme -> Elm.Declaration
spacing theme =
    Elm.group
        [ Elm.declaration "space"
            (Elm.record
                (toFields Elm.int
                    theme.spacing
                )
            )
        , Elm.declaration "mapSpace"
            (Elm.fn
                (Elm.Arg.var "f")
                (\f ->
                    Elm.record
                        (toFields
                            (\s ->
                                Elm.apply f [ Elm.int s ]
                            )
                            theme.spacing
                        )
                )
            )
        , Elm.declaration "gap"
            (Elm.record
                (toFields (attr theme.target << toSpacing theme.target)
                    theme.spacing
                )
            )
            |> Elm.exposeConstructor
        , Elm.alias "Spaced"
            (toFieldsType (\_ -> Elm.Annotation.var "item") theme.spacing)
        , Elm.declaration "pad"
            (Elm.record
                (toFields (attr theme.target << padding theme.target)
                    theme.spacing
                    ++ [ ( "xy"
                         , Elm.apply (Elm.val "mapSpace")
                            [ Elm.fn
                                (Elm.Arg.varWith "spacingX" Elm.Annotation.int)
                                (\spacingX ->
                                    Elm.apply (Elm.val "mapSpace")
                                        [ Elm.fn
                                            (Elm.Arg.varWith "spacingY" Elm.Annotation.int)
                                            (\spacingY ->
                                                toPaddingXY theme.target spacingX spacingY
                                            )
                                        ]
                                )
                            ]
                            |> Elm.withType
                                (Elm.Annotation.namedWith []
                                    "Spaced"
                                    [ attrSpacingType theme.target
                                    ]
                                )
                         )
                       , ( "top"
                         , Elm.apply (Elm.val "mapSpace") [ padTop theme.target ]
                            |> Elm.withType (attrSpacingType theme.target)
                         )
                       , ( "right"
                         , Elm.apply (Elm.val "mapSpace") [ padRight theme.target ]
                            |> Elm.withType (attrSpacingType theme.target)
                         )
                       , ( "bottom"
                         , Elm.apply (Elm.val "mapSpace") [ padBottom theme.target ]
                            |> Elm.withType (attrSpacingType theme.target)
                         )
                       , ( "left"
                         , Elm.apply (Elm.val "mapSpace") [ padLeft theme.target ]
                            |> Elm.withType (attrSpacingType theme.target)
                         )
                       ]
                )
            )
            |> Elm.exposeConstructor
        ]


typography : Theme.Theme -> Elm.Declaration
typography theme =
    Elm.group
        [ Elm.declaration "font"
            (theme.typography
                |> List.foldl
                    (\typeface gathered ->
                        let
                            basename =
                                Theme.nameToString typeface.name

                            fullClassName =
                                -- className theme.namespace typeface.name (Theme.weightNameToString (Tuple.first typeface.item.weight))
                                typographyClassName typeface.name (Tuple.first typeface.item.weight)
                                    |> addNamespace theme.namespace
                                    |> classAttr theme.target

                            innerName =
                                Theme.weightNameField (Tuple.first typeface.item.weight)
                        in
                        case Tuple.first typeface.item.weight of
                            Theme.Default ->
                                Dict.insert basename
                                    [ ( innerName, fullClassName )
                                    ]
                                    gathered

                            _ ->
                                Dict.update basename
                                    (\maybe ->
                                        case maybe of
                                            Just fields ->
                                                Just (( innerName, fullClassName ) :: fields)

                                            Nothing ->
                                                Just
                                                    [ ( innerName, fullClassName )
                                                    ]
                                    )
                                    gathered
                    )
                    Dict.empty
                |> Dict.foldl
                    (\name fields typographyRecord ->
                        case fields of
                            [] ->
                                typographyRecord

                            [ single ] ->
                                ( name, Tuple.second single )
                                    :: typographyRecord

                            many ->
                                ( name, Elm.record many )
                                    :: typographyRecord
                    )
                    []
                |> Elm.record
            )
            |> Elm.exposeConstructor
        ]


capitalize : String -> String
capitalize str =
    let
        top =
            String.left 1 str

        remain =
            String.dropLeft 1 str
    in
    String.toUpper top ++ remain


toHtmlHeaderNode : Theme.Named Theme.Typeface -> (Elm.Expression -> Elm.Expression -> Elm.Expression)
toHtmlHeaderNode typeface =
    case Theme.nameToString typeface.name of
        "h1" ->
            Gen.Html.call_.h1

        "h2" ->
            Gen.Html.call_.h2

        "h3" ->
            Gen.Html.call_.h3

        "h4" ->
            Gen.Html.call_.h4

        "h5" ->
            Gen.Html.call_.h5

        "h6" ->
            Gen.Html.call_.h6

        _ ->
            Gen.Html.call_.div


getHeaderAttr : String -> Maybe Elm.Expression
getHeaderAttr cls =
    String.split "-" cls
        |> List.foldl
            (\val found ->
                case found of
                    Nothing ->
                        case val of
                            "h1" ->
                                Just Gen.Ui.Accessibility.h1

                            "h2" ->
                                Just Gen.Ui.Accessibility.h2

                            "h3" ->
                                Just Gen.Ui.Accessibility.h3

                            "h4" ->
                                Just Gen.Ui.Accessibility.h4

                            "h5" ->
                                Just Gen.Ui.Accessibility.h5

                            "h6" ->
                                Just Gen.Ui.Accessibility.h6

                            _ ->
                                Nothing

                    Just _ ->
                        found
            )
            Nothing


generateTextElements : Theme.Theme -> Elm.File
generateTextElements theme =
    Elm.file [ "Theme", "Text" ]
        (theme.typography
            |> List.foldr
                (\typeface gathered ->
                    let
                        basename =
                            Theme.nameToString typeface.name

                        fullClassName =
                            typographyClassName typeface.name (Tuple.first typeface.item.weight)
                                |> addNamespace theme.namespace

                        fullClassAttr =
                            classAttr theme.target fullClassName

                        addAcccessibilityAttrs : Elm.Expression -> Elm.Expression
                        addAcccessibilityAttrs attrs =
                            case getHeaderAttr fullClassName of
                                Nothing ->
                                    attrs

                                Just headerAttr ->
                                    Elm.Op.cons
                                        headerAttr
                                        attrs

                        elFn =
                            case theme.target of
                                Theme.HTML ->
                                    Elm.fn2
                                        (Elm.Arg.varWith "attrs"
                                            (Elm.Annotation.list
                                                (Gen.Html.annotation_.attribute
                                                    (Elm.Annotation.var "msg")
                                                )
                                            )
                                        )
                                        (Elm.Arg.varWith "child" Elm.Annotation.string)
                                        (\attrs child ->
                                            toHtmlHeaderNode typeface
                                                (Elm.Op.cons fullClassAttr attrs)
                                                (Elm.list [ Gen.Html.call_.text child ])
                                        )

                                Theme.ElmUI ->
                                    Elm.fn2
                                        (Elm.Arg.var "attrs")
                                        (Elm.Arg.varWith "child" Elm.Annotation.string)
                                        (\attrs child ->
                                            Gen.Ui.call_.el
                                                (addAcccessibilityAttrs (Elm.Op.cons fullClassAttr attrs))
                                                (Gen.Ui.call_.text child)
                                        )

                        innerName =
                            Theme.weightNameField (Tuple.first typeface.item.weight)
                    in
                    case Tuple.first typeface.item.weight of
                        Theme.Default ->
                            Dict.insert basename
                                [ ( innerName, elFn )
                                ]
                                gathered

                        _ ->
                            Dict.update basename
                                (\maybe ->
                                    case maybe of
                                        Just fields ->
                                            Just (( innerName, elFn ) :: fields)

                                        Nothing ->
                                            Just
                                                [ ( innerName, elFn )
                                                ]
                                )
                                gathered
                )
                Dict.empty
            |> Dict.foldl
                (\name fields typographyRecord ->
                    case fields of
                        [] ->
                            typographyRecord

                        [ single ] ->
                            (Elm.declaration name (Tuple.second single)
                                |> Elm.expose
                            )
                                :: typographyRecord

                        many ->
                            let
                                new =
                                    List.map
                                        (\( innerName, body ) ->
                                            Elm.declaration (name ++ capitalize innerName) body
                                                |> Elm.expose
                                        )
                                        many
                                        |> Elm.group
                            in
                            new :: typographyRecord
                )
                []
        )


generateElmColorPalette : Theme.Theme -> Elm.File
generateElmColorPalette theme =
    Elm.file [ "Theme", "Color", "Palette" ]
        (List.foldl
            (\colorInstance list ->
                case theme.target of
                    Theme.HTML ->
                        case colorInstance.color of
                            Theme.Color.Color adjust clr ->
                                (Elm.declaration (Theme.toColorName colorInstance) (toColor theme.target clr)
                                    |> Elm.expose
                                )
                                    :: list

                            Theme.Color.Grad _ ->
                                list

                    Theme.ElmUI ->
                        case colorInstance.color of
                            Theme.Color.Color adjust clr ->
                                (Elm.declaration (Theme.toColorName colorInstance) (toColor theme.target clr)
                                    |> Elm.expose
                                )
                                    :: list

                            Theme.Color.Grad _ ->
                                list
            )
            []
            theme.colors
        )


generateElmColorTheme : Theme.Theme -> Elm.File
generateElmColorTheme theme =
    Elm.file [ "Theme", "Color" ]
        (case theme.themes of
            Nothing ->
                []

            Just themes ->
                let
                    toStyles : String -> List Theme.ColorDefinition -> Elm.Declaration
                    toStyles propName colorDefs =
                        colorDefs
                            |> List.concatMap
                                (\colorDef ->
                                    let
                                        className =
                                            Theme.colorDefintionToCssClass theme propName colorDef
                                    in
                                    [ ( className
                                      , Elm.declaration (propName ++ capitalize colorDef.name)
                                            (case theme.target of
                                                Theme.HTML ->
                                                    Gen.Html.Attributes.class className

                                                Theme.ElmUI ->
                                                    Gen.Ui.htmlAttribute
                                                        (Gen.Html.Attributes.class className)
                                            )
                                            |> Elm.expose
                                      )
                                    ]
                                )
                            |> List.sortBy Tuple.first
                            |> List.map Tuple.second
                            |> Elm.group
                in
                [ Elm.comment " Text "
                , toStyles "text" themes.default.text
                , Elm.comment " Backgrounds "
                , toStyles "background" themes.default.background
                , Elm.comment " Borders "
                , toStyles "border" themes.default.border
                ]
        )



{- HELPERS -}


toFieldsType : (thing -> Elm.Annotation.Annotation) -> List (Theme.Named thing) -> Elm.Annotation.Annotation
toFieldsType toType fields =
    fields
        |> List.map
            (\named ->
                ( Theme.nameToString named.name, toType named.item )
            )
        |> Elm.Annotation.record


toFields : (thing -> Elm.Expression) -> List (Theme.Named thing) -> List ( String, Elm.Expression )
toFields toExp fields =
    fields
        |> List.map
            (\item -> field item toExp)


record : (thing -> Elm.Expression) -> List (Theme.Named thing) -> Elm.Expression
record toExp fields =
    Elm.record
        (fields
            |> List.map
                (\item -> field item toExp)
        )


field : Theme.Named thing -> (thing -> Elm.Expression) -> ( String, Elm.Expression )
field named toVal =
    ( Theme.nameToString named.name, toVal named.item )



{---}


stylesheet : Theme.Theme -> Elm.File
stylesheet theme =
    Style.file (Just theme.namespace)
        [ "elm-ui.css" ]
        (List.concat
            [ colorStyles theme
            , typographyStyles theme
            ]
        )


colorStyles : Theme.Theme -> List Style.Rule
colorStyles theme =
    case theme.themes of
        Nothing ->
            []

        Just themes ->
            let
                defaultColorRules =
                    generateColorClasses theme themes.default

                ( darkModeThemes, otherThemes ) =
                    List.partition (\t -> Theme.nameToString t.name == "darkmode") themes.alternates

                darkModeColorRules =
                    case darkModeThemes of
                        darkTheme :: _ ->
                            generateColorClasses theme darkTheme.item

                        [] ->
                            -- Autogenerate dark mode
                            -- generateColorClasses theme themes.default
                            []

                darkModeMediaQuery =
                    if List.isEmpty darkModeColorRules then
                        Style.none

                    else
                        Style.media Style.darkmode
                            [ Style.classAll "automode" darkModeColorRules ]
            in
            [ colorVars theme.colors
            , Style.ruleList defaultColorRules
            , Style.classAll "darkmode" darkModeColorRules
            , Style.ruleList
                (otherThemes
                    |> List.map
                        (\other ->
                            let
                                themeName =
                                    Theme.nameToString other.name
                            in
                            Style.classAll themeName
                                (generateColorClasses theme other.item)
                        )
                )
            , darkModeMediaQuery
            ]


colorVars : List Theme.ColorInstance -> Style.Rule
colorVars colors =
    colors
        |> List.foldl
            (\clr dict ->
                let
                    varName =
                        "--" ++ clr.name
                in
                if not (Dict.member varName dict) then
                    Dict.insert varName
                        (Style.string varName (Theme.Color.toCssStringBase clr.color))
                        dict

                else
                    dict
            )
            Dict.empty
        |> Dict.values
        |> Style.root


generateColorClasses : Theme.Theme -> Theme.ColorTheme -> List Style.Rule
generateColorClasses fullTheme theme =
    let
        genColorClass colorType propName colors =
            List.map
                (\colorDef ->
                    let
                        className =
                            Theme.colorDefintionToCssClassNoNamespace colorType colorDef
                    in
                    Style.ruleList
                        -- Base color
                        [ Style.class className
                            [ Style.string propName (Theme.toColorVar colorDef.color)
                            ]

                        -- Hover
                        , case colorDef.hover of
                            Nothing ->
                                Style.none

                            Just hoverColor ->
                                Style.hover className
                                    [ Style.string propName
                                        (Theme.toColorVar hoverColor)
                                    ]

                        -- Active
                        , case colorDef.active of
                            Nothing ->
                                Style.none

                            Just activeColor ->
                                Style.active className
                                    [ Style.string propName
                                        (Theme.toColorVar activeColor)
                                    ]

                        -- Focus
                        , case colorDef.focus of
                            Nothing ->
                                Style.none

                            Just focusColor ->
                                Style.active className
                                    [ Style.string propName
                                        (Theme.toColorVar focusColor)
                                    ]
                        ]
                )
                colors
                |> Style.ruleList
    in
    [ genColorClass "text" "color" theme.text
    , genColorClass "background" "background-color" theme.background
    , genColorClass "border" "border-color" theme.border
    ]


brighten : Int -> Theme.FullColorName -> Theme.FullColorName
brighten amount fullColorName =
    case fullColorName.variant of
        Nothing ->
            fullColorName

        Just variant ->
            { fullColorName | variant = Just (roundToNearestSlot (variant + amount)) }


{-| The allowed values are 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 95
-}
roundToNearestSlot : Int -> Int
roundToNearestSlot value =
    let
        bounded =
            max 5 (min 95 value)
    in
    -- Round to the nearest allowed values are 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 95
    -- without using pattern matching
    case bounded // 10 of
        0 ->
            5

        1 ->
            10

        2 ->
            20

        3 ->
            30

        4 ->
            40

        5 ->
            50

        6 ->
            60

        7 ->
            70

        8 ->
            80

        9 ->
            90

        10 ->
            95

        _ ->
            bounded



{- Typography -}


typographyClassName : Theme.Name -> Theme.WeightName -> String
typographyClassName name weight =
    "font-" ++ Theme.nameToString name ++ Theme.weightNameToString weight


typographyStyles : Theme.Theme -> List Style.Rule
typographyStyles theme =
    theme.typography
        |> List.concatMap
            (\{ name, item } ->
                let
                    ( textTransform, italic, variants ) =
                        captureVariants Nothing False [] item.variants

                    fontSize =
                        case Maybe.andThen .fontSizeByCapital item.capitalSizing of
                            Nothing ->
                                toFloat item.size

                            Just ratio ->
                                toFloat item.size * ratio

                    fontClass =
                        typographyClassName name (Tuple.first item.weight)

                    capitalSizingStyles =
                        case item.capitalSizing of
                            Nothing ->
                                Style.none

                            Just capitalSizing ->
                                if theme.target == Theme.HTML then
                                    Style.none

                                else
                                    Style.ruleList
                                        [ Style.custom
                                            (Style.AllChildren
                                                (Style.Class fontClass)
                                                -- Paragraph class in elm-ui
                                                (Style.Class "p")
                                                |> Style.After
                                            )
                                            [ Style.string "content" "\" \""
                                            , Style.string "margin-top"
                                                -- autocalc is :  "calc((1lh - 1cap) / -2)"
                                                ("calc(1em * " ++ String.fromFloat capitalSizing.bottom ++ ")")
                                            , Style.string "display" "table"
                                            ]
                                        , Style.custom
                                            (Style.AllChildren
                                                (Style.Class fontClass)
                                                -- Paragraph class in elm-ui
                                                (Style.Class "p")
                                                |> Style.Before
                                            )
                                            [ Style.string "content" "\" \""
                                            , Style.string "margin-bottom"
                                                -- autocalc is :  "calc((1lh - 1cap) / -2)"
                                                ("calc(1em * " ++ String.fromFloat capitalSizing.top ++ ")")
                                            , Style.string "display" "table"
                                            ]
                                        , Style.custom
                                            (Style.AllChildren
                                                (Style.Class fontClass)
                                                -- text class in elm-ui
                                                (Style.Class "t")
                                                |> Style.After
                                            )
                                            [ Style.string "content" "\" \""
                                            , Style.string "margin-top"
                                                -- autocalc is :  "calc((1lh - 1cap) / -2)"
                                                ("calc(1em * " ++ String.fromFloat capitalSizing.bottom ++ ")")
                                            , Style.string "display" "table"
                                            ]
                                        , Style.custom
                                            (Style.AllChildren
                                                (Style.Class fontClass)
                                                -- text class in elm-ui
                                                (Style.Class "t")
                                                |> Style.Before
                                            )
                                            [ Style.string "content" "\" \""
                                            , Style.string "margin-bottom"
                                                -- autocalc is :  "calc((1lh - 1cap) / -2)"
                                                ("calc(1em * " ++ String.fromFloat capitalSizing.top ++ ")")
                                            , Style.string "display" "table"
                                            ]
                                        ]
                in
                [ Style.class fontClass
                    [ Style.string "font-family" (fontFamily (item.face :: item.fallback))
                    , Style.int "font-weight" (Tuple.second item.weight)
                    , Style.fontSizeInPxAsRem fontSize
                    , Style.float "line-height" item.lineHeight
                    , if italic then
                        Style.string "font-style" "italic"

                      else
                        Style.none
                    , case textTransform of
                        Just transform ->
                            Style.string "text-transform" transform

                        Nothing ->
                            Style.none
                    , case variants of
                        [] ->
                            Style.none

                        _ ->
                            Style.string "font-variant" (String.join " " variants)
                    ]
                , capitalSizingStyles
                ]
            )


captureVariants :
    Maybe String
    -> Bool
    -> List String
    -> List String
    -> ( Maybe String, Bool, List String )
captureVariants textTransform italic variants vars =
    case vars of
        [] ->
            ( textTransform, italic, variants )

        var :: rest ->
            case var of
                "italic" ->
                    captureVariants textTransform True variants rest

                "uppercase" ->
                    captureVariants (Just "uppercase") italic variants rest

                "lowercase" ->
                    captureVariants (Just "lowercase") italic variants rest

                "capitalize" ->
                    captureVariants (Just "capitalize") italic variants rest

                _ ->
                    captureVariants textTransform italic (var :: variants) rest


fontFamily : List String -> String
fontFamily fonts =
    String.join ", " (List.map (\f -> "\"" ++ f ++ "\"") fonts)



{- Helpers that vary by target (e.g. html vs elm-ui) -}


classAttr : Theme.Target -> String -> Elm.Expression
classAttr target name =
    case target of
        Theme.HTML ->
            Gen.Html.Attributes.class name

        Theme.ElmUI ->
            Gen.Ui.htmlAttribute
                (Gen.Html.Attributes.class name)


attr : Theme.Target -> Elm.Expression -> Elm.Expression
attr target a =
    a
        |> Elm.withType (attrType target)


attrType : Theme.Target -> Elm.Annotation.Annotation
attrType target =
    case target of
        Theme.HTML ->
            Gen.Html.annotation_.attribute (Elm.Annotation.var "msg")

        Theme.ElmUI ->
            Gen.Ui.annotation_.attribute (Elm.Annotation.var "msg")


to255 : Float -> Int
to255 value =
    round (value * 255)


toColor : Theme.Target -> Color.Color -> Elm.Expression
toColor target clr =
    let
        rgb =
            Color.toRgba clr
    in
    case target of
        Theme.HTML ->
            Elm.record
                [ ( "red", Elm.int (to255 rgb.red) )
                , ( "green", Elm.int (to255 rgb.green) )
                , ( "blue", Elm.int (to255 rgb.blue) )
                ]

        Theme.ElmUI ->
            Gen.Ui.rgb
                (to255 rgb.red)
                (to255 rgb.green)
                (to255 rgb.blue)


toBorder : Theme.Target -> Side -> Elm.Expression -> Elm.Expression
toBorder target side widthInt =
    case target of
        Theme.HTML ->
            case side of
                All ->
                    Gen.Html.Attributes.call_.style (Elm.string ("border-" ++ sideToString side ++ "-width"))
                        (Elm.Op.append (Gen.String.call_.fromInt widthInt) (Elm.string "px"))

                _ ->
                    Gen.Html.Attributes.call_.style (Elm.string ("border-" ++ sideToString side ++ "-width"))
                        (Elm.Op.append (Gen.String.call_.fromInt widthInt) (Elm.string "px"))

        Theme.ElmUI ->
            case side of
                All ->
                    Gen.Ui.call_.border widthInt

                _ ->
                    Gen.Ui.htmlAttribute
                        (Gen.Html.Attributes.call_.style (Elm.string ("border-" ++ sideToString side ++ "-width"))
                            (Elm.Op.append (Gen.String.call_.fromInt widthInt) (Elm.string "px"))
                        )


toBorderRadius : Theme.Target -> Side -> Elm.Expression -> Elm.Expression
toBorderRadius target side widthInt =
    case target of
        Theme.HTML ->
            case side of
                All ->
                    Gen.Html.Attributes.call_.style (Elm.string "border-width")
                        (Elm.Op.append (Gen.String.call_.fromInt widthInt) (Elm.string "px"))

                _ ->
                    Gen.Html.Attributes.call_.style (Elm.string ("border-" ++ sideToString side ++ "-width"))
                        (Elm.Op.append (Gen.String.call_.fromInt widthInt) (Elm.string "px"))

        Theme.ElmUI ->
            case side of
                All ->
                    Gen.Ui.call_.border widthInt

                _ ->
                    Gen.Ui.htmlAttribute
                        (Gen.Html.Attributes.call_.style (Elm.string ("border-" ++ sideToString side ++ "-width"))
                            (Elm.Op.append (Gen.String.call_.fromInt widthInt) (Elm.string "px"))
                        )
