module Theme exposing (..)

{-| First, a basic language for our design system
-}

import Color
import Parser exposing ((|.), (|=))
import Theme.Color


type alias Theme =
    { namespace : String
    , colors : List ColorInstance
    , target : Target
    , themes : Maybe ColorThemeDefinitions
    , spacing : List (Named Int)
    , typography : List (Named Typeface)
    , borderRadii : List (Named Int)
    , borderWidths : List (Named Int)
    }


type alias ColorThemeDefinitions =
    { default : ColorTheme
    , alternates : List (Named ColorTheme)
    }


type alias FullColorName =
    { base : String -- purple
    , alias : Maybe String -- primary
    , variant : Maybe Int -- 700
    , state : Maybe State -- hover
    , nuance : Maybe String -- subtle
    }


type State
    = Hover
    | Active
    | Focus


{-| -}
type alias ColorTheme =
    { text : List ColorDefinition
    , background : List ColorDefinition
    , border : List ColorDefinition
    }


type alias ColorDefinition =
    { name : String
    , color : ColorInstance
    , hover : Maybe ColorInstance
    , active : Maybe ColorInstance
    , focus : Maybe ColorInstance
    , theme : Maybe String
    }


colorDefintionToCssClass : Theme -> String -> ColorDefinition -> String
colorDefintionToCssClass theme propName colorDef =
    if String.isEmpty colorDef.name then
        theme.namespace ++ "-" ++ propName

    else
        theme.namespace ++ "-" ++ propName ++ "-" ++ colorDef.name


colorDefintionToCssClassNoNamespace : String -> ColorDefinition -> String
colorDefintionToCssClassNoNamespace propName colorDef =
    if String.isEmpty colorDef.name then
        propName

    else
        propName ++ "-" ++ colorDef.name


colorDefToCssVar : Theme -> String -> ColorDefinition -> Maybe State -> String
colorDefToCssVar theme propName colorDef maybeState =
    let
        stateString =
            case maybeState of
                Just state ->
                    "-" ++ stateToString state

                Nothing ->
                    ""
    in
    if String.isEmpty colorDef.name then
        theme.namespace ++ "-" ++ propName ++ stateString

    else
        theme.namespace ++ "-" ++ propName ++ "-" ++ colorDef.name ++ stateString


type Target
    = HTML
    | ElmUI


type alias ColorInstance =
    { color : Theme.Color.Color
    , name : String
    , variant : Maybe Int
    }


type Name
    = Name String


nameToString : Name -> String
nameToString (Name name) =
    name


type alias Named thing =
    { name : Name
    , item : thing
    }


toFullColorDescription : FullColorName -> String
toFullColorDescription fullColorName =
    let
        variant =
            case fullColorName.variant of
                Just v ->
                    String.fromInt v

                Nothing ->
                    ""

        state =
            case fullColorName.state of
                Just s ->
                    stateToString s

                Nothing ->
                    ""

        nuance =
            case fullColorName.nuance of
                Just n ->
                    n

                Nothing ->
                    ""
    in
    fullColorName.base ++ variant


fullColorNameToCssVar : FullColorName -> String
fullColorNameToCssVar fullColorName =
    let
        variant =
            case fullColorName.variant of
                Just v ->
                    String.fromInt v

                Nothing ->
                    ""

        state =
            case fullColorName.state of
                Just s ->
                    stateToString s

                Nothing ->
                    ""

        nuance =
            case fullColorName.nuance of
                Just n ->
                    n

                Nothing ->
                    ""
    in
    "var(--" ++ decapitalize fullColorName.base ++ variant ++ ")"


fullColorToCssClass : String -> FullColorName -> String
fullColorToCssClass functionName fullColorName =
    let
        base =
            case fullColorName.alias of
                Just alias ->
                    alias

                Nothing ->
                    fullColorName.base

        state =
            case fullColorName.state of
                Just s ->
                    stateToCssClassName s

                Nothing ->
                    ""

        nuance =
            case fullColorName.nuance of
                Just n ->
                    n

                Nothing ->
                    ""

        tail =
            let
                final =
                    case state of
                        "" ->
                            nuance

                        _ ->
                            case nuance of
                                "" ->
                                    state

                                _ ->
                                    state ++ "-" ++ nuance
            in
            if String.isEmpty final then
                ""

            else
                "-" ++ final
    in
    functionName ++ "-" ++ decapitalize base ++ tail


toFullColorName : String -> FullColorName -> String
toFullColorName functionName fullColorName =
    let
        base =
            case fullColorName.alias of
                Just alias ->
                    alias

                Nothing ->
                    fullColorName.base

        variant =
            case fullColorName.variant of
                Just v ->
                    String.fromInt v

                Nothing ->
                    ""

        state =
            case fullColorName.state of
                Just s ->
                    stateToString s

                Nothing ->
                    ""

        nuance =
            case fullColorName.nuance of
                Just n ->
                    capitalize n

                Nothing ->
                    ""
    in
    functionName ++ capitalize base ++ state ++ nuance


stateToCssClassName : State -> String
stateToCssClassName state =
    case state of
        Hover ->
            "hover"

        Active ->
            "active"

        Focus ->
            "focus"


stateToString : State -> String
stateToString state =
    case state of
        Hover ->
            "Hover"

        Active ->
            "Active"

        Focus ->
            "Focus"


toColorVar : ColorInstance -> String
toColorVar colorInstance =
    let
        var =
            "var(--" ++ decapitalize colorInstance.name ++ ")"
    in
    case colorInstance.variant of
        Just variant ->
            "oklch(from " ++ var ++ " " ++ String.fromInt variant ++ "% c h)"

        Nothing ->
            var


toColorName : ColorInstance -> String
toColorName colorInstance =
    case colorInstance.variant of
        Just variant ->
            colorInstance.name ++ String.fromInt variant

        Nothing ->
            colorInstance.name


capitalize : String -> String
capitalize str =
    let
        top =
            String.left 1 str

        remain =
            String.dropLeft 1 str
    in
    String.toUpper top ++ remain


decapitalize : String -> String
decapitalize str =
    let
        top =
            String.left 1 str

        remain =
            String.dropLeft 1 str
    in
    String.toLower top ++ remain


type alias Typeface =
    { face : String
    , fallback : List String
    , weight : ( WeightName, Int )
    , size : Int
    , lineHeight : Float
    , variants : List String
    , capitalSizing :
        Maybe
            { top : Float
            , bottom : Float
            , fontSizeByCapital : Maybe Float
            }
    }


type WeightName
    = Default
    | Regular
    | Bold
    | Light


weightNameField : WeightName -> String
weightNameField weightName =
    case weightName of
        Default ->
            "default"

        Regular ->
            "regular"

        Bold ->
            "bold"

        Light ->
            "light"


weightNameToString : WeightName -> String
weightNameToString weightName =
    case weightName of
        Default ->
            ""

        Regular ->
            "-reg"

        Bold ->
            "-bold"

        Light ->
            "-light"


type Palette thing
    = Single thing
    | Palette (List (Named thing))


type alias Shadows =
    { shadows : List (Named Shadow)
    }


type alias Shadow =
    { x : Int
    , y : Int
    , color : Color.Color
    , opacity : Int
    }
