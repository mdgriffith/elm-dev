module Theme.Generate.Stylesheet exposing
    ( File, file
    , none, color, string, transition, maybe, px, int, float, fontSizeInPxAsRem
    , class, classAll, id, root, raw
    , media
    , Media, darkmode
    , ruleList
    , Selector(..), custom
    , hover, focus, active
    , Rule
    )

{-|

@docs File, file

@docs none, color, string, transition, maybe, px, int, float, fontSizeInPxAsRem

@docs class, classAll, id, root, raw

@docs media

@docs Media, darkmode

@docs ruleList

@docs Selector, custom

@docs hover, focus, active

-}

import Color


type alias File =
    { path : String
    , contents : String
    , warnings :
        List
            { declaration : String
            , warning : String
            }
    }


file : Maybe String -> List String -> List Rule -> File
file namespace path rules =
    { path = String.join "/" path
    , contents = toString namespace rules
    , warnings = []
    }


none : Rule
none =
    Prop NoProp


maybe : (a -> Rule) -> Maybe a -> Rule
maybe f m =
    case m of
        Just a ->
            f a

        Nothing ->
            none


color : String -> Color.Color -> Rule
color key c =
    Prop (Color key c)


int : String -> Int -> Rule
int key value =
    Prop (Str key (String.fromInt value))


float : String -> Float -> Rule
float key value =
    Prop (Str key (String.fromFloat value))


px : String -> Int -> Rule
px key value =
    Prop (Str key (String.fromInt value ++ "px"))


fontSizeInPxAsRem : Float -> Rule
fontSizeInPxAsRem value =
    let
        rem =
            value / 16
    in
    Prop (Str "font-size" (String.fromFloat rem ++ "rem"))


string : String -> String -> Rule
string key value =
    Prop (Str key value)


ruleList : List Rule -> Rule
ruleList =
    RuleList


media : Media -> List Rule -> Rule
media query rules =
    Rule (Media (mediaToString query)) rules


type Media
    = Darkmode


darkmode : Media
darkmode =
    Darkmode


mediaToString : Media -> String
mediaToString query =
    case query of
        Darkmode ->
            "(prefers-color-scheme: dark)"


root : List Rule -> Rule
root rules =
    Rule Root rules


class : String -> List Rule -> Rule
class name rules =
    Rule (Class name) rules


classAll : String -> List Rule -> Rule
classAll name rules =
    Rule (ClassAll name) rules


raw : String -> List Rule -> Rule
raw selector rules =
    Rule (Raw selector) rules


id : String -> List Rule -> Rule
id name rules =
    Rule (Id name) rules


hover : String -> List Rule -> Rule
hover name rules =
    Rule (Pseudo Hover (Class name)) rules


focus : String -> List Rule -> Rule
focus name rules =
    Rule (Pseudo Focus (Class name)) rules


active : String -> List Rule -> Rule
active name rules =
    Rule (Pseudo Active (Class name)) rules


transition : Int -> Rule
transition ms =
    Prop
        (Str "transition"
            (("transform " ++ String.fromInt ms ++ "ms, ")
                ++ ("opacity " ++ String.fromInt ms ++ "ms")
            )
        )


type Selector
    = Root
    | Raw String
    | Class String
    | ClassAll String
    | Id String
    | Pseudo PseudoClass Selector
    | Child Selector Selector
    | AllChildren Selector Selector
    | After Selector
    | Before Selector
    | Media String


type PseudoClass
    = Active
    | Focus
    | Hover


custom : Selector -> List Rule -> Rule
custom selector rules =
    Rule selector rules


type Rule
    = Rule Selector (List Rule)
    | RuleList (List Rule)
    | Prop Property


type CompiledRule
    = Compiled Selector (List Property)
    | CompiledMedia String (List CompiledRule)


type Property
    = Color String Color.Color
    | Str String String
    | NoProp


type alias Cursor =
    { rules : List CompiledRule
    , props : List Property
    }


empty : Cursor
empty =
    { rules = []
    , props = []
    }


flatten : Maybe Selector -> List Rule -> Cursor -> Cursor
flatten maybeParentSelector rules cursor =
    List.foldr (flattenRule maybeParentSelector) cursor rules


flattenRule : Maybe Selector -> Rule -> Cursor -> Cursor
flattenRule maybeParentSelector rule cursor =
    case rule of
        Rule (Media q) rules ->
            let
                gathered =
                    flatten maybeParentSelector rules empty

                newRule =
                    CompiledMedia q gathered.rules
            in
            { rules = newRule :: cursor.rules
            , props = cursor.props
            }

        Rule selector rules ->
            let
                newSelector =
                    case maybeParentSelector of
                        Nothing ->
                            selector

                        Just (ClassAll cls) ->
                            AllChildren (Class cls) selector

                        Just (Raw rawSelector) ->
                            Raw (rawSelector ++ selectorToString Nothing selector)

                        Just parentSelector ->
                            Child parentSelector selector

                gathered =
                    flatten (Just newSelector) rules empty

                newRule =
                    Compiled newSelector gathered.props
            in
            { rules = gathered.rules ++ newRule :: cursor.rules
            , props = cursor.props
            }

        RuleList rules ->
            flatten maybeParentSelector rules cursor

        Prop prop ->
            { cursor
                | props = prop :: cursor.props
            }


toString : Maybe String -> List Rule -> String
toString namespace rules =
    flatten Nothing rules empty
        |> .rules
        |> List.foldl (ruleToString namespace 0) ( SingleLine, "" )
        |> Tuple.second


type RuleSize
    = SingleLine
    | Multiline


ruleToString : Maybe String -> Int -> CompiledRule -> ( RuleSize, String ) -> ( RuleSize, String )
ruleToString namespace indentSize compiled ( previousSize, rendered ) =
    case compiled of
        CompiledMedia q innerRules ->
            let
                ( innerRuleSize, innerRulesRendered ) =
                    List.foldl (ruleToString namespace 2) ( SingleLine, "" ) innerRules
            in
            ( Multiline
            , rendered ++ "\n\n@media " ++ q ++ " {\n" ++ innerRulesRendered ++ "\n}"
            )

        Compiled selector props ->
            let
                indent =
                    String.repeat indentSize " "

                addToRendered size rule =
                    if String.isEmpty rendered then
                        ( size, rule )

                    else
                        case previousSize of
                            SingleLine ->
                                case size of
                                    SingleLine ->
                                        ( size, rendered ++ "\n" ++ rule )

                                    Multiline ->
                                        ( size, rendered ++ "\n\n" ++ rule )

                            Multiline ->
                                ( size, rendered ++ "\n\n" ++ rule )
            in
            if List.length props > 1 then
                let
                    renderedProps =
                        renderProps "\n" props ""

                    renderedRule =
                        indent ++ selectorToString namespace selector ++ " {\n  " ++ renderedProps ++ "}"
                in
                if String.isEmpty renderedProps then
                    ( previousSize, rendered )

                else
                    addToRendered Multiline renderedRule

            else
                let
                    renderedProps =
                        renderProps "" props ""

                    renderedRule =
                        indent ++ selectorToString namespace selector ++ " { " ++ renderedProps ++ " }"
                in
                if String.isEmpty renderedProps then
                    ( previousSize, rendered )

                else
                    addToRendered SingleLine renderedRule


renderProps : String -> List Property -> String -> String
renderProps separator props rendered =
    case props of
        [] ->
            rendered

        NoProp :: rest ->
            renderProps separator rest rendered

        prop :: rest ->
            if String.isEmpty rendered then
                renderProps separator rest (propToString prop ++ separator)

            else
                renderProps separator rest (rendered ++ "  " ++ propToString prop ++ separator)


selectorToString : Maybe String -> Selector -> String
selectorToString maybeNamespace selector =
    case selector of
        Root ->
            ":root"

        Raw rawSelector ->
            rawSelector

        Class name ->
            "." ++ withNamespace maybeNamespace name

        ClassAll name ->
            "." ++ withNamespace maybeNamespace name

        Id name ->
            "#" ++ withNamespace maybeNamespace name

        Pseudo pseudo inner ->
            selectorToString maybeNamespace inner ++ ":" ++ pseudoToString pseudo

        Child parent child ->
            selectorToString maybeNamespace parent ++ " > " ++ selectorToString maybeNamespace child

        AllChildren parent child ->
            selectorToString maybeNamespace parent ++ " " ++ selectorToString maybeNamespace child

        After inner ->
            selectorToString maybeNamespace inner ++ "::after"

        Before inner ->
            selectorToString maybeNamespace inner ++ "::before"

        Media query ->
            "@media " ++ query


withNamespace : Maybe String -> String -> String
withNamespace maybeNamespace name =
    case maybeNamespace of
        Just namespace ->
            namespace ++ "-" ++ name

        Nothing ->
            name


pseudoToString : PseudoClass -> String
pseudoToString pseudo =
    case pseudo of
        Active ->
            "active"

        Focus ->
            "focus"

        Hover ->
            "hover"


propToString : Property -> String
propToString prop =
    case prop of
        Color key clr ->
            key ++ ": " ++ Color.toCssString clr ++ ";"

        Str key value ->
            key ++ ": " ++ value ++ ";"

        NoProp ->
            ""
