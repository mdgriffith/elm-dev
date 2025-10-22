module Example.Type exposing
    ( matches, matchesName
    , isStartingPoint, isBuilder, isBuilderOf, isCreatorOf
    , getArgs, getResultType, getBuilderOf
    , debugStartingPoint
    )

{-|

@docs matches, matchesName

@docs isStartingPoint, isBuilder, isBuilderOf, isCreatorOf

@docs getArgs, getResultType, getBuilderOf

-}

import Elm
import Elm.Docs
import Elm.Op
import Elm.Type


getResultType : Elm.Type.Type -> Elm.Type.Type
getResultType tipe =
    case tipe of
        Elm.Type.Lambda _ result ->
            getResultType result

        _ ->
            tipe


getBuilderOf : String -> Elm.Docs.Value -> Maybe Elm.Docs.Value
getBuilderOf name doc =
    if isBuilderOfName name doc.tipe then
        Just doc

    else
        Nothing


isBuilderOfName : String -> Elm.Type.Type -> Bool
isBuilderOfName name tipe =
    case tipe of
        Elm.Type.Lambda arg result ->
            if (arg |> matchesName name) && (result |> matchesName name) then
                True

            else
                isBuilderOfName name result

        _ ->
            False


getArgs : Elm.Type.Type -> List Elm.Type.Type
getArgs tipe =
    getArgsHelper tipe []


getArgsHelper : Elm.Type.Type -> List Elm.Type.Type -> List Elm.Type.Type
getArgsHelper tipe found =
    case tipe of
        Elm.Type.Lambda arg result ->
            getArgsHelper result (arg :: found)

        _ ->
            List.reverse found


isStartingPoint : Elm.Type.Type -> Bool
isStartingPoint tipe =
    if isBuilder tipe then
        False

    else
        isStartingPointHelper tipe


isStartingPointHelper : Elm.Type.Type -> Bool
isStartingPointHelper tipe =
    case tipe of
        Elm.Type.Lambda arg result ->
            if isPrimitive arg then
                isStartingPointHelper result

            else
                False

        _ ->
            True


debugStartingPoint : Elm.Type.Type -> List String
debugStartingPoint tipe =
    if isBuilder tipe then
        [ "Is Builder, skipping" ]

    else
        List.reverse (debugIsStartingPointHelper tipe [])


debugIsStartingPointHelper : Elm.Type.Type -> List String -> List String
debugIsStartingPointHelper tipe pieces =
    case tipe of
        Elm.Type.Lambda arg result ->
            if isPrimitive arg then
                debugIsStartingPointHelper result
                    (("yes, " ++ typeToString arg) :: pieces)

            else
                ("Not a primitive, skipping " ++ typeToString arg)
                    :: pieces

        _ ->
            "Dont need to see anymore, it is" :: pieces


{-| -}
isBuilder : Elm.Type.Type -> Bool
isBuilder possibleBuilder =
    case possibleBuilder of
        Elm.Type.Lambda arg result ->
            if arg |> matches result then
                True

            else
                isBuilder result

        _ ->
            False


{-| -}
isBuilderOf : Elm.Type.Type -> Elm.Type.Type -> Bool
isBuilderOf desiredResult possibleBuilder =
    case possibleBuilder of
        Elm.Type.Lambda arg result ->
            if (arg |> matches desiredResult) && (result |> matches desiredResult) then
                True

            else
                isBuilderOf desiredResult result

        _ ->
            False


isCreatorOf : String -> Elm.Type.Type -> Bool
isCreatorOf name tipe =
    case tipe of
        Elm.Type.Type typeName _ ->
            typeName == name

        Elm.Type.Lambda arg result ->
            if arg |> matchesName name then
                False

            else
                isCreatorOf name result

        _ ->
            False


matches : Elm.Type.Type -> Elm.Type.Type -> Bool
matches one two =
    case one of
        Elm.Type.Var oneVar ->
            case two of
                Elm.Type.Var twoVar ->
                    True

                _ ->
                    False

        Elm.Type.Lambda fst snd ->
            case two of
                Elm.Type.Lambda twoFst twoSnd ->
                    matches fst twoFst
                        && matches snd twoSnd

                _ ->
                    False

        Elm.Type.Tuple ones ->
            case two of
                Elm.Type.Tuple twos ->
                    listMatches ones twos

                _ ->
                    False

        Elm.Type.Type name types ->
            case two of
                Elm.Type.Type twoName twoTypes ->
                    (name == twoName)
                        && listMatches types twoTypes

                _ ->
                    False

        Elm.Type.Record fields maybeExtensible ->
            case two of
                Elm.Type.Record twoRecords twoMaybeExtensible ->
                    -- Debug.todo ""
                    False

                _ ->
                    False


listMatches : List Elm.Type.Type -> List Elm.Type.Type -> Bool
listMatches ones twos =
    case ( ones, twos ) of
        ( [], [] ) ->
            True

        ( oneTop :: oneRemain, twoTop :: twoRemain ) ->
            if matches oneTop twoTop then
                listMatches oneRemain twoRemain

            else
                False

        _ ->
            False


matchesName : String -> Elm.Type.Type -> Bool
matchesName name tipe =
    case tipe of
        Elm.Type.Type typeName _ ->
            typeName == name

        Elm.Type.Lambda arg result ->
            matchesName name arg || matchesName name result

        _ ->
            False


isPrimitive : Elm.Type.Type -> Bool
isPrimitive tipe =
    case tipe of
        Elm.Type.Var "msg" ->
            -- bit of a hack for the moment
            True

        Elm.Type.Var _ ->
            False

        Elm.Type.Lambda arg result ->
            False

        Elm.Type.Tuple tups ->
            List.all isPrimitive tups

        Elm.Type.Type name [] ->
            List.member name primitiveNames

        Elm.Type.Type name [ inner ] ->
            if List.member name primitiveSingleContainers then
                isPrimitive inner

            else
                False

        Elm.Type.Type _ _ ->
            False

        Elm.Type.Record fields maybeName ->
            List.all (Tuple.second >> isPrimitive) fields


primitiveNames : List String
primitiveNames =
    [ "String.String"
    , "Basics.Bool"
    , "Basics.Int"
    , "Basics.Float"
    ]


primitiveSingleContainers : List String
primitiveSingleContainers =
    [ "List.List"
    , "Maybe.Maybe"
    ]



{---}


typeToString : Elm.Type.Type -> String
typeToString tipe =
    typeToStringHelper 0 tipe


typeToStringHelper : Int -> Elm.Type.Type -> String
typeToStringHelper indent tipe =
    let
        indentStr =
            String.repeat (indent * 2) " "
    in
    case tipe of
        Elm.Type.Var name ->
            name

        Elm.Type.Lambda arg result ->
            let
                argStr =
                    typeToStringHelper indent arg

                resultStr =
                    typeToStringHelper indent result
            in
            "(" ++ argStr ++ "\n" ++ indentStr ++ "-> " ++ resultStr ++ ")"

        Elm.Type.Tuple types ->
            case types of
                [] ->
                    "()"

                _ ->
                    let
                        typeStrs =
                            List.map (typeToStringHelper indent) types
                    in
                    "(" ++ String.join ", " typeStrs ++ ")"

        Elm.Type.Type name typeArgs ->
            case typeArgs of
                [] ->
                    name

                _ ->
                    let
                        argStrs =
                            List.map (typeToStringHelper indent) typeArgs
                    in
                    name ++ " " ++ String.join " " argStrs

        Elm.Type.Record fields maybeExtensible ->
            let
                extensiblePrefix =
                    case maybeExtensible of
                        Just extName ->
                            extName ++ " | "

                        Nothing ->
                            ""

                fieldStrs =
                    List.map
                        (\( fieldName, fieldType ) ->
                            indentStr ++ fieldName ++ " : " ++ typeToStringHelper (indent + 1) fieldType
                        )
                        fields

                fieldsStr =
                    String.join "\n" fieldStrs
            in
            case fields of
                [] ->
                    "{}"

                _ ->
                    "{\n" ++ extensiblePrefix ++ fieldsStr ++ "\n" ++ String.repeat ((indent - 1) * 2) " " ++ "}"
