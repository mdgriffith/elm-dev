module Type exposing (Type(..), AliasInfo, expand)

{-|

@docs Type, AliasInfo, expand

-}

import Dict exposing (Dict)
import Elm.Docs
import Elm.Type


{-| The same as Elm.Type.Type but with `Aliased` info added.

<https://package.elm-lang.org/packages/elm/project-metadata-utils/latest/Elm-Type>

  - `Aliased` has both the original alias used, and the expanded type

-}
type Type
    = Aliased AliasInfo Type
    | Var String
    | Lambda Type Type
    | Tuple (List Type)
    | Type String (List Type)
    | Record (List ( String, Type )) (Maybe String)


type alias AliasInfo =
    { name : String
    , comment : String
    }


{-| The key is the fully wualified name of the alias.

e.g. for
module Example.Type exposing (Alias)

the key would be "Example.Type.Alias"

-}
type alias ExternalAliases =
    Dict String Elm.Docs.Alias


{-| To start us off, this function will take a type and if it expands any aliases, it will return the expanded type.
-}
expand :
    ExternalAliases
    -> List Elm.Docs.Alias
    -> Elm.Type.Type
    -> Type
expand externalAliases localAliases tipe =
    case tipe of
        Elm.Type.Var name ->
            Var name

        Elm.Type.Lambda arg result ->
            Lambda
                (expand externalAliases localAliases arg)
                (expand externalAliases localAliases result)

        Elm.Type.Tuple types ->
            Tuple (List.map (expand externalAliases localAliases) types)

        Elm.Type.Type name args ->
            let
                expandedArgs =
                    List.map (expand externalAliases localAliases) args
            in
            case findAliasNamed name localAliases of
                Just foundAlias ->
                    Aliased
                        { name = foundAlias.name
                        , comment = foundAlias.comment
                        }
                        (expand externalAliases localAliases foundAlias.tipe)

                Nothing ->
                    case Dict.get name externalAliases of
                        Just foundExternalAlias ->
                            Aliased
                                { name = foundExternalAlias.name
                                , comment = foundExternalAlias.comment
                                }
                                (expand externalAliases localAliases foundExternalAlias.tipe)

                        Nothing ->
                            Type name expandedArgs

        Elm.Type.Record fields maybeExtensible ->
            let
                expandedFields =
                    List.map
                        (\( fieldName, fieldType ) ->
                            ( fieldName, expand externalAliases localAliases fieldType )
                        )
                        fields
            in
            Record expandedFields maybeExtensible


findAliasNamed : String -> List Elm.Docs.Alias -> Maybe Elm.Docs.Alias
findAliasNamed name aliases =
    case aliases of
        [] ->
            Nothing

        top :: remain ->
            if top.name == name then
                Just top

            else
                findAliasNamed name remain


isElmTypePrimitive : Elm.Type.Type -> Bool
isElmTypePrimitive tipe =
    case tipe of
        Elm.Type.Var "msg" ->
            -- bit of a hack for the moment
            True

        Elm.Type.Var _ ->
            False

        Elm.Type.Lambda arg result ->
            False

        Elm.Type.Tuple tups ->
            List.all isElmTypePrimitive tups

        Elm.Type.Type "String.String" [] ->
            True

        Elm.Type.Type "Basics.Bool" [] ->
            True

        Elm.Type.Type "Basics.Int" [] ->
            True

        Elm.Type.Type "Basics.Float" [] ->
            True

        Elm.Type.Type name [ inner ] ->
            if List.member name primitiveSingleContainers then
                isElmTypePrimitive inner

            else
                False

        Elm.Type.Type _ _ ->
            False

        Elm.Type.Record fields maybeName ->
            List.all (Tuple.second >> isElmTypePrimitive) fields


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
