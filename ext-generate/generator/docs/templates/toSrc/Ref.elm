module Ref exposing
    ( Id
    , Ref
    , fromModule
    , fromPackage
    , lookup
    , toModuleName
    )

import Docs.Modules
import Docs.Packages
import Elm.Docs


type alias Ref =
    { id : Id
    , source : Source
    , block : Elm.Docs.Block
    }


type Id
    = Id String


type Source
    = FromModule
        { moduleName : String
        }
    | FromPackage
        { moduleName : String
        , package : String
        }


toModuleName : Ref -> String
toModuleName ref =
    case ref.source of
        FromModule { moduleName } ->
            moduleName

        FromPackage { moduleName } ->
            moduleName


fromModule : String -> Elm.Docs.Block -> Ref
fromModule moduleName block =
    let
        src =
            FromModule { moduleName = moduleName }
    in
    { id = toId src block
    , source = src
    , block = block
    }


fromPackage : String -> String -> Elm.Docs.Block -> Ref
fromPackage package moduleName block =
    let
        src =
            FromPackage
                { moduleName = moduleName
                , package = package
                }
    in
    { id = toId src block
    , source = src
    , block = block
    }


toId : Source -> Elm.Docs.Block -> Id
toId source block =
    let
        blockId =
            blockToIdString block
    in
    case source of
        FromModule { moduleName } ->
            Id (moduleName ++ ":" ++ blockId)

        FromPackage { moduleName, package } ->
            Id (package ++ ":" ++ moduleName ++ ":" ++ blockId)


blockToIdString : Elm.Docs.Block -> String
blockToIdString block =
    case block of
        Elm.Docs.MarkdownBlock markdown ->
            markdown

        Elm.Docs.UnionBlock details ->
            details.name

        Elm.Docs.AliasBlock details ->
            details.name

        Elm.Docs.ValueBlock details ->
            details.name

        Elm.Docs.BinopBlock details ->
            details.name

        Elm.Docs.UnknownBlock text ->
            text


{-| -}
lookup : String -> Maybe Ref
lookup id =
    let
        ( moduleName, typeName ) =
            case List.reverse (String.split "." id) of
                [] ->
                    ( "", "" )

                typeN :: rest ->
                    ( List.reverse rest |> String.join ".", typeN )

        searchModule : Elm.Docs.Module -> Maybe Ref
        searchModule mod =
            if mod.name == moduleName then
                -- Found the module, now look for matching block
                List.foldl
                    (\block found ->
                        case found of
                            Just r ->
                                found

                            Nothing ->
                                case block of
                                    Elm.Docs.UnionBlock details ->
                                        if details.name == typeName then
                                            Just
                                                { id = toId (FromModule { moduleName = mod.name }) block
                                                , source = FromModule { moduleName = mod.name }
                                                , block = block
                                                }

                                        else
                                            Nothing

                                    Elm.Docs.AliasBlock details ->
                                        if details.name == typeName then
                                            Just
                                                { id = toId (FromModule { moduleName = mod.name }) block
                                                , source = FromModule { moduleName = mod.name }
                                                , block = block
                                                }

                                        else
                                            Nothing

                                    _ ->
                                        Nothing
                    )
                    Nothing
                    (Elm.Docs.toBlocks mod)

            else
                Nothing

        searchPackage : { name : String, modules : List Elm.Docs.Module } -> Maybe Ref
        searchPackage { name, modules } =
            List.foldl
                (\mod found ->
                    case found of
                        Just ref ->
                            found

                        Nothing ->
                            if mod.name == moduleName then
                                -- Found the module, now look for matching block
                                List.foldl
                                    (\block blockFound ->
                                        case blockFound of
                                            Just r ->
                                                blockFound

                                            Nothing ->
                                                case block of
                                                    Elm.Docs.UnionBlock details ->
                                                        if details.name == typeName then
                                                            Just
                                                                { id = toId (FromPackage { package = name, moduleName = mod.name }) block
                                                                , source = FromPackage { package = name, moduleName = mod.name }
                                                                , block = block
                                                                }

                                                        else
                                                            Nothing

                                                    Elm.Docs.AliasBlock details ->
                                                        if details.name == typeName then
                                                            Just
                                                                { id = toId (FromPackage { package = name, moduleName = mod.name }) block
                                                                , source = FromPackage { package = name, moduleName = mod.name }
                                                                , block = block
                                                                }

                                                        else
                                                            Nothing

                                                    _ ->
                                                        Nothing
                                    )
                                    Nothing
                                    (Elm.Docs.toBlocks mod)

                            else
                                Nothing
                )
                Nothing
                modules

        inModules =
            List.foldl
                (\mod found ->
                    case found of
                        Just ref ->
                            found

                        Nothing ->
                            searchModule mod
                )
                Nothing
                Docs.Modules.modules
    in
    case inModules of
        Just ref ->
            Just ref

        Nothing ->
            List.foldl
                (\package found ->
                    case found of
                        Just ref ->
                            found

                        Nothing ->
                            searchPackage package
                )
                Nothing
                Docs.Packages.directory
