module Docs.Ref.Get exposing
    ( Id
    , Ref
    , lookup
    )

import App.Stores
import Docs.Ref
import Elm.Docs
import Elm.Module
import Elm.Package
import Store.Modules


type alias Ref =
    Docs.Ref.Ref


type alias Id =
    Docs.Ref.Id


{-| Look up a value, alias or union type by its name.

Names are in the format `Module.Name.typeName`.

-}
lookup : String -> App.Stores.Stores -> Maybe Ref
lookup id stores =
    case List.reverse (String.split "." id) of
        [] ->
            Nothing

        typeN :: rest ->
            let
                ( moduleNameString, typeName ) =
                    ( List.reverse rest |> String.join ".", typeN )
            in
            case Elm.Module.fromString moduleNameString of
                Just moduleName ->
                    Store.Modules.getByName moduleName stores.modules
                        |> Maybe.andThen (getBlockRef id moduleName typeName)

                Nothing ->
                    Nothing


getBlockRef : String -> Elm.Module.Name -> String -> Store.Modules.Module -> Maybe Ref
getBlockRef id modName typeName mod =
    case getBlock typeName mod of
        Just block ->
            Just
                { id = Docs.Ref.Id id
                , source =
                    { moduleName = modName
                    , package = mod.package
                    }
                , block = block
                }

        Nothing ->
            Nothing


getBlock : String -> Store.Modules.Module -> Maybe Elm.Docs.Block
getBlock typeName mod =
    List.foldl
        (\block blockFound ->
            case blockFound of
                Just r ->
                    blockFound

                Nothing ->
                    case toBlockName block of
                        Just name ->
                            if name == typeName then
                                Just block

                            else
                                Nothing

                        Nothing ->
                            Nothing
        )
        Nothing
        (Elm.Docs.toBlocks mod.info)


toBlockName : Elm.Docs.Block -> Maybe String
toBlockName block =
    case block of
        Elm.Docs.UnionBlock details ->
            Just details.name

        Elm.Docs.AliasBlock details ->
            Just details.name

        Elm.Docs.MarkdownBlock markdown ->
            Nothing

        Elm.Docs.ValueBlock details ->
            Just details.name

        Elm.Docs.BinopBlock details ->
            Just details.name

        Elm.Docs.UnknownBlock text ->
            Nothing
