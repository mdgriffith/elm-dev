module Press.Generate.Regions exposing (generate, values)

import Elm
import Elm.Annotation as Type
import Elm.Arg
import Elm.Case
import Elm.Let
import Elm.Op
import Gen.List
import Gen.Maybe
import Press.Model


values =
    { empty =
        Elm.value
            { importFrom = [ "App", "View", "Id" ]
            , name = "empty"
            , annotation = Nothing
            }
    , mapOperation =
        \fn region ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "App", "View", "Id" ]
                    , name = "mapOperation"
                    , annotation = Nothing
                    }
                )
                [ fn
                , region
                ]
    , toList =
        Elm.value
            { importFrom = [ "App", "View", "Id" ]
            , name = "toList"
            , annotation = Nothing
            }
    , setRegion =
        \region value regions ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "App", "View", "Id" ]
                    , name = "setRegion"
                    , annotation = Nothing
                    }
                )
                [ region, value, regions ]
    , update =
        \msg model ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "App", "View", "Id" ]
                    , name = "update"
                    , annotation = Nothing
                    }
                )
                [ msg
                , model
                ]
    }


generate : Press.Model.ViewRegions -> Elm.File
generate viewRegions =
    generateRegionIndex viewRegions


types =
    { region =
        Type.named [] "Region"
    , regionRecord =
        Type.namedWith [ "App", "View" ] "Regions" [ Type.var "view" ]
    , regionRecordWith =
        \var ->
            Type.namedWith [ "App", "View" ] "Regions" [ Type.var var ]
    , changes =
        Type.namedWith [] "Changes" [ Type.var "view" ]
    , id =
        Type.named [] "Id"
    , operation =
        Type.namedWith [] "Operation" [ Type.var "view" ]
    , operationWith =
        \str ->
            Type.namedWith [] "Operation" [ Type.var str ]
    }


generateRegionIndex : Press.Model.ViewRegions -> Elm.File
generateRegionIndex viewRegions =
    let
        otherRegions =
            viewRegions.regions
                |> List.map
                    (\( name, regionType ) ->
                        Elm.variant name
                    )

        idName base =
            base ++ "Id"

        otherRegionIds =
            viewRegions.regions
                |> List.map
                    (\( name, regionType ) ->
                        case regionType of
                            Press.Model.One ->
                                Elm.variant (idName name)

                            Press.Model.Many ->
                                Elm.variantWith (idName name) [ Type.int ]
                    )

        -- Useful references
        route =
            Press.Model.types.routeType
    in
    Elm.fileWith [ "App", "View", "Id" ]
        { docs = ""
        , aliases = []
        }
        [ Elm.customType "Region"
            otherRegions
            |> Elm.exposeConstructor
        , Elm.customType "Id"
            otherRegionIds
            |> Elm.exposeConstructor
        , Elm.alias "Changes"
            (Type.record
                [ ( "added", Type.list (Type.var "view") )
                , ( "removed", Type.list (Type.var "view") )
                ]
            )
            |> Elm.exposeConstructor
        , Elm.customType "Operation"
            [ Elm.variantWith "Push" [ types.region, Type.var "view" ]
            , Elm.variantWith "PushTo" [ types.id, Type.var "view" ]
            , Elm.variantWith "ReplaceAt" [ types.id, Type.var "view" ]
            , Elm.variantWith "Clear" []
            , Elm.variantWith "ClearRegion" [ types.region ]
            , Elm.variantWith "ClearView" [ types.id ]
            ]
            |> Elm.exposeConstructor
        , mapOperation
        , update viewRegions

        -- Region management
        , setRegion viewRegions
        , setRegionItem viewRegions
        , clearRegion viewRegions
        , clearRegionAt viewRegions
        , toList viewRegions
        , allRegionsDeclaration viewRegions
        , mapRegion viewRegions
        , Elm.declaration "empty"
            (initViewRegions viewRegions)
            |> Elm.expose
        ]


viewRegionAlias : Press.Model.ViewRegions -> Elm.Declaration
viewRegionAlias regions =
    let
        regionFields =
            regions.regions
                |> List.map
                    (\( field, regionType ) ->
                        ( field
                        , case regionType of
                            Press.Model.One ->
                                Type.maybe Type.string

                            Press.Model.Many ->
                                Type.list Type.string
                        )
                    )
    in
    Elm.alias "ViewRegions"
        (Type.record regionFields)


update regions =
    let
        allRegions =
            regions.regions
    in
    Elm.declaration "update"
        (Elm.fn2
            (Elm.Arg.varWith "operation" types.operation)
            (Elm.Arg.varWith "regions" types.regionRecord)
            (\operation model ->
                Elm.Case.custom operation
                    types.operation
                    [ Elm.Case.branch
                        (Elm.Arg.customType "Push" Tuple.pair
                            |> Elm.Arg.item (Elm.Arg.varWith "region" types.region)
                            |> Elm.Arg.item (Elm.Arg.varWith "val" (Type.var "view"))
                        )
                        (\( region, pageId ) ->
                            -- let
                            --     shared =
                            --         Press.Model.toShared config (Elm.get "frame" model)
                            --     pageId =
                            --         Elm.apply
                            --             (Elm.val "toPageKey")
                            --             [ pageIdToLoad ]
                            -- in
                            Elm.Let.letIn
                                (\newModel ->
                                    -- getPageInit.call pageIdToLoad
                                    --     shared
                                    --     (Elm.get "states" newModel)
                                    --     |> preloadPage.call config newModel pageIdToLoad
                                    Elm.tuple newModel
                                        (added pageId)
                                )
                                |> Elm.Let.value "modelWithRegionSet"
                                    (Elm.apply (Elm.val "setRegion")
                                        [ region
                                        , pageId
                                        , model
                                        ]
                                    )
                                |> Elm.Let.toExpression
                        )
                    , Elm.Case.branch
                        (Elm.Arg.customType "PushTo" Tuple.pair
                            |> Elm.Arg.item (Elm.Arg.varWith "regionId" types.id)
                            |> Elm.Arg.item (Elm.Arg.varWith "val" (Type.var "view"))
                        )
                        (\( regionId, pageId ) ->
                            -- let
                            --     shared =
                            --         Press.Model.toShared config (Elm.get "frame" model)
                            --     pageId =
                            --         Elm.apply
                            --             (Elm.val "toPageKey")
                            --             [ pageIdToLoad ]
                            -- in
                            Elm.Let.letIn
                                (\newModel ->
                                    -- getPageInit.call pageIdToLoad
                                    --     shared
                                    --     (Elm.get "states" newModel)
                                    --     |> preloadPage.call config newModel pageIdToLoad
                                    Elm.tuple newModel
                                        (added pageId)
                                )
                                |> Elm.Let.value "modelWithRegionSet"
                                    (Elm.apply (Elm.val "setRegionItem")
                                        [ regionId
                                        , pageId
                                        , model
                                        , Elm.bool False
                                        ]
                                    )
                                |> Elm.Let.toExpression
                        )
                    , Elm.Case.branch
                        (Elm.Arg.customType "ReplaceAt" Tuple.pair
                            |> Elm.Arg.item (Elm.Arg.varWith "regionId" types.id)
                            |> Elm.Arg.item (Elm.Arg.varWith "val" (Type.var "view"))
                        )
                        (\( regionId, pageId ) ->
                            -- let
                            --     shared =
                            --         Press.Model.toShared config (Elm.get "frame" model)
                            --     pageId =
                            --         Elm.apply
                            --             (Elm.val "toPageKey")
                            --             [ pageIdToLoad ]
                            -- in
                            Elm.Let.letIn
                                (\newModel ->
                                    -- getPageInit.call pageIdToLoad
                                    --     shared
                                    --     (Elm.get "states" newModel)
                                    --     |> preloadPage.call config newModel pageIdToLoad
                                    Elm.tuple newModel (added pageId)
                                )
                                |> Elm.Let.value "modelWithRegionSet"
                                    (Elm.apply (Elm.val "setRegionItem")
                                        [ regionId
                                        , pageId
                                        , model
                                        , Elm.bool True
                                        ]
                                    )
                                |> Elm.Let.toExpression
                        )
                    , Elm.Case.branch (Elm.Arg.customType "Clear" ())
                        (\_ ->
                            Elm.tuple
                                (Gen.List.call_.foldl
                                    (Elm.val "clearRegion")
                                    model
                                    (Elm.val "allRegions")
                                )
                                noChanges
                        )
                    , Elm.Case.branch
                        (Elm.Arg.customType "ClearRegion" identity
                            |> Elm.Arg.item (Elm.Arg.varWith "region" types.region)
                        )
                        (\region ->
                            Elm.tuple
                                (Elm.apply (Elm.val "clearRegion")
                                    [ region
                                    , model
                                    ]
                                )
                                noChanges
                        )
                    , Elm.Case.branch
                        (Elm.Arg.customType "ClearView" identity
                            |> Elm.Arg.item (Elm.Arg.varWith "regionId" types.id)
                        )
                        (\regionId ->
                            Elm.tuple
                                (Elm.apply (Elm.val "clearRegionAt")
                                    [ regionId
                                    , model
                                    ]
                                )
                                noChanges
                        )
                    ]
                    |> Elm.withType
                        (Type.tuple
                            types.regionRecord
                            types.changes
                        )
            )
        )
        -- |> Elm.withType
        --     (Type.function
        --         [ types.operation
        --         , types.regionRecord
        --         ]
        --         (Type.tuple types.regionRecord types.changes)
        --     )
        |> Elm.expose


added val =
    Elm.record
        [ ( "added", Elm.list [ val ] )
        , ( "removed", Elm.list [] )
        ]


removed val =
    Elm.record
        [ ( "added", Elm.list [] )
        , ( "removed", Elm.list [ val ] )
        ]


noChanges =
    Elm.record
        [ ( "added", Elm.list [] )
        , ( "removed", Elm.list [] )
        ]


{-| Clear a region and set the new content
-}
setRegion : Press.Model.ViewRegions -> Elm.Declaration
setRegion regions =
    let
        allRegions =
            regions.regions
    in
    Elm.declaration "setRegion"
        (Elm.fn3
            (Elm.Arg.varWith "region" types.region)
            (Elm.Arg.varWith "contentId" (Type.var "view"))
            (Elm.Arg.varWith "viewRegions" types.regionRecord)
            (\region contentId viewRegions ->
                Elm.Case.custom region
                    types.region
                    (List.map
                        (\( field, regionType ) ->
                            Elm.Case.branch (Elm.Arg.customType (capitalize field) ())
                                (\_ ->
                                    Elm.updateRecord
                                        [ ( field
                                          , case regionType of
                                                Press.Model.One ->
                                                    Elm.just contentId

                                                Press.Model.Many ->
                                                    Elm.list [ contentId ]
                                          )
                                        ]
                                        viewRegions
                                )
                        )
                        allRegions
                    )
            )
        )
        |> Elm.expose


capitalize : String -> String
capitalize str =
    let
        top =
            String.left 1 str

        remain =
            String.dropLeft 1 str
    in
    String.toUpper top ++ remain


{-| Given a specific region ID, insert a new page id at that
-}
setRegionItem : Press.Model.ViewRegions -> Elm.Declaration
setRegionItem regions =
    let
        allRegions =
            regions.regions
    in
    Elm.declaration "setRegionItem"
        (Elm.fnBuilder
            (\regionId contentId viewRegions replaceExisting ->
                Elm.Case.custom regionId
                    types.id
                    (List.map
                        (\( field, regionType ) ->
                            case regionType of
                                Press.Model.One ->
                                    Elm.Case.branch (Elm.Arg.customType (toRegionIdType field) ())
                                        (\_ ->
                                            Elm.updateRecord
                                                [ ( field
                                                  , Elm.just contentId
                                                  )
                                                ]
                                                viewRegions
                                        )

                                Press.Model.Many ->
                                    Elm.Case.branch
                                        (Elm.Arg.customType (toRegionIdType field) identity
                                            |> Elm.Arg.item (Elm.Arg.varWith "index" Type.int)
                                        )
                                        (\index ->
                                            Elm.ifThen (Elm.Op.lte index (Elm.int 0))
                                                -- Add to the beginning
                                                (Elm.updateRecord
                                                    [ ( field
                                                      , Elm.Op.cons contentId (Elm.get field viewRegions)
                                                      )
                                                    ]
                                                    viewRegions
                                                )
                                                (Elm.ifThen (Elm.Op.gt index (Gen.List.call_.length (Elm.get field viewRegions)))
                                                    -- Add to the end
                                                    (Elm.updateRecord
                                                        [ ( field
                                                          , Elm.Op.append
                                                                (Elm.get field viewRegions)
                                                                (Elm.list [ contentId ])
                                                          )
                                                        ]
                                                        viewRegions
                                                    )
                                                    -- Add at the index, pushing whatever back
                                                    (Elm.updateRecord
                                                        [ ( field
                                                          , Elm.get field viewRegions
                                                                |> Gen.List.call_.indexedMap
                                                                    (Elm.fn2
                                                                        (Elm.Arg.varWith "itemIndex" Type.int)
                                                                        (Elm.Arg.varWith "pageId" Type.string)
                                                                        (\itemIndex pageId ->
                                                                            Elm.ifThen (Elm.Op.equal itemIndex index)
                                                                                (Elm.list [ contentId, pageId ])
                                                                                (Elm.list [ pageId ])
                                                                        )
                                                                    )
                                                                |> Gen.List.call_.concat
                                                          )
                                                        ]
                                                        viewRegions
                                                    )
                                                )
                                        )
                        )
                        allRegions
                    )
                    |> Elm.withType types.regionRecord
            )
            |> Elm.fnArg (Elm.Arg.varWith "regionId" types.id)
            |> Elm.fnArg (Elm.Arg.varWith "contentId" (Type.var "view"))
            |> Elm.fnArg (Elm.Arg.varWith "viewRegions" types.regionRecord)
            |> Elm.fnArg (Elm.Arg.varWith "replaceExisting" Type.bool)
            |> Elm.fnDone
        )


clearRegion : Press.Model.ViewRegions -> Elm.Declaration
clearRegion regions =
    let
        allRegions =
            regions.regions
    in
    Elm.declaration "clearRegion"
        (Elm.fn2
            (Elm.Arg.varWith "region" types.region)
            (Elm.Arg.varWith "viewRegions" types.regionRecord)
            (\region viewRegions ->
                Elm.Case.custom region
                    types.region
                    (List.map
                        (\( field, regionType ) ->
                            Elm.Case.branch (Elm.Arg.customType field ())
                                (\_ ->
                                    Elm.updateRecord
                                        [ ( field
                                          , case regionType of
                                                Press.Model.One ->
                                                    Elm.nothing

                                                Press.Model.Many ->
                                                    Elm.list []
                                          )
                                        ]
                                        viewRegions
                                )
                        )
                        allRegions
                    )
            )
        )


toRegionIdType base =
    capitalize base ++ "Id"


clearRegionAt : Press.Model.ViewRegions -> Elm.Declaration
clearRegionAt regions =
    let
        allRegions =
            regions.regions
    in
    Elm.declaration "clearRegionAt"
        (Elm.fn2
            (Elm.Arg.varWith "regionId" types.id)
            (Elm.Arg.varWith "viewRegions" types.regionRecord)
            (\regionId viewRegions ->
                Elm.Case.custom regionId
                    types.id
                    (List.map
                        (\( field, regionType ) ->
                            case regionType of
                                Press.Model.One ->
                                    Elm.Case.branch (Elm.Arg.customType (toRegionIdType field) ())
                                        (\_ ->
                                            Elm.updateRecord
                                                [ ( field
                                                  , Elm.nothing
                                                  )
                                                ]
                                                viewRegions
                                        )

                                Press.Model.Many ->
                                    Elm.Case.branch
                                        (Elm.Arg.customType (toRegionIdType field) identity
                                            |> Elm.Arg.item (Elm.Arg.varWith "index" Type.int)
                                        )
                                        (\index ->
                                            -- Add at the index, pushing whatever back
                                            Elm.updateRecord
                                                [ ( field
                                                  , Elm.get field viewRegions
                                                        |> Gen.List.call_.indexedMap
                                                            (Elm.fn2
                                                                (Elm.Arg.varWith "itemIndex" Type.int)
                                                                (Elm.Arg.varWith "pageId" Type.string)
                                                                (\itemIndex pageId ->
                                                                    Elm.ifThen (Elm.Op.equal itemIndex index)
                                                                        (Elm.list [])
                                                                        (Elm.list [ pageId ])
                                                                )
                                                            )
                                                        |> Gen.List.call_.concat
                                                  )
                                                ]
                                                viewRegions
                                        )
                        )
                        allRegions
                    )
                    |> Elm.withType types.regionRecord
            )
        )


allRegionsDeclaration : Press.Model.ViewRegions -> Elm.Declaration
allRegionsDeclaration regions =
    let
        allRegions =
            regions.regions
    in
    Elm.declaration "allRegions"
        (Elm.list
            (regions.regions
                |> List.map
                    (\( regionName, _ ) ->
                        Elm.value
                            { importFrom = []
                            , name = capitalize regionName
                            , annotation = Just types.region
                            }
                    )
            )
        )


toList : Press.Model.ViewRegions -> Elm.Declaration
toList regions =
    let
        allRegions =
            regions.regions
    in
    Elm.declaration "toList"
        (Elm.fn
            (Elm.Arg.varWith "viewRegions" types.regionRecord)
            (\viewRegions ->
                allRegions
                    |> List.map
                        (\( typename, regionType ) ->
                            case regionType of
                                Press.Model.One ->
                                    Elm.get typename viewRegions
                                        |> Gen.Maybe.map (\x -> Elm.list [ x ])
                                        |> Gen.Maybe.withDefault (Elm.list [])

                                Press.Model.Many ->
                                    Elm.get typename viewRegions
                        )
                    |> Elm.list
                    |> Gen.List.call_.concat
                    |> Elm.withType (Type.list (Type.var "view"))
            )
        )
        |> Elm.expose


initViewRegions : Press.Model.ViewRegions -> Elm.Expression
initViewRegions regions =
    let
        regionFields =
            regions.regions
                |> List.map
                    (\( field, regionType ) ->
                        ( field
                        , case regionType of
                            Press.Model.One ->
                                Elm.nothing

                            Press.Model.Many ->
                                Elm.list []
                        )
                    )
    in
    Elm.record regionFields
        |> Elm.withType types.regionRecord


mapOperation : Elm.Declaration
mapOperation =
    Elm.declaration "mapOperation"
        (Elm.fn2
            (Elm.Arg.varWith "fn"
                (Type.function
                    [ Type.var "view"
                    ]
                    (Type.var "b")
                )
            )
            (Elm.Arg.varWith "operation" types.operation)
            (\fn operation ->
                Elm.Case.custom operation
                    types.operation
                    [ Elm.Case.branch
                        (Elm.Arg.customType "Push" Tuple.pair
                            |> Elm.Arg.item (Elm.Arg.varWith "region" types.region)
                            |> Elm.Arg.item (Elm.Arg.varWith "pageId" (Type.var "view"))
                        )
                        (\( region, pageId ) ->
                            Elm.apply
                                (Elm.val "Push")
                                [ region
                                , Elm.apply fn [ pageId ]
                                ]
                        )
                    , Elm.Case.branch
                        (Elm.Arg.customType "PushTo" Tuple.pair
                            |> Elm.Arg.item (Elm.Arg.varWith "regionId" types.id)
                            |> Elm.Arg.item (Elm.Arg.varWith "route" (Type.var "view"))
                        )
                        (\( regionId, pageId ) ->
                            Elm.apply
                                (Elm.val "PushTo")
                                [ regionId
                                , Elm.apply fn [ pageId ]
                                ]
                        )
                    , Elm.Case.branch
                        (Elm.Arg.customType "ReplaceAt" Tuple.pair
                            |> Elm.Arg.item (Elm.Arg.varWith "regionId" types.id)
                            |> Elm.Arg.item (Elm.Arg.varWith "pageId" Press.Model.types.pageId)
                        )
                        (\( regionId, pageId ) ->
                            Elm.apply
                                (Elm.val "ReplaceAt")
                                [ regionId
                                , Elm.apply fn [ pageId ]
                                ]
                        )
                    , Elm.Case.branch (Elm.Arg.customType "Clear" ())
                        (\_ -> Elm.val "Clear")
                    , Elm.Case.branch
                        (Elm.Arg.customType "ClearRegion" identity
                            |> Elm.Arg.item (Elm.Arg.varWith "region" types.region)
                        )
                        (\region ->
                            Elm.apply
                                (Elm.val "ClearRegion")
                                [ region ]
                        )
                    , Elm.Case.branch
                        (Elm.Arg.customType "ClearView" identity
                            |> Elm.Arg.item (Elm.Arg.varWith "regionId" types.id)
                        )
                        (\regionId ->
                            Elm.apply
                                (Elm.val "ClearView")
                                [ regionId ]
                        )
                    ]
                    |> Elm.withType
                        (types.operationWith "b")
            )
        )
        |> Elm.expose


mapRegion : Press.Model.ViewRegions -> Elm.Declaration
mapRegion regions =
    let
        allRegions =
            regions.regions
    in
    Elm.declaration "mapRegion"
        (Elm.fn2
            (Elm.Arg.varWith "fn" (Type.function [ types.id, Type.var "view" ] (Type.var "b")))
            (Elm.Arg.varWith "regions" types.regionRecord)
            (\fn viewRegions ->
                allRegions
                    |> List.map
                        (\( fieldName, regionType ) ->
                            let
                                idName =
                                    capitalize fieldName ++ "Id"

                                regionId =
                                    Elm.value
                                        { importFrom = []
                                        , name = idName
                                        , annotation = Just types.id
                                        }
                            in
                            ( fieldName
                            , case regionType of
                                Press.Model.One ->
                                    Elm.get fieldName viewRegions
                                        |> Gen.Maybe.call_.map
                                            (Elm.apply
                                                fn
                                                [ regionId
                                                ]
                                            )

                                Press.Model.Many ->
                                    Elm.get fieldName viewRegions
                                        |> Gen.List.call_.indexedMap
                                            (Elm.fn2
                                                (Elm.Arg.varWith "index" Type.int)
                                                (Elm.Arg.varWith "pageId" (Type.var "view"))
                                                (\index pageId ->
                                                    Elm.apply fn
                                                        [ Elm.apply regionId
                                                            [ index
                                                            ]
                                                        , pageId
                                                        ]
                                                )
                                            )
                            )
                        )
                    |> Elm.record
                    |> Elm.withType
                        (types.regionRecordWith "b")
            )
        )
        |> Elm.expose
