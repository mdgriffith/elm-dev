module Press.Model exposing (..)

{-| -}

import Elm
import Elm.Annotation as Type
import Elm.Arg
import Elm.Case
import Elm.Declare
import Elm.Let
import Elm.Op
import Gen.App.Page
import Gen.App.Page.Error
import Gen.App.State
import Gen.Browser
import Gen.Browser.Navigation
import Gen.Json.Encode
import Gen.List
import Gen.Listen
import Gen.Platform.Cmd
import Gen.Platform.Sub
import Gen.Set
import Gen.Url
import Json.Decode
import Options.App
import Path
import Set exposing (Set)


type alias ViewRegions =
    { regions : List ( String, RegionType )
    }


type RegionType
    = One
    | Many


decodeViewRegions : Json.Decode.Decoder ViewRegions
decodeViewRegions =
    Json.Decode.list Json.Decode.string
        |> Json.Decode.map
            (\names ->
                let
                    regions =
                        List.map
                            (\name ->
                                if String.endsWith "[]" name then
                                    ( name
                                        |> String.replace "[]" ""
                                        |> Path.decapitalize
                                    , Many
                                    )

                                else
                                    ( name
                                        |> Path.decapitalize
                                    , One
                                    )
                            )
                            names
                in
                { regions =
                    if List.isEmpty regions then
                        [ ( "primary", One ) ]

                    else
                        regions
                }
            )


type alias Page =
    { id : String
    , moduleName : List String
    , url : UrlPattern
    , deprecatedUrls : List UrlPattern
    , source : String
    , assets : Maybe SourceDirectory
    }


type alias SourceDirectory =
    { base : String
    , baseOnApp : String
    , baseOnServer : String
    , files : List Source
    }


type alias Source =
    { path : String
    , source : String
    }


type UrlPattern
    = UrlPattern UrlPatternDetails


type alias UrlPatternDetails =
    { path : List UrlPiece
    , includePathTail : Bool
    , queryParams : QueryParams
    }


type alias QueryParams =
    { includeCatchAll : Bool
    , specificFields : Set String
    }


type UrlPiece
    = Token String
    | Variable String


hasVars : List UrlPiece -> Bool
hasVars pieces =
    List.any
        (\piece ->
            case piece of
                Token _ ->
                    False

                Variable _ ->
                    True
        )
        pieces


{-| Two URL patterns have collisions if they can both match the same URL.
-}
hasCollisions : UrlPattern -> UrlPattern -> Bool
hasCollisions (UrlPattern one) (UrlPattern two) =
    hasCollisionsHelp one one.path two two.path


hasCollisionsHelp : UrlPatternDetails -> List UrlPiece -> UrlPatternDetails -> List UrlPiece -> Bool
hasCollisionsHelp one onePath two twoPath =
    case ( onePath, twoPath ) of
        ( [], [] ) ->
            --
            True

        ( [], _ ) ->
            False

        ( _, [] ) ->
            False

        ( (Token oneToken) :: oneTail, (Token twoToken) :: twoTail ) ->
            if oneToken == twoToken then
                hasCollisionsHelp one oneTail two twoTail

            else
                False

        ( (Variable oneVar) :: oneTail, (Token twoToken) :: twoTail ) ->
            -- A variable can potentially collide unless there is a descriminator later
            -- /item/:item/details
            -- /item/:item/preview
            hasCollisionsHelp one oneTail two twoTail

        ( (Token oneToken) :: oneTail, (Variable _) :: twoTail ) ->
            -- Same as above
            hasCollisionsHelp one oneTail two twoTail

        ( (Variable _) :: oneTail, (Variable _) :: twoTail ) ->
            hasCollisionsHelp one oneTail two twoTail


getRoutes : Page -> List UrlPattern
getRoutes page =
    page.url :: page.deprecatedUrls


type ConfigType
    = ViewConfig
    | SubscriptionConfig
    | UpdateConfig
    | FullConfig
    | TestConfig


toConfig configType =
    (if configType == FullConfig then
        Type.record

     else
        Type.extensible "config"
    )
        (List.filterMap
            (\( allowed, name, val ) ->
                if List.member configType allowed || configType == FullConfig then
                    Just ( name, val )

                else
                    Nothing
            )
            [ ( [ TestConfig ]
              , "init"
              , Type.function
                    [ storesType
                    , Gen.Json.Encode.annotation_.value
                    , Gen.Url.annotation_.url
                    ]
                    (Type.tuple
                        (Type.var "model")
                        effectType
                    )
              )
            , ( [ UpdateConfig, TestConfig ]
              , "update"
              , Type.function
                    [ storesType
                    , Type.var "msg"
                    , Type.var "model"
                    ]
                    (Type.tuple
                        (Type.var "model")
                        effectType
                    )
              )
            , ( [ SubscriptionConfig, UpdateConfig, TestConfig ]
              , "subscriptions"
              , Type.function
                    [ storesType
                    , Type.var "model"
                    ]
                    (Gen.Listen.annotation_.listen (Type.var "msg"))
              )
            , ( [ ViewConfig, TestConfig ]
              , "view"
              , Type.function
                    [ storesType
                    , Type.function [ Type.var "msg" ] appMsg
                    , Type.var "model"
                    , Type.namedWith [ "App", "View" ]
                        "Regions"
                        [ Type.namedWith []
                            "View"
                            [ appMsg ]
                        ]
                    ]
                    (Gen.Browser.annotation_.document appMsg)
              )
            , ( []
              , "toCmd"
              , Type.function
                    [ storesType
                    , Type.namedWith [] "CmdOptions" [ Type.var "msg" ]
                    , Type.var "model"
                    , effectWith appMsg
                    ]
                    (Gen.Platform.Cmd.annotation_.cmd appMsg)
              )
            , ( [ SubscriptionConfig, UpdateConfig, TestConfig ]
              , "toSub"
              , Type.function
                    [ storesType
                    , Type.namedWith [] "SubOptions" [ Type.var "msg" ]
                    , Type.var "model"
                    , Gen.Listen.annotation_.listen appMsg
                    ]
                    (Gen.Platform.Sub.annotation_.sub appMsg)
              )
            , ( [ TestConfig ]
              , "onUrlChange"
              , Type.function
                    [ Gen.Url.annotation_.url
                    ]
                    (Type.var "msg")
              )
            , ( [ TestConfig
                ]
              , "onUrlRequest"
              , Type.function
                    [ Gen.Browser.annotation_.urlRequest
                    ]
                    (Type.var "msg")
              )
            ]
        )


appMsg : Type.Annotation
appMsg =
    Type.namedWith [] "Msg" [ Type.var "msg" ]


routePath =
    [ "App", "Route" ]


routeType : Type.Annotation
routeType =
    Type.named routePath "Route"


regionsRecord =
    Type.namedWith [ "App", "View" ] "Regions" [ pageIdType ]


stateCache =
    Gen.App.State.annotation_.cache (Type.named [] "State")


regionOperation =
    Type.namedWith [ "App", "View", "Id" ] "Operation" [ pageIdType ]


regionViewType =
    Type.namedWith [ "App", "View" ] "Regions"


regionIdType =
    Type.named [ "App", "View", "Id" ] "Id"


regionType =
    Type.named [ "App", "View", "Id" ] "Region"


pageIdType =
    Type.named [ "App", "Page", "Id" ] "Id"


storesType =
    Type.named [ "App", "Stores" ] "Stores"


storeMsgType =
    Type.named [ "App", "Store", "Msg" ] "Msg"


effectType =
    Type.namedWith [ "Effect" ] "Effect" [ Type.var "msg" ]


effectWith =
    \inner -> Type.namedWith [ "Effect" ] "Effect" [ inner ]


types =
    { msg = appMsg
    , pageMsg = Type.named [] "PageMsg"
    , routePath = routePath
    , routeType = routeType
    , pageId = pageIdType
    , broadcast = Type.named [ "Broadcast" ] "Msg"

    -- Model
    , model = Type.namedWith [] "Model" [ Type.var "key", Type.var "model" ]
    , testModel = Type.namedWith [] "Model" [ Type.unit, Type.var "model" ]
    , modelRecord =
        Type.record
            [ ( "key", Type.var "key" )
            , ( "limits", Gen.App.State.annotation_.limit )
            , ( "states", stateCache )
            , ( "stores", storesType )
            , ( "app", Type.var "app" )
            ]
    , pageLoadResult =
        Gen.App.Page.annotation_.init
            appMsg
            (Type.named [] "State")
    , regionsRecord = regionsRecord
    , regionIdType = regionIdType
    , regionViewType = regionViewType
    , regionOperation = regionOperation
    , regionType = regionType
    , stateCache = stateCache
    , renderedView = regionViewType [ Type.namedWith [] "View" [ appMsg ] ]
    , subOptions =
        Type.record
            [ ( "ignore", Type.function [ Type.string ] appMsg )
            ]
    , cmdOptions =
        Type.record
            [ ( "navKey", Gen.Browser.Navigation.annotation_.key )
            , ( "dropPageCache"
              , appMsg
              )
            , ( "preload"
              , Type.function [ pageIdType ] appMsg
              )
            , ( "viewRequested"
              , Type.function [ regionOperation ] appMsg
              )
            , ( "broadcast"
              , Type.function [ Type.named [ "Broadcast" ] "Msg" ] appMsg
              )
            ]
    , frame =
        toConfig FullConfig
    , frameView =
        toConfig ViewConfig
    , frameUpdate =
        toConfig UpdateConfig
    , frameSub =
        toConfig SubscriptionConfig
    , frameTest =
        toConfig TestConfig
    , pageModel = Type.named [] "PageModel"
    , effect = Type.namedWith [ "Effect" ] "Effect" [ Type.var "msg" ]
    , effectWith = effectWith
    , subscription = Type.namedWith [] "Subscription" [ Type.var "msg" ]
    , cache = Type.named [] "Cache"
    , toPageMsg =
        \string ->
            "To" ++ string
    }


effectNone =
    Elm.value
        { importFrom = [ "Effect" ]
        , name = "none"
        , annotation = Just types.effect
        }


effectMap mapArg mapArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Effect" ]
            , name = "map"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.var "a" ] (Type.var "b")
                        , Type.namedWith [] "Effect" [ Type.var "a" ]
                        ]
                        (Type.namedWith [] "Effect" [ Type.var "b" ])
                    )
            }
        )
        [ mapArg, mapArg0 ]


effectBatch : List Elm.Expression -> Elm.Expression
effectBatch batchArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Effect" ]
            , name = "batch"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [] "Effect" [ Type.var "msg" ])
                        ]
                        (Type.namedWith [] "Effect" [ Type.var "msg" ])
                    )
            }
        )
        [ Elm.list batchArg ]



{- 'Frame' helpers -}


toCmd : Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
toCmd config stores navKey frameModel effect =
    Elm.apply (Elm.get "toCmd" config)
        [ stores
        , Elm.record
            [ ( "navKey", navKey )
            , ( "dropPageCache", Elm.val "PageCacheCleared" )
            , ( "viewRequested", Elm.val "ViewUpdated" )
            , ( "broadcast", Elm.val "Broadcast" )
            , ( "preload", Elm.val "Preload" )
            ]
        , frameModel
        , effect
        ]


toSub : Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
toSub config stores frameModel sub =
    Elm.apply (Elm.get "toSub" config)
        [ stores
        , Elm.record
            [ ( "ignore", Elm.val "SubscriptionEventIgnored" )
            ]
        , frameModel
        , sub
        ]


getState : Elm.Expression -> Elm.Expression -> Elm.Expression
getState key model =
    Gen.App.State.call_.get key (Elm.get "states" model)


setState : Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
setState key val model =
    Gen.App.State.call_.insert key val (Elm.get "states" model)


setRegion region value regions =
    Elm.apply
        (Elm.value
            { importFrom = [ "App", "View", "Id" ]
            , name = "setRegion"
            , annotation = Nothing
            }
        )
        [ region, value, regions ]


{-|

    Only handles state loading

-}
loadPage :
    List Options.App.PageUsage
    ->
        Elm.Declare.Function
            (Elm.Expression
             -> Elm.Expression
             -> Elm.Expression
             -> Elm.Expression
             -> Elm.Expression
            )
loadPage routes =
    Elm.Declare.fn4 "loadPage"
        (Elm.Arg.varWith "config" types.frameUpdate)
        (Elm.Arg.varWith "model" types.model)
        (Elm.Arg.varWith "pageId" types.pageId)
        (Elm.Arg.varWith "initialization" types.pageLoadResult)
        (\config model pageId initialization ->
            Elm.Let.letIn
                (\pageKey pageGroupKey keep ->
                    Elm.Case.custom initialization
                        (Type.namedWith
                            [ "App", "Page" ]
                            "InitPlan"
                            [ Type.var "msg", Type.var "model" ]
                        )
                        [ Elm.Case.branch (Elm.Arg.customType "NotFound" ())
                            (\_ ->
                                let
                                    updatedModel =
                                        Elm.updateRecord
                                            [ ( "states"
                                              , Elm.get "states" model
                                                    |> Gen.App.State.call_.remove pageKey
                                              )
                                            ]
                                            model
                                in
                                Elm.tuple updatedModel
                                    effectNone
                            )
                        , Elm.Case.branch
                            (Elm.Arg.customType "Error" identity
                                |> Elm.Arg.item (Elm.Arg.varWith "err" Gen.App.Page.Error.annotation_.error)
                            )
                            (\err ->
                                let
                                    updatedModel =
                                        Elm.updateRecord
                                            [ ( "states"
                                              , Elm.get "states" model
                                                    |> Gen.App.State.call_.insert pageKey
                                                        (Elm.apply
                                                            (Elm.value
                                                                { importFrom = []
                                                                , name = "PageError_"
                                                                , annotation = Nothing
                                                                }
                                                            )
                                                            [ err ]
                                                        )
                                              )
                                            ]
                                            model
                                in
                                Elm.tuple updatedModel
                                    effectNone
                            )
                        , Elm.Case.branch
                            (Elm.Arg.customType "Loaded" Tuple.pair
                                |> Elm.Arg.item (Elm.Arg.varWith "newPage" (Type.named [] "State"))
                                |> Elm.Arg.item (Elm.Arg.varWith "pageEffect" (effectWith types.msg))
                            )
                            (\( newPage, pageEffect ) ->
                                Elm.Let.letIn
                                    (\limitUpdated ->
                                        let
                                            updatedModel =
                                                Elm.updateRecord
                                                    [ ( "states"
                                                      , Elm.get "states" model
                                                            |> Gen.App.State.call_.insert pageKey newPage
                                                            |> Gen.App.State.call_.purge (Elm.get "removedIds" limitUpdated)
                                                      )
                                                    , ( "limits"
                                                      , Elm.get "limit" limitUpdated
                                                      )
                                                    ]
                                                    model
                                        in
                                        Elm.tuple updatedModel
                                            pageEffect
                                    )
                                    |> Elm.Let.value "limitUpdated"
                                        (Gen.App.State.call_.addToLimit
                                            (Elm.record
                                                [ ( "groupId", pageGroupKey )
                                                , ( "instanceId", pageKey )
                                                , ( "max", Elm.apply (Elm.val "toPageLimit") [ pageId ] )
                                                , ( "keep", keep )
                                                ]
                                            )
                                            (Elm.get "limits" model)
                                        )
                                    |> Elm.Let.toExpression
                            )
                        , Elm.Case.branch
                            (Elm.Arg.customType "LoadFrom" identity
                                |> Elm.Arg.item (Elm.Arg.varWith "pageEffect" types.pageLoadResult)
                            )
                            (\pageEffect ->
                                let
                                    updatedModel =
                                        Elm.updateRecord
                                            [ ( "states"
                                              , Elm.get "states" model
                                                    |> Gen.App.State.call_.insert pageKey
                                                        (Elm.apply
                                                            (Elm.value
                                                                { importFrom = []
                                                                , name = "PageLoading_"
                                                                , annotation = Nothing
                                                                }
                                                            )
                                                            [ pageId ]
                                                        )
                                              )
                                            ]
                                            model
                                in
                                Elm.tuple updatedModel
                                    (pageEffect
                                        |> effectMap
                                            (Elm.apply
                                                (Elm.val "Loaded")
                                                [ pageId
                                                ]
                                            )
                                    )
                            )
                        ]
                )
                |> Elm.Let.value "pageKey"
                    (Elm.apply
                        (Elm.val "toPageKey")
                        [ pageId ]
                    )
                |> Elm.Let.value "pageGroupKey"
                    (Elm.apply
                        (Elm.val "toPageGroupKey")
                        [ pageId ]
                    )
                |> Elm.Let.value "keep"
                    (Elm.apply
                        (Elm.value
                            { importFrom = [ "App", "View", "Id" ]
                            , name = "toList"
                            , annotation = Nothing
                            }
                        )
                        [ Elm.get "viewing" (Elm.get "stores" model) ]
                        |> Elm.Op.pipe
                            (Elm.apply Gen.List.values_.map [ Elm.val "toPageKey" ])
                        |> Elm.Op.pipe
                            Gen.Set.values_.fromList
                    )
                |> Elm.Let.toExpression
                |> Elm.withType (Type.tuple types.model (effectWith types.msg))
        )


preloadPage :
    List Options.App.PageUsage
    ->
        Elm.Declare.Function
            (Elm.Expression
             -> Elm.Expression
             -> Elm.Expression
             -> Elm.Expression
             -> Elm.Expression
            )
preloadPage routes =
    Elm.Declare.fn4 "preloadPage"
        (Elm.Arg.varWith "config" types.frameUpdate)
        (Elm.Arg.varWith "pageId" types.pageId)
        (Elm.Arg.varWith "initialization" types.pageLoadResult)
        (Elm.Arg.varWith "model" types.model)
        (\config pageIdToPreload initialization model ->
            Elm.Let.letIn
                (\pageId ->
                    Elm.Case.custom initialization
                        (Type.namedWith
                            [ "App", "Page" ]
                            "InitPlan"
                            [ Type.var "msg", Type.var "model" ]
                        )
                        [ Elm.Case.branch (Elm.Arg.customType "NotFound" ())
                            (\_ ->
                                Elm.tuple model
                                    effectNone
                            )
                        , Elm.Case.branch
                            (Elm.Arg.customType "Error" identity
                                |> Elm.Arg.item (Elm.Arg.varWith "err" Gen.App.Page.Error.annotation_.error)
                            )
                            (\err ->
                                let
                                    updatedModel =
                                        Elm.updateRecord
                                            [ ( "states"
                                              , Elm.get "states" model
                                                    |> Gen.App.State.call_.insert pageId
                                                        (Elm.apply
                                                            (Elm.value
                                                                { importFrom = []
                                                                , name = "PageError_"
                                                                , annotation = Nothing
                                                                }
                                                            )
                                                            [ err ]
                                                        )
                                              )
                                            ]
                                            model
                                in
                                Elm.tuple updatedModel
                                    effectNone
                            )
                        , Elm.Case.branch
                            (Elm.Arg.customType "Loaded" Tuple.pair
                                |> Elm.Arg.item (Elm.Arg.varWith "newPage" (Type.named [] "State"))
                                |> Elm.Arg.item (Elm.Arg.varWith "pageEffect" (effectWith types.msg))
                            )
                            (\( newPage, pageEffect ) ->
                                Elm.tuple
                                    (Elm.updateRecord
                                        [ ( "states"
                                          , Elm.get "states" model
                                                |> Gen.App.State.call_.insert pageId newPage
                                          )
                                        ]
                                        model
                                    )
                                    pageEffect
                            )
                        , Elm.Case.branch
                            (Elm.Arg.customType "LoadFrom" identity
                                |> Elm.Arg.item (Elm.Arg.varWith "pageEffect" types.pageLoadResult)
                            )
                            (\pageEffect ->
                                let
                                    updatedModel =
                                        Elm.updateRecord
                                            [ ( "states"
                                              , Elm.get "states" model
                                                    |> Gen.App.State.call_.insert pageId
                                                        (Elm.apply
                                                            (Elm.value
                                                                { importFrom = []
                                                                , name = "PageLoading_"
                                                                , annotation = Nothing
                                                                }
                                                            )
                                                            [ pageIdToPreload ]
                                                        )
                                              )
                                            ]
                                            model
                                in
                                Elm.tuple updatedModel
                                    (pageEffect
                                        |> effectMap
                                            (Elm.apply
                                                (Elm.val "Loaded")
                                                [ pageIdToPreload
                                                ]
                                            )
                                    )
                            )
                        ]
                )
                |> Elm.Let.value "pageKey"
                    (Elm.apply
                        (Elm.val "toPageKey")
                        [ pageIdToPreload ]
                    )
                |> Elm.Let.toExpression
                |> Elm.withType (Type.tuple types.model (effectWith types.msg))
        )


toPageBranch : Options.App.PageUsage -> (Elm.Expression -> Elm.Expression) -> Elm.Case.Branch
toPageBranch pageInfo toBranchBody =
    let
        paramType =
            String.join "_" (String.split "." pageInfo.id) ++ "_Params"
    in
    Elm.Case.branch
        (Elm.Arg.customType (String.replace "." "" pageInfo.id) identity
            |> Elm.Arg.item (Elm.Arg.varWith "params" Type.unit)
        )
        toBranchBody


getPageInit :
    List Options.App.PageUsage
    ->
        Elm.Declare.Function
            (Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression)
getPageInit pages =
    Elm.Declare.fn3 "getPageInit"
        (Elm.Arg.varWith "pageId" types.pageId)
        (Elm.Arg.varWith "stores" storesType)
        (Elm.Arg.varWith "cache" (Gen.App.State.annotation_.cache (Type.named [] "State")))
        (\pageId stores cache ->
            Elm.Case.custom pageId
                types.pageId
                (pages
                    |> List.map
                        (\pageInfo ->
                            let
                                pageModule =
                                    pageInfo.moduleName

                                pageMsgTypeName =
                                    types.toPageMsg pageInfo.id

                                pageConfig =
                                    Elm.value
                                        { importFrom = pageModule
                                        , name = "page"
                                        , annotation = Nothing
                                        }
                            in
                            toPageBranch pageInfo
                                (\params ->
                                    Elm.Let.letIn
                                        (\pageDetails pageKey ->
                                            Elm.apply
                                                (Elm.apply
                                                    Gen.App.Page.values_.mapInitPlan
                                                    [ Elm.record
                                                        [ ( "onModel", Elm.val pageInfo.id )
                                                        , ( "onMsg"
                                                          , Elm.apply
                                                                (Elm.val pageMsgTypeName)
                                                                [ pageId ]
                                                          )
                                                        ]
                                                    ]
                                                )
                                                [ Elm.apply
                                                    (Elm.get "init" pageDetails)
                                                    [ pageId
                                                    , params
                                                    , stores
                                                    , getPage pageKey
                                                        pageInfo.id
                                                        cache
                                                        { nothing = Elm.nothing
                                                        , just = Elm.just
                                                        }
                                                    ]
                                                ]
                                        )
                                        |> Elm.Let.value "pageDetails"
                                            (Elm.apply
                                                Gen.App.Page.values_.toInternalDetails
                                                [ pageConfig ]
                                            )
                                        |> Elm.Let.value "pageKey"
                                            (Elm.apply (Elm.val "toPageKey")
                                                [ pageId ]
                                            )
                                        |> Elm.Let.toExpression
                                )
                        )
                )
                |> Elm.withType
                    types.pageLoadResult
        )


withPageHelper : Elm.Expression -> String -> (Elm.Expression -> Elm.Expression) -> Elm.Expression
withPageHelper pageConfig fieldName fn =
    Elm.Let.letIn
        (\pageDetails ->
            fn (Elm.get fieldName pageDetails)
        )
        |> Elm.Let.value "pageDetails"
            (Elm.apply
                Gen.App.Page.values_.toInternalDetails
                [ pageConfig ]
            )
        |> Elm.Let.toExpression


noneEffect =
    Elm.value
        { importFrom = [ "Effect" ]
        , name = "none"
        , annotation = Just types.effect
        }


storeValue storeId name =
    Elm.value
        { importFrom = [ "Store", storeId ]
        , name = "store"
        , annotation = Nothing
        }
        |> Elm.get name


updateStoreBranches :
    List Options.App.Store
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
    -> List Elm.Case.Branch
updateStoreBranches stores config storesState model =
    stores
        |> List.map
            (\store ->
                Elm.Case.branch
                    (Elm.Arg.customType ("Store" ++ store.id) identity
                        |> Elm.Arg.item (Elm.Arg.varWith "storeMsg" (Type.named [ "Store", store.id ] "Msg"))
                    )
                    (\storeMsg ->
                        let
                            storeUpdate =
                                Elm.apply
                                    (Elm.value
                                        { importFrom = [ "Store", store.id ]
                                        , name = "store"
                                        , annotation = Nothing
                                        }
                                        |> Elm.get "update"
                                    )
                                    [ storeMsg
                                    , model
                                        |> Elm.get "stores"
                                        |> Elm.get store.id
                                    ]
                        in
                        Elm.Let.letIn
                            (\( newStoreModel, newStoreEffect ) ->
                                let
                                    localStorageSync =
                                        Elm.Case.maybe (storeValue store.id "codec")
                                            { nothing = noneEffect
                                            , just =
                                                ( "codec"
                                                , \codec ->
                                                    Elm.apply
                                                        (Elm.value
                                                            { importFrom = [ "Effect", "LocalStorage" ]
                                                            , name = "save"
                                                            , annotation = Just (effectWith types.msg)
                                                            }
                                                        )
                                                        [ Elm.string store.id
                                                        , Elm.apply
                                                            (Elm.get "encode" codec)
                                                            [ newStoreModel ]
                                                        ]
                                                )
                                            }
                                in
                                Elm.tuple
                                    (model
                                        |> Elm.updateRecord
                                            [ ( "stores"
                                              , Elm.updateRecord
                                                    [ ( store.id
                                                      , newStoreModel
                                                      )
                                                    ]
                                                    (Elm.get "stores" model)
                                              )
                                            ]
                                    )
                                    (effectBatch
                                        [ effectMap (Elm.val ("Store" ++ store.id)) newStoreEffect
                                        , localStorageSync
                                        ]
                                    )
                            )
                            |> Elm.Let.unpack
                                (Elm.Arg.tuple
                                    (Elm.Arg.var "newStoreModel")
                                    (Elm.Arg.var "newStoreEffect")
                                )
                                storeUpdate
                            |> Elm.Let.toExpression
                    )
            )


updatePageBranches :
    List Options.App.PageUsage
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
    -> List Elm.Case.Branch
updatePageBranches pages config shared model =
    pages
        |> List.filterMap
            (\pageInfo ->
                let
                    stateKey =
                        pageInfo.id

                    pageModule =
                        pageInfo.moduleName

                    pageMsgTypeName =
                        types.toPageMsg pageInfo.id
                in
                Just <|
                    Elm.Case.branch
                        (Elm.Arg.customType pageMsgTypeName Tuple.pair
                            |> Elm.Arg.item (Elm.Arg.varWith "pageId" types.pageId)
                            |> Elm.Arg.item (Elm.Arg.varWith "pageMsg" (Type.named pageModule "Msg"))
                        )
                        (\( pageId, pageMsg ) ->
                            let
                                pageKey =
                                    Elm.apply
                                        (Elm.val "toPageKey")
                                        [ pageId ]
                            in
                            getPage pageKey
                                stateKey
                                (Elm.get "states" model)
                                { nothing = Elm.tuple model effectNone
                                , just =
                                    \pageState ->
                                        let
                                            updated =
                                                withPageHelper
                                                    (Elm.value
                                                        { importFrom = pageModule
                                                        , name = "page"
                                                        , annotation = Nothing
                                                        }
                                                    )
                                                    "update"
                                                    (\pageUpdate ->
                                                        Elm.apply pageUpdate
                                                            [ shared
                                                            , pageMsg
                                                            , pageState
                                                            ]
                                                    )
                                        in
                                        Elm.Let.letIn
                                            (\( innerPageModel, pageEffect ) ->
                                                let
                                                    pageModel =
                                                        Elm.apply (Elm.val stateKey)
                                                            [ innerPageModel ]
                                                in
                                                Elm.tuple
                                                    (model
                                                        |> Elm.updateRecord
                                                            [ ( "states", setState pageKey pageModel model )
                                                            ]
                                                    )
                                                    (pageEffect
                                                        |> effectMap
                                                            (Elm.apply
                                                                (Elm.val pageMsgTypeName)
                                                                [ pageId
                                                                ]
                                                            )
                                                    )
                                            )
                                            |> Elm.Let.unpack
                                                (Elm.Arg.tuple (Elm.Arg.var "updatedPage") (Elm.Arg.var "pageEffect"))
                                                updated
                                            |> Elm.Let.toExpression
                                }
                        )
            )


getPage :
    Elm.Expression
    -> String
    -> Elm.Expression
    ->
        { just : Elm.Expression -> Elm.Expression
        , nothing : Elm.Expression
        }
    -> Elm.Expression
getPage pageId pageConstructor states onFound =
    Elm.Case.maybe (Gen.App.State.call_.get pageId states)
        { nothing = onFound.nothing
        , just =
            Tuple.pair "foundPage" <|
                \foundPage ->
                    Elm.Case.custom foundPage
                        types.pageModel
                        [ Elm.Case.branch
                            (Elm.Arg.customType pageConstructor identity
                                |> Elm.Arg.item (Elm.Arg.varWith "page" types.pageModel)
                            )
                            onFound.just
                        , Elm.Case.branch Elm.Arg.ignore
                            (\_ ->
                                onFound.nothing
                            )
                        ]
        }
