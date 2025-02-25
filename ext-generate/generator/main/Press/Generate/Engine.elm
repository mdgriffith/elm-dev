module Press.Generate.Engine exposing (generate)

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
import Gen.App.View
import Gen.Browser
import Gen.Browser.Navigation
import Gen.Effect.LocalStorage
import Gen.Json.Decode
import Gen.Json.Encode
import Gen.List
import Gen.Listen
import Gen.Platform.Sub
import Gen.Result
import Gen.Tuple
import Gen.Url
import Options.App
import Options.Route
import Press.Generate.Regions
import Press.Model exposing (..)


generate : List Options.App.Store -> List Options.App.PageUsage -> Elm.File
generate stores allPageDefinitions =
    let
        pageUsages =
            List.filter (\pageInfo -> not pageInfo.urlOnly) allPageDefinitions

        loadPage =
            Press.Model.loadPage pageUsages

        getPageInit =
            Press.Model.getPageInit pageUsages
    in
    Elm.file [ "App" ]
        [ Elm.group
            [ Elm.alias "App"
                (Type.namedWith []
                    "Program"
                    [ Gen.Json.Encode.annotation_.value
                    , Type.namedWith [] "Model" [ Gen.Browser.Navigation.annotation_.key, Type.var "model" ]
                    , Type.namedWith [] "Msg" [ Type.var "msg" ]
                    ]
                )
                |> Elm.exposeConstructor
            , app pageUsages getPageInit loadPage
            ]

        -- You're going to be tempted to add aliases for `Page`, `Effect` and `Sub` here
        -- But they won't work because Page's can't import `App.elm`, be cause `App.elm` imports `Page.elm`
        , Elm.group
            [ Elm.alias "CmdOptions" types.cmdOptions
                |> Elm.exposeConstructor
            , Elm.alias "SubOptions" types.subOptions
                |> Elm.exposeConstructor
            ]
        , toEmptyStores stores
        , toPageKey pageUsages
        , toPageGroupKey pageUsages
        , toPageLimit pageUsages
        , Elm.group
            [ Elm.alias "Model" types.modelRecord
            , msgType stores pageUsages
            , update stores pageUsages getPageInit loadPage
            ]
        , Elm.customType "State"
            (let
                routeVariants =
                    pageUsages
                        |> List.filterMap
                            (\pageInfo ->
                                if pageInfo.elmModuleIsPresent then
                                    Elm.variantWith pageInfo.id
                                        [ Type.named pageInfo.moduleName "Model"
                                        ]
                                        |> Just

                                else
                                    Nothing
                            )
             in
             Elm.variantWith "PageError_" [ Gen.App.Page.Error.annotation_.error ]
                :: Elm.variantWith "PageLoading_" [ types.pageId ]
                :: routeVariants
            )
        , viewType
        , viewPageModel pageUsages
        , syncStoresToLocalStorage stores
        , getPageInit.declaration
        , loadPage.declaration
        , view pageUsages
        , getSubscriptions stores pageUsages
        , subscriptions pageUsages
        , Elm.group
            [ testAlias
            , test getPageInit loadPage
            ]
        ]


syncStoresToLocalStorage : List Options.App.Store -> Elm.Declaration
syncStoresToLocalStorage stores =
    Elm.declaration "syncStoresToLocalStorage"
        (Elm.fn
            (Elm.Arg.varWith "stores" storesType)
            (\storesState ->
                if List.isEmpty stores then
                    noneEffect

                else
                    stores
                        |> List.map
                            (\store ->
                                Elm.Case.maybe (storeValue store.id "codec")
                                    { nothing = noneEffect
                                    , just =
                                        ( "codec"
                                        , \codec ->
                                            Gen.Effect.LocalStorage.save
                                                store.id
                                                (Elm.apply
                                                    (Elm.get "encode" codec)
                                                    [ Elm.get store.id storesState
                                                    ]
                                                )
                                        )
                                    }
                            )
                        |> effectBatch
                        |> Elm.withType (types.effectWith types.msg)
            )
        )


storeValue storeId name =
    Elm.value
        { importFrom = [ "Store", storeId ]
        , name = "store"
        , annotation = Nothing
        }
        |> Elm.get name


toEmptyStores : List Options.App.Store -> Elm.Declaration
toEmptyStores stores =
    Elm.declaration "initStores"
        (Elm.fn3
            (Elm.Arg.varWith "flags" Gen.Json.Encode.annotation_.value)
            (Elm.Arg.varWith "viewing" Press.Model.regionsRecord)
            (Elm.Arg.varWith "url" Gen.Url.annotation_.url)
            (\flags viewing url ->
                case stores of
                    [] ->
                        Elm.tuple (Elm.record [ ( "viewing", viewing ) ]) effectNone
                            |> Elm.withType
                                (Type.tuple
                                    storesType
                                    (effectWith types.msg)
                                )

                    _ ->
                        Elm.Let.letIn
                            (\stateAndEffectRecord ->
                                let
                                    stateRecordFields =
                                        stores
                                            |> List.map
                                                (\store ->
                                                    ( store.id
                                                    , Gen.Tuple.first (Elm.get store.id stateAndEffectRecord)
                                                    )
                                                )

                                    stateRecord =
                                        Elm.record (( "viewing", viewing ) :: stateRecordFields)

                                    finalEffects =
                                        stores
                                            |> List.map
                                                (\store ->
                                                    Gen.Tuple.second (Elm.get store.id stateAndEffectRecord)
                                                        |> effectMap (Elm.val ("Store" ++ store.id))
                                                )
                                            |> effectBatch
                                in
                                Elm.tuple stateRecord finalEffects
                                    |> Elm.withType
                                        (Type.tuple
                                            storesType
                                            (effectWith types.msg)
                                        )
                            )
                            |> Elm.Let.value "updatedStores"
                                (stores
                                    |> List.map
                                        (\store ->
                                            let
                                                cachedModel =
                                                    Elm.Case.maybe (storeValue store.id "codec")
                                                        { nothing = Elm.nothing
                                                        , just =
                                                            ( "codec"
                                                            , \codec ->
                                                                let
                                                                    decoder =
                                                                        Gen.Json.Decode.field "localStorage"
                                                                            (Gen.Json.Decode.field store.id (Elm.get "decoder" codec))
                                                                in
                                                                Gen.Result.toMaybe (Gen.Json.Decode.decodeValue decoder flags)
                                                            )
                                                        }

                                                updatedStorePair =
                                                    Elm.apply
                                                        (Elm.value
                                                            { importFrom = [ "Store", store.id ]
                                                            , name = "store"
                                                            , annotation = Nothing
                                                            }
                                                            |> Elm.get "init"
                                                        )
                                                        [ flags
                                                        , url
                                                        , cachedModel
                                                        ]
                                            in
                                            ( store.id
                                            , updatedStorePair
                                            )
                                        )
                                    |> Elm.record
                                )
                            |> Elm.Let.toExpression
            )
        )


toPageGroupKey : List Options.App.PageUsage -> Elm.Declaration
toPageGroupKey pages =
    .declaration <|
        Elm.Declare.fn "toPageGroupKey"
            (Elm.Arg.varWith "pageId" types.pageId)
            (\pageId ->
                Elm.Case.custom pageId
                    types.pageId
                    (pages
                        |> List.map
                            (\pageInfo ->
                                toPageBranch pageInfo
                                    (\params ->
                                        Elm.string pageInfo.id
                                    )
                            )
                    )
                    |> Elm.withType Type.string
            )


toPageLimit : List Options.App.PageUsage -> Elm.Declaration
toPageLimit pages =
    .declaration <|
        Elm.Declare.fn "toPageLimit"
            (Elm.Arg.varWith "pageId" types.pageId)
            (\pageId ->
                Elm.Case.custom pageId
                    types.pageId
                    (pages
                        |> List.map
                            (\pageInfo ->
                                if pageInfo.elmModuleIsPresent then
                                    let
                                        pageConfig =
                                            Elm.value
                                                { importFrom = pageInfo.moduleName
                                                , name = "page"
                                                , annotation = Nothing
                                                }
                                    in
                                    toPageBranch pageInfo
                                        (\_ ->
                                            Elm.apply
                                                Gen.App.Page.values_.toInternalDetails
                                                [ pageConfig ]
                                                |> Elm.get ".pageCacheLimit"
                                        )

                                else
                                    toPageBranch pageInfo
                                        (\params ->
                                            Elm.int 1
                                        )
                            )
                    )
                    |> Elm.withType Type.int
            )


toPageKey : List Options.App.PageUsage -> Elm.Declaration
toPageKey pages =
    .declaration <|
        Elm.Declare.fn "toPageKey"
            (Elm.Arg.varWith "pageId" types.pageId)
            (\pageId ->
                Elm.Case.custom pageId
                    types.pageId
                    (pages
                        |> List.map
                            (\pageInfo ->
                                if pageInfo.elmModuleIsPresent then
                                    let
                                        pageModule =
                                            pageInfo.moduleName

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
                                                (\pageDetails ->
                                                    Elm.Case.maybe (Elm.get "toKey" pageDetails)
                                                        { nothing =
                                                            case pageInfo.route of
                                                                Nothing ->
                                                                    Elm.string pageInfo.id

                                                                Just pageRoute ->
                                                                    let
                                                                        vars =
                                                                            Options.Route.toUrlVariables pageRoute
                                                                    in
                                                                    case vars of
                                                                        [] ->
                                                                            Elm.string pageInfo.id

                                                                        first :: remaining ->
                                                                            Elm.Op.append
                                                                                (Elm.string (pageInfo.id ++ "/"))
                                                                                (List.foldl
                                                                                    (\field acc ->
                                                                                        Elm.Op.append
                                                                                            acc
                                                                                            (Elm.Op.append
                                                                                                (Elm.string "/")
                                                                                                (Elm.get field params)
                                                                                            )
                                                                                    )
                                                                                    (Elm.get first params)
                                                                                    remaining
                                                                                )
                                                        , just =
                                                            ( "toKey"
                                                            , \toKey ->
                                                                Elm.Op.append
                                                                    (Elm.string pageInfo.id)
                                                                    (Elm.apply toKey [ params ])
                                                            )
                                                        }
                                                )
                                                |> Elm.Let.value "pageDetails"
                                                    (Elm.apply
                                                        Gen.App.Page.values_.toInternalDetails
                                                        [ pageConfig ]
                                                    )
                                                |> Elm.Let.toExpression
                                        )

                                else
                                    toPageBranch pageInfo
                                        (\params ->
                                            Elm.string pageInfo.id
                                        )
                            )
                    )
                    |> Elm.withType Type.string
            )


msgType : List Options.App.Store -> List Options.App.PageUsage -> Elm.Declaration
msgType stores pageUsages =
    let
        pageVariants =
            pageUsages
                |> List.filterMap
                    (\pageInfo ->
                        if pageInfo.elmModuleIsPresent then
                            Just
                                (Elm.variantWith
                                    (types.toPageMsg pageInfo.id)
                                    [ types.pageId
                                    , Type.named pageInfo.moduleName "Msg"
                                    ]
                                )

                        else
                            Nothing
                    )

        storeVariants =
            stores
                |> List.map
                    (\store ->
                        Elm.variantWith ("Store" ++ store.id)
                            [ Type.named [ "Store", store.id ] "Msg" ]
                    )
    in
    Elm.customType "Msg"
        ([ Elm.variant "PageCacheCleared"
         , Elm.variantWith "Preload" [ types.pageId ]
         , Elm.variantWith "ViewUpdated" [ types.regionOperation ]
         , Elm.variantWith "Broadcast" [ Type.named [ "Broadcast" ] "Msg" ]
         , Elm.variantWith "SubscriptionEventIgnored" [ Type.string ]
         , Elm.variantWith "Global" [ Type.var "msg" ]
         , Elm.variantWith "Loaded"
            [ types.pageId
            , types.pageLoadResult
            ]
         ]
            ++ storeVariants
            ++ pageVariants
        )
        |> Elm.expose


viewType =
    Elm.customType "View"
        [ Elm.variantWith "NotFound" []
        , Elm.variantWith "Loading" [ types.pageId ]
        , Elm.variantWith "Error" [ Gen.App.Page.Error.annotation_.error ]
        , Elm.variantWith "View"
            [ Gen.App.View.annotation_.view (Type.var "appMsg")
            ]
        ]
        |> Elm.exposeConstructor


testAlias =
    Elm.alias "Test"
        (Type.record
            [ ( "init"
              , Type.function
                    [ Gen.Json.Encode.annotation_.value
                    , Gen.Url.annotation_.url
                    , Type.unit
                    ]
                    (Type.tuple types.testModel (types.effectWith types.msg))
              )
            , ( "view", Type.function [ types.testModel ] (Gen.Browser.annotation_.document types.msg) )
            , ( "update", Type.function [ types.msg, types.testModel ] (Type.tuple types.testModel (types.effectWith types.msg)) )
            , ( "onUrlRequest", Type.function [ Gen.Browser.annotation_.urlRequest ] types.msg )
            , ( "onUrlChange", Type.function [ Gen.Url.annotation_.url ] types.msg )
            ]
        )
        |> Elm.exposeConstructor


{-|

    { init : flags -> Url -> () -> ( model, effect )
    , view : model -> Document msg
    , update : msg -> model -> ( model, effect )
    , onUrlRequest : UrlRequest -> msg
    , onUrlChange : Url -> msg
    }

-}
test getPageInit loadPage =
    Elm.declaration "test"
        (Elm.fn
            (Elm.Arg.varWith "config" types.frameTest)
            (\config ->
                Elm.record
                    [ ( "init"
                      , Elm.fn3
                            (Elm.Arg.varWith "flags" Gen.Json.Encode.annotation_.value)
                            (Elm.Arg.varWith "url" Gen.Url.annotation_.url)
                            (Elm.Arg.varWith "key" Type.unit)
                            (\flags url key ->
                                init getPageInit loadPage config flags url key
                            )
                      )
                    , ( "view", Elm.apply (Elm.val "view") [ config ] )
                    , ( "update", Elm.apply (Elm.val "update") [ config ] )
                    , ( "onUrlChange"
                      , Elm.fn (Elm.Arg.varWith "url" Gen.Url.annotation_.url)
                            (\url ->
                                Elm.apply (Elm.val "Global")
                                    [ Elm.apply
                                        (Elm.get "onUrlChange" config)
                                        [ url ]
                                    ]
                            )
                      )
                    , ( "onUrlRequest"
                      , Elm.fn (Elm.Arg.varWith "urlRequest" Gen.Browser.annotation_.urlRequest)
                            (\urlRequest ->
                                Elm.apply (Elm.val "Global")
                                    [ Elm.apply
                                        (Elm.get "onUrlRequest" config)
                                        [ urlRequest ]
                                    ]
                            )
                      )
                    ]
                    |> Elm.withType (Type.namedWith [] "Test" [ Type.var "model", Type.var "msg" ])
            )
        )
        |> Elm.exposeConstructor


app routes getPageInit loadPage =
    Elm.declaration "app"
        (Elm.fn
            (Elm.Arg.varWith
                "config"
                types.frame
            )
            (\config ->
                Gen.Browser.call_.application
                    (Elm.record
                        [ ( "init"
                          , Elm.fn3
                                (Elm.Arg.varWith "flags" Gen.Json.Encode.annotation_.value)
                                (Elm.Arg.varWith "url" Gen.Url.annotation_.url)
                                (Elm.Arg.varWith "key" Gen.Browser.Navigation.annotation_.key)
                                (\flags url key ->
                                    Elm.Let.letIn
                                        (\( newModel, effect ) ->
                                            Elm.tuple newModel
                                                (Press.Model.toCmd config
                                                    (Elm.get "stores" newModel)
                                                    (Elm.get "key" newModel)
                                                    (Elm.get "app" newModel)
                                                    effect
                                                )
                                        )
                                        |> Elm.Let.unpack
                                            (Elm.Arg.tuple
                                                (Elm.Arg.var "newModel")
                                                (Elm.Arg.var "effect")
                                            )
                                            (init getPageInit loadPage config flags url key)
                                        |> Elm.Let.toExpression
                                )
                          )
                        , ( "update"
                          , Elm.fn2
                                (Elm.Arg.varWith "msg" types.msg)
                                (Elm.Arg.varWith "model" types.model)
                                (\msg model ->
                                    Elm.Let.letIn
                                        (\( newModel, effect ) ->
                                            Elm.tuple newModel
                                                (Press.Model.toCmd
                                                    config
                                                    (Elm.get "stores" newModel)
                                                    (Elm.get "key" newModel)
                                                    (Elm.get "app" newModel)
                                                    effect
                                                )
                                        )
                                        |> Elm.Let.unpack
                                            (Elm.Arg.tuple
                                                (Elm.Arg.var "newModel")
                                                (Elm.Arg.var "effect")
                                            )
                                            (Elm.apply (Elm.val "update") [ config, msg, model ])
                                        |> Elm.Let.toExpression
                                )
                          )
                        , ( "view", Elm.apply (Elm.val "view") [ config ] )
                        , ( "subscriptions", Elm.apply (Elm.val "subscriptions") [ config ] )
                        , ( "onUrlChange"
                          , Elm.fn (Elm.Arg.varWith "url" Gen.Url.annotation_.url)
                                (\url ->
                                    Elm.apply (Elm.val "Global")
                                        [ Elm.apply
                                            (Elm.get "onUrlChange" config)
                                            [ url ]
                                        ]
                                )
                          )
                        , ( "onUrlRequest"
                          , Elm.fn (Elm.Arg.varWith "urlRequest" Gen.Browser.annotation_.urlRequest)
                                (\urlRequest ->
                                    Elm.apply (Elm.val "Global")
                                        [ Elm.apply
                                            (Elm.get "onUrlRequest" config)
                                            [ urlRequest ]
                                        ]
                                )
                          )
                        ]
                    )
                    |> Elm.withType (Type.namedWith [] "App" [ Type.var "model", Type.var "msg" ])
            )
        )
        |> Elm.exposeConstructor


initStores : Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
initStores flags viewing url =
    Elm.apply
        (Elm.value
            { importFrom = []
            , name = "initStores"
            , annotation =
                Just
                    (Type.tuple
                        storesType
                        (Type.namedWith [ "Effect" ] "Effect" [ types.msg ])
                    )
            }
        )
        [ flags, viewing, url ]


init getPageInit loadPage config flags url key =
    let
        viewing =
            Press.Generate.Regions.values.empty
    in
    Elm.Let.letIn
        (\( stores, storeEffects ) ->
            let
                frameInitialized =
                    Elm.apply
                        (Elm.get "init" config)
                        [ stores
                        , flags
                        , url
                        ]
            in
            Elm.Let.letIn
                (\( frameModel, frameEffect ) ->
                    let
                        globalFrameEffect =
                            frameEffect
                                |> effectMap (Elm.val "Global")

                        model =
                            Elm.record
                                [ ( "key", key )
                                , ( "app", frameModel )
                                , ( "stores", stores )
                                , ( "limits", Gen.App.State.initLimit )
                                , ( "states"
                                  , Gen.App.State.init
                                  )
                                ]
                                |> Elm.withType types.model
                    in
                    Elm.tuple model
                        (effectBatch
                            [ globalFrameEffect
                            , storeEffects
                            , Elm.apply (Elm.val "syncStoresToLocalStorage")
                                [ stores
                                ]
                            ]
                        )
                )
                |> Elm.Let.unpack
                    (Elm.Arg.tuple
                        (Elm.Arg.var "appModel")
                        (Elm.Arg.var "appEffect")
                    )
                    frameInitialized
                |> Elm.Let.toExpression
        )
        |> Elm.Let.unpack
            (Elm.Arg.tuple
                (Elm.Arg.var "stores")
                (Elm.Arg.var "storeEffects")
            )
            (initStores flags viewing url)
        |> Elm.Let.toExpression


update :
    List Options.App.Store
    -> List Options.App.PageUsage
    ->
        { a
            | call :
                Elm.Expression
                -> Elm.Expression
                -> Elm.Expression
                -> Elm.Expression
        }
    ->
        Elm.Declare.Function
            (Elm.Expression
             -> Elm.Expression
             -> Elm.Expression
             -> Elm.Expression
             -> Elm.Expression
            )
    -> Elm.Declaration
update stores routes getPageInit loadPage =
    Elm.declaration "update"
        (Elm.fn3
            (Elm.Arg.varWith "config" types.frameUpdate)
            (Elm.Arg.varWith "msg" types.msg)
            (Elm.Arg.varWith "model" types.model)
            (\config msg model ->
                Elm.Case.custom msg
                    types.msg
                    ([ Elm.Case.branch (Elm.Arg.customType "PageCacheCleared" ())
                        (\_ ->
                            Elm.tuple
                                model
                                effectNone
                        )
                     , Elm.Case.branch
                        (Elm.Arg.customType "Preload" identity
                            |> Elm.Arg.item (Elm.Arg.varWith "pageId" types.pageId)
                        )
                        (\pageId ->
                            let
                                pageInit =
                                    getPageInit.call pageId
                                        (Elm.get "stores" model)
                                        (Elm.get "states" model)
                            in
                            pageInit
                                |> Elm.Op.pipe (Elm.apply loadPage.value [ config, model, pageId ])
                        )
                     , Elm.Case.branch
                        (Elm.Arg.customType "Loaded" Tuple.pair
                            |> Elm.Arg.item (Elm.Arg.varWith "pageId" types.pageId)
                            |> Elm.Arg.item (Elm.Arg.varWith "initialization" types.pageLoadResult)
                        )
                        (\( pageId, initialization ) ->
                            loadPage.call config model pageId initialization
                        )
                     , Elm.Case.branch
                        (Elm.Arg.customType "Broadcast" identity
                            |> Elm.Arg.item
                                (Elm.Arg.varWith "broadcastMsg" types.broadcast)
                        )
                        (\broadcastMsg ->
                            Elm.Let.letIn
                                (\pageMsgList ->
                                    Gen.List.call_.foldl
                                        (Elm.fn2
                                            (Elm.Arg.var "pageMsg")
                                            (Elm.Arg.tuple (Elm.Arg.var "innerModel") (Elm.Arg.var "innerEffect"))
                                            (\pageMsg ( innerModel, innerEffect ) ->
                                                Elm.Let.letIn
                                                    (\( newModel, newEffect ) ->
                                                        Elm.tuple
                                                            newModel
                                                            (effectBatch
                                                                [ newEffect
                                                                , innerEffect
                                                                ]
                                                            )
                                                    )
                                                    |> Elm.Let.unpack
                                                        (Elm.Arg.tuple
                                                            (Elm.Arg.var "newModel")
                                                            (Elm.Arg.var "newEffect")
                                                        )
                                                        (Elm.apply (Elm.val "update") [ config, pageMsg, innerModel ])
                                                    |> Elm.Let.toExpression
                                            )
                                        )
                                        (Elm.tuple model effectNone)
                                        pageMsgList
                                )
                                |> Elm.Let.value "pageMsgList"
                                    (Gen.Listen.broadcastListeners broadcastMsg
                                        (Elm.apply
                                            (Elm.val "getSubscriptions")
                                            [ config
                                            , model
                                            ]
                                        )
                                    )
                                |> Elm.Let.toExpression
                        )
                     , Elm.Case.branch
                        (Elm.Arg.customType "ViewUpdated" identity
                            |> Elm.Arg.item (Elm.Arg.varWith "operation" types.regionOperation)
                        )
                        (\regionOperation ->
                            Elm.Let.letIn
                                (\( newRegions, regionDiff ) liveStores ->
                                    Elm.get "added" regionDiff
                                        |> Gen.List.call_.foldl
                                            (Elm.fn2
                                                (Elm.Arg.var "pageId")
                                                (Elm.Arg.var "inner")
                                                (\pageId existingTuple ->
                                                    Elm.Let.letIn
                                                        (\( innerModel, innerEffect ) ->
                                                            let
                                                                pageInit =
                                                                    getPageInit.call pageId
                                                                        (Elm.get "stores" innerModel)
                                                                        (Elm.get "states" innerModel)

                                                                preloadedTuple =
                                                                    -- loadPage.call config innerModel pageId pageInit
                                                                    pageInit
                                                                        |> Elm.Op.pipe (Elm.apply loadPage.value [ config, innerModel, pageId ])
                                                            in
                                                            Elm.Let.letIn
                                                                (\( preloadedModel, preloadedEffect ) ->
                                                                    Elm.tuple preloadedModel
                                                                        (effectBatch
                                                                            [ innerEffect
                                                                            , preloadedEffect
                                                                            ]
                                                                        )
                                                                )
                                                                |> Elm.Let.unpack
                                                                    (Elm.Arg.tuple
                                                                        (Elm.Arg.var "preloadedModel")
                                                                        (Elm.Arg.var "preloadedEffect")
                                                                    )
                                                                    preloadedTuple
                                                                |> Elm.Let.toExpression
                                                        )
                                                        |> Elm.Let.unpack
                                                            (Elm.Arg.tuple
                                                                (Elm.Arg.var "innerModel")
                                                                (Elm.Arg.var "innerEffect")
                                                            )
                                                            existingTuple
                                                        |> Elm.Let.toExpression
                                                )
                                            )
                                            (Elm.tuple
                                                (Elm.updateRecord
                                                    [ ( "stores"
                                                      , liveStores
                                                            |> Elm.updateRecord
                                                                [ ( "viewing", newRegions )
                                                                ]
                                                      )
                                                    ]
                                                    model
                                                )
                                                effectNone
                                            )
                                )
                                |> Elm.Let.unpack
                                    (Elm.Arg.tuple
                                        (Elm.Arg.var "newRegions")
                                        (Elm.Arg.var "regionDiff")
                                    )
                                    (Press.Generate.Regions.values.update
                                        regionOperation
                                        (Elm.get "viewing" (Elm.get "stores" model))
                                    )
                                |> Elm.Let.value "stores" (Elm.get "stores" model)
                                |> Elm.Let.toExpression
                        )
                     , Elm.Case.branch
                        (Elm.Arg.customType "SubscriptionEventIgnored" identity
                            |> Elm.Arg.item (Elm.Arg.varWith "message" types.pageId)
                        )
                        (\_ ->
                            Elm.tuple
                                model
                                effectNone
                        )
                     , Elm.Case.branch
                        (Elm.Arg.customType "Global" identity
                            |> Elm.Arg.item (Elm.Arg.varWith "appMsg" (Type.var "appMsg"))
                        )
                        (\appMsg ->
                            let
                                updatedFrame =
                                    Elm.apply (Elm.get "update" config)
                                        [ Elm.get "stores" model
                                        , appMsg
                                        , Elm.get "app" model
                                        ]
                            in
                            Elm.Let.letIn
                                (\( newFrame, frameEffect ) ->
                                    Elm.tuple
                                        (model
                                            |> Elm.updateRecord
                                                [ ( "app", newFrame )
                                                ]
                                        )
                                        (frameEffect
                                            |> effectMap (Elm.val "Global")
                                        )
                                )
                                |> Elm.Let.unpack
                                    (Elm.Arg.tuple
                                        (Elm.Arg.var "newFrame")
                                        (Elm.Arg.var "frameEffect")
                                    )
                                    updatedFrame
                                |> Elm.Let.toExpression
                        )
                     ]
                        ++ Press.Model.updateStoreBranches stores
                            config
                            (Elm.get "stores" model)
                            model
                        ++ Press.Model.updatePageBranches routes
                            config
                            (Elm.get "stores" model)
                            model
                    )
                    |> Elm.withType (Type.tuple types.model (types.effectWith types.msg))
            )
        )


view : List Options.App.PageUsage -> Elm.Declaration
view routes =
    Elm.declaration "view"
        (Elm.fn2
            (Elm.Arg.varWith "config" types.frameView)
            (Elm.Arg.varWith "model" types.model)
            (\config model ->
                let
                    frameView pageView =
                        Elm.apply
                            (Elm.get "view" config)
                            [ Elm.get "stores" model
                            , Elm.val "Global"
                            , Elm.get "app" model
                            , pageView
                            ]
                            |> Elm.withType (Gen.Browser.annotation_.document types.msg)
                in
                Elm.Let.letIn frameView
                    |> Elm.Let.value "viewRegions"
                        (Elm.apply
                            (Elm.value
                                { importFrom = [ "App", "View", "Id" ]
                                , name = "mapRegion"
                                , annotation = Nothing
                                }
                            )
                            [ Elm.apply
                                (Elm.val "viewPageModel")
                                [ Elm.get "stores" model
                                , Elm.get "states" model
                                ]
                            , Elm.get "viewing" (Elm.get "stores" model)
                            ]
                        )
                    |> Elm.Let.toExpression
            )
        )


viewPageModel : List Options.App.PageUsage -> Elm.Declaration
viewPageModel pages =
    Elm.declaration "viewPageModel"
        (Elm.fnBuilder
            (\stores states regionId pageId ->
                let
                    pageKey =
                        Elm.apply (Elm.val "toPageKey") [ pageId ]
                in
                Elm.Case.maybe (Gen.App.State.call_.get pageKey states)
                    { nothing =
                        Elm.val "NotFound"
                    , just =
                        ( "currentState"
                        , \current ->
                            Elm.Case.custom current
                                types.pageModel
                                (Elm.Case.branch
                                    (Elm.Arg.customType "PageError_" identity
                                        |> Elm.Arg.item (Elm.Arg.varWith "pageError" Gen.App.Page.Error.annotation_.error)
                                    )
                                    (\err ->
                                        Elm.apply
                                            (Elm.val "Error")
                                            [ err ]
                                    )
                                    :: Elm.Case.branch
                                        (Elm.Arg.customType "PageLoading_" identity
                                            |> Elm.Arg.item (Elm.Arg.varWith "loadingPageId" types.pageId)
                                        )
                                        (\loadingPageId ->
                                            Elm.apply
                                                (Elm.val "Loading")
                                                [ loadingPageId ]
                                        )
                                    :: List.filterMap (routeToView stores regionId pageId) pages
                                )
                        )
                    }
                    |> Elm.withType (Type.namedWith [] "View" [ appMsg ])
            )
            |> Elm.fnArg (Elm.Arg.varWith "stores" storesType)
            |> Elm.fnArg (Elm.Arg.varWith "states" types.stateCache)
            |> Elm.fnArg (Elm.Arg.varWith "regionId" types.regionIdType)
            |> Elm.fnArg (Elm.Arg.varWith "pageId" types.pageId)
            |> Elm.fnDone
        )


routeToView stores regionId pageId pageInfo =
    if pageInfo.elmModuleIsPresent then
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
                (Elm.Arg.customType stateKey identity
                    |> Elm.Arg.item (Elm.Arg.varWith "pageModel" (Type.named pageModule "Model"))
                )
                (\pageState ->
                    Press.Model.withPageHelper
                        (Elm.value
                            { importFrom = pageModule
                            , name = "page"
                            , annotation = Nothing
                            }
                        )
                        "view"
                        (\pageView ->
                            Elm.Let.letIn
                                (\pageViewResult ->
                                    Elm.Case.result pageViewResult
                                        { err =
                                            ( "pageError"
                                            , \pageError ->
                                                Elm.apply
                                                    (Elm.val "Error")
                                                    [ pageError ]
                                            )
                                        , ok =
                                            ( "pageViewSuccess"
                                            , \pageViewSuccess ->
                                                Elm.apply (Elm.val "View")
                                                    [ Gen.App.View.call_.map
                                                        (Elm.fn
                                                            (Elm.Arg.var "innerMsg")
                                                            (\innerMsg ->
                                                                Elm.apply
                                                                    (Elm.val pageMsgTypeName)
                                                                    [ pageId
                                                                    , innerMsg
                                                                    ]
                                                            )
                                                        )
                                                        pageViewSuccess
                                                    ]
                                            )
                                        }
                                )
                                |> Elm.Let.value "pageViewResult"
                                    (Elm.apply pageView
                                        [ regionId
                                        , stores
                                        , pageState
                                        ]
                                    )
                                |> Elm.Let.toExpression
                        )
                )

    else
        Nothing


getSubscriptions : List Options.App.Store -> List Options.App.PageUsage -> Elm.Declaration
getSubscriptions stores pages =
    Elm.declaration "getSubscriptions"
        (Elm.fn2
            (Elm.Arg.varWith "config" types.frameSub)
            (Elm.Arg.varWith "model" types.model)
            (\config model ->
                Gen.Listen.batch
                    [ Elm.apply
                        (Elm.get "subscriptions" config)
                        [ Elm.get "stores" model
                        , Elm.get "app" model
                        ]
                        |> Gen.Listen.call_.map (Elm.val "Global")
                    , case stores of
                        [] ->
                            Gen.Listen.none

                        _ ->
                            stores
                                |> List.map
                                    (\store ->
                                        toSub config
                                            (Elm.get "stores" model)
                                            (Elm.get "app" model)
                                            (Elm.apply
                                                (storeValue store.id "subscriptions")
                                                [ model
                                                ]
                                            )
                                    )
                                |> Gen.Listen.batch
                    , Elm.apply Press.Generate.Regions.values.toList
                        [ Elm.get "viewing" (Elm.get "stores" model) ]
                        |> Gen.List.call_.filterMap
                            (Elm.fn
                                (Elm.Arg.varWith "pageId" Type.string)
                                (\pageId ->
                                    let
                                        pageKey =
                                            Elm.apply (Elm.val "toPageKey") [ pageId ]
                                    in
                                    Elm.Case.maybe (Gen.App.State.call_.get pageKey (Elm.get "states" model))
                                        { nothing = Elm.nothing
                                        , just =
                                            ( "pageState"
                                            , \pageState ->
                                                Elm.just (pageModelToSubscription config model pages pageState pageId)
                                            )
                                        }
                                )
                            )
                        |> Gen.Listen.call_.batch
                    ]
            )
            |> Elm.withType
                (Type.function
                    [ types.frameSub
                    , types.model
                    ]
                    (Gen.Listen.annotation_.listen types.msg)
                )
        )


subscriptions : List Options.App.PageUsage -> Elm.Declaration
subscriptions pages =
    Elm.declaration "subscriptions"
        (Elm.fn2
            (Elm.Arg.varWith "config" types.frameSub)
            (Elm.Arg.varWith "model" types.model)
            (\config model ->
                toSub config (Elm.get "stores" model) (Elm.get "app" model) <|
                    Elm.apply
                        (Elm.val "getSubscriptions")
                        [ config
                        , model
                        ]
            )
            |> Elm.withType
                (Type.function
                    [ types.frameSub
                    , types.model
                    ]
                    (Gen.Platform.Sub.annotation_.sub types.msg)
                )
        )


pageModelToSubscription :
    Elm.Expression
    -> Elm.Expression
    -> List Options.App.PageUsage
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
pageModelToSubscription config model pages current pageId =
    Elm.Case.custom current
        types.pageModel
        (Elm.Case.branch
            (Elm.Arg.customType "PageError_" identity
                |> Elm.Arg.item (Elm.Arg.varWith "pageError" Gen.App.Page.Error.annotation_.error)
            )
            (\err -> Gen.Listen.none)
            :: Elm.Case.branch
                (Elm.Arg.customType "PageLoading_" identity
                    |> Elm.Arg.item (Elm.Arg.varWith "pageId_" types.pageId)
                )
                (\_ -> Gen.Listen.none)
            :: List.filterMap
                (pageInfoToSubscriptioon config model pageId)
                pages
        )


pageInfoToSubscriptioon :
    Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
    -> Options.App.PageUsage
    -> Maybe Elm.Case.Branch
pageInfoToSubscriptioon config model pageId pageInfo =
    if pageInfo.elmModuleIsPresent then
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
                (Elm.Arg.customType stateKey identity
                    |> Elm.Arg.item (Elm.Arg.varWith "pageModel" (Type.named pageModule "Model"))
                )
                (\pageState ->
                    Press.Model.withPageHelper
                        (Elm.value
                            { importFrom = pageModule
                            , name = "page"
                            , annotation = Nothing
                            }
                        )
                        "subscriptions"
                        (\pageSubs ->
                            Elm.apply pageSubs
                                [ Elm.get "stores" model
                                , pageState
                                ]
                                |> Gen.Listen.call_.map
                                    (Elm.apply (Elm.val pageMsgTypeName)
                                        [ pageId ]
                                    )
                        )
                )

    else
        Nothing
