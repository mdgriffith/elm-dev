module Press.Generate exposing
    ( Error
    , errorToDetails
    , generate
    )

{-| Press generates the minimal amount needed to have a working app.

The goal is to generate pieces that you can integrate into an existing Elm app.

List of things to generate

1.  The directory of all source files used to generate stuff.
    This can be used to show a sidebar r a directory of all informations.

2.  A route parser and encoder.

3.  Files for each markdown file.

It will also generate a full app for you??

-}

import Elm
import Elm.Annotation as Type
import Elm.Arg
import Elm.Case
import Generate.Route
import Options.App
import Parser exposing ((|.), (|=))
import Press.Generate.Engine
import Press.Model exposing (..)


type alias Error =
    Generate.Route.Error


errorToDetails : Error -> { title : String, description : String }
errorToDetails error =
    Generate.Route.errorToDetails error


generate : Options.App.Options -> Result (List Error) (List Elm.File)
generate options =
    let
        routes =
            List.filterMap .route options.pages

        pages =
            List.map populateParamType options.pages
    in
    case Generate.Route.generate routes of
        Err err ->
            Err err

        Ok routeFile ->
            Ok
                (Press.Generate.Engine.generate options.stores pages
                    :: generatePageId options.pages
                    :: routeFile
                    :: generateStores options.stores
                )


toParamTypeString : { page | id : String } -> String
toParamTypeString page =
    String.join "_" (String.split "." page.id) ++ "_Params"


populateParamType : Options.App.PageUsage -> Options.App.PageUsage
populateParamType page =
    { page | paramType = Just (toParamTypeString page) }


generateStores : List Options.App.Store -> List Elm.File
generateStores stores =
    let
        userDefinedStores =
            List.map
                (\store ->
                    ( store.id
                    , Type.named [ "Store", store.id ] "Model"
                    )
                )
                stores

        viewingStore =
            ( "viewing"
            , Press.Model.regionsRecord
            )
    in
    [ Elm.file [ "App", "Stores" ]
        [ Elm.alias "Stores"
            (Type.record
                (viewingStore :: userDefinedStores)
            )
        ]
    ]


generatePageId : List Options.App.PageUsage -> Elm.File
generatePageId pageUsages =
    let
        pageIdType =
            Elm.customType "Id"
                (List.filterMap
                    (\page ->
                        if page.urlOnly then
                            Nothing

                        else
                            Just
                                (Elm.variantWith (String.replace "." "" page.id) [ Type.named [] (toParamTypeString page) ])
                    )
                    pageUsages
                )

        paramAliases =
            List.filterMap
                (\page ->
                    if page.urlOnly then
                        Nothing

                    else
                        Just <|
                            case page.route of
                                Nothing ->
                                    Elm.alias
                                        (toParamTypeString page)
                                        (Type.record [])

                                Just parsedRoute ->
                                    case Generate.Route.checkForErrors [ parsedRoute ] of
                                        Err _ ->
                                            Elm.alias
                                                (toParamTypeString page)
                                                (Type.record [])

                                        Ok [ route ] ->
                                            Elm.alias
                                                (toParamTypeString page)
                                                (Type.named [ "App", "Route" ] (toParamTypeString page))

                                        _ ->
                                            Elm.alias
                                                (toParamTypeString page)
                                                (Type.record [])
                )
                pageUsages

        fromRoute =
            Elm.declaration "fromRoute"
                (Elm.fn
                    (Elm.Arg.varWith "route" (Type.named [ "App", "Route" ] "Route"))
                    (\route ->
                        Elm.Case.custom route
                            (Type.named [ "App", "Route" ] "Route")
                            (List.filterMap
                                (\page ->
                                    case page.route of
                                        Nothing ->
                                            Nothing

                                        Just parsedRoute ->
                                            case Generate.Route.checkForErrors [ parsedRoute ] of
                                                Err _ ->
                                                    Nothing

                                                Ok [ pageRoute ] ->
                                                    Just
                                                        (Elm.Case.branch
                                                            (Elm.Arg.customType (String.replace "." "" pageRoute.id) identity
                                                                |> Elm.Arg.item
                                                                    (Elm.Arg.varWith "params"
                                                                        (Type.named [ "App", "Route" ]
                                                                            (toParamTypeString pageRoute)
                                                                        )
                                                                    )
                                                            )
                                                            (\params ->
                                                                if page.urlOnly then
                                                                    Elm.nothing

                                                                else
                                                                    Elm.apply
                                                                        (Elm.val (String.replace "." "" pageRoute.id))
                                                                        [ params ]
                                                                        |> Elm.just
                                                            )
                                                        )

                                                _ ->
                                                    Nothing
                                )
                                pageUsages
                            )
                            |> Elm.withType (Type.maybe (Type.named [] "Id"))
                    )
                )
    in
    Elm.file
        [ "App", "Page", "Id" ]
        (pageIdType :: fromRoute :: paramAliases)
