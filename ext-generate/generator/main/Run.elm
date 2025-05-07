module Run exposing (main)

{-| -}

import Gen.CodeGen.Generate as Generate
import Generate.Assets
import Generate.Docs
import Json.Decode
import Options.App
import Options.Assets
import Options.Docs
import Press.Generate
import Press.Generate.Regions
import Press.Model
import Theme
import Theme.Decoder
import Theme.Generate


type alias Runs =
    { app : Maybe Options.App.Options
    , appView : Maybe Press.Model.ViewRegions
    , assets : Maybe (List Options.Assets.AssetGroup)
    , theme : Maybe Theme.Theme
    , docs : Maybe Options.Docs.Docs
    }


main : Program Json.Decode.Value () ()
main =
    Generate.withFeedback
        (\flags ->
            case Json.Decode.decodeValue decodePlugin flags of
                Ok runs ->
                    let
                        ( appGeneratedFiles, appGeneratedErrors ) =
                            Maybe.map
                                (\pageUsages ->
                                    case Press.Generate.generate pageUsages of
                                        Ok output ->
                                            ( output, [] )

                                        Err errorList ->
                                            ( [], List.map Press.Generate.errorToDetails errorList )
                                )
                                runs.app
                                |> Maybe.withDefault ( [], [] )

                        appViewResult =
                            let
                                viewRegions =
                                    runs.appView |> Maybe.withDefault { regions = [ ( "primary", Press.Model.One ) ] }
                            in
                            [ Press.Generate.Regions.generate viewRegions ]

                        assetsResult =
                            Maybe.map
                                (\assets ->
                                    Generate.Assets.generate assets
                                )
                                runs.assets
                                |> Maybe.withDefault []

                        themeResult =
                            Maybe.map
                                (\theme ->
                                    Theme.Generate.generate theme
                                )
                                runs.theme
                                |> Maybe.withDefault []

                        docsResult =
                            Maybe.map
                                (\docs ->
                                    Generate.Docs.generate docs
                                )
                                runs.docs
                                |> Maybe.withDefault []
                    in
                    if List.isEmpty appGeneratedErrors then
                        Ok
                            { info = []
                            , files =
                                List.concat
                                    [ appGeneratedFiles
                                    , appViewResult
                                    , assetsResult
                                    , themeResult
                                    , docsResult
                                    ]
                            }

                    else
                        Err appGeneratedErrors

                Err errors ->
                    Err
                        [ { title = "Error decoding flags"
                          , description = Json.Decode.errorToString errors
                          }
                        ]
        )


decodePlugin : Json.Decode.Decoder Runs
decodePlugin =
    Json.Decode.map5 Runs
        (Json.Decode.nullable (Json.Decode.field "app" Options.App.decode))
        (Json.Decode.map Just (Json.Decode.field "app-view" Press.Model.decodeViewRegions))
        (Json.Decode.field "assets" (Json.Decode.nullable (Json.Decode.list Options.Assets.decodeAssetGroup)))
        (Json.Decode.field "theme" (Json.Decode.nullable Theme.Decoder.decode))
        (Json.Decode.field "docs" (Json.Decode.nullable Options.Docs.decoder))
