module Generate exposing (main)

{-| -}

import Elm
import Elm.Annotation as Type
import Gen.CodeGen.Generate as Generate
import Gen.Platform.Cmd
import Gen.Platform.Sub


main : Program {} () ()
main =
    Generate.run
        [ file
        ]


file : Elm.File
file =
    Elm.file [ "Live" ]
        [ Elm.declaration "main"
            (Elm.apply
                (Elm.value
                    { importFrom = [ "Browser" ]
                    , name = "element"
                    , annotation =
                        Nothing
                    }
                )
                [ Elm.record
                    [ ( "init"
                      , Elm.fn ( "flags", Nothing ) (\flags -> Elm.tuple Elm.unit Gen.Platform.Cmd.none)
                      )
                    , ( "update"
                      , Elm.fn2 ( "msg", Nothing )
                            ( "model", Nothing )
                            (\msg model ->
                                Elm.tuple model Gen.Platform.Cmd.none
                            )
                      )
                    , ( "view"
                      , Elm.fn ( "model", Nothing )
                            (\model ->
                                Elm.apply
                                    (Elm.value
                                        { importFrom = [ "Html" ]
                                        , name = "text"
                                        , annotation = Nothing
                                        }
                                    )
                                    [ Elm.string "World!"
                                    ]
                            )
                      )
                    , ( "subscriptions"
                      , Elm.fn ( "model", Nothing )
                            (\model ->
                                Gen.Platform.Sub.none
                            )
                      )
                    ]
                ]
                |> Elm.withType (Type.namedWith [] "Program" [ Type.record [], Type.unit, Type.unit ])
            )
        ]
