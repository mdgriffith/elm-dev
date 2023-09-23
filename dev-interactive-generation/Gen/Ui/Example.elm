module Gen.Ui.Example exposing (call_, example, includeIf, moduleName_, values_, view)

{-| 
@docs moduleName_, view, includeIf, example, call_, values_
-}


import Elm
import Elm.Annotation as Type


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Ui", "Example" ]


{-| view: Example -> Element.Element msg -}
view : Elm.Expression -> Elm.Expression
view viewArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Ui", "Example" ]
            , name = "view"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Example" [] ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ viewArg ]


{-| includeIf: Elm.Expression -> Elm.Expression -> Example -> Example -}
includeIf : Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
includeIf includeIfArg includeIfArg0 includeIfArg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Ui", "Example" ]
            , name = "includeIf"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Elm" ] "Expression" []
                        , Type.namedWith [ "Elm" ] "Expression" []
                        , Type.namedWith [] "Example" []
                        ]
                        (Type.namedWith [] "Example" [])
                    )
            }
        )
        [ includeIfArg, includeIfArg0, includeIfArg1 ]


{-| example: Elm.Expression -> Example -}
example : Elm.Expression -> Elm.Expression
example exampleArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Ui", "Example" ]
            , name = "example"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Elm" ] "Expression" [] ]
                        (Type.namedWith [] "Example" [])
                    )
            }
        )
        [ exampleArg ]


call_ :
    { view : Elm.Expression -> Elm.Expression
    , includeIf :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , example : Elm.Expression -> Elm.Expression
    }
call_ =
    { view =
        \viewArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Ui", "Example" ]
                    , name = "view"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [] "Example" [] ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Element"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ viewArg ]
    , includeIf =
        \includeIfArg includeIfArg0 includeIfArg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Ui", "Example" ]
                    , name = "includeIf"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [ "Elm" ] "Expression" []
                                , Type.namedWith [ "Elm" ] "Expression" []
                                , Type.namedWith [] "Example" []
                                ]
                                (Type.namedWith [] "Example" [])
                            )
                    }
                )
                [ includeIfArg, includeIfArg0, includeIfArg1 ]
    , example =
        \exampleArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Ui", "Example" ]
                    , name = "example"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [ "Elm" ] "Expression" [] ]
                                (Type.namedWith [] "Example" [])
                            )
                    }
                )
                [ exampleArg ]
    }


values_ :
    { view : Elm.Expression
    , includeIf : Elm.Expression
    , example : Elm.Expression
    }
values_ =
    { view =
        Elm.value
            { importFrom = [ "Ui", "Example" ]
            , name = "view"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Example" [] ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , includeIf =
        Elm.value
            { importFrom = [ "Ui", "Example" ]
            , name = "includeIf"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Elm" ] "Expression" []
                        , Type.namedWith [ "Elm" ] "Expression" []
                        , Type.namedWith [] "Example" []
                        ]
                        (Type.namedWith [] "Example" [])
                    )
            }
    , example =
        Elm.value
            { importFrom = [ "Ui", "Example" ]
            , name = "example"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Elm" ] "Expression" [] ]
                        (Type.namedWith [] "Example" [])
                    )
            }
    }


