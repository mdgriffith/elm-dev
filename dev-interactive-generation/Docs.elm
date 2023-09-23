module Docs exposing (button)

import Elm.Docs
import Elm.Type


value name tipe =
    { comment = ""
    , name = name
    , tipe = tipe
    }


types =
    { string = Elm.Type.Type "String.String" []
    , int = Elm.Type.Type "Basics.Int" []
    , float = Elm.Type.Type "Basics.Float" []
    , bool = Elm.Type.Type "Basics.Boolean" []
    , confabulator = Elm.Type.Type "Confab.Confabulator" []
    , list = \t -> Elm.Type.Type "List.List" [ t ]
    , record = \fields -> Elm.Type.Record fields Nothing
    }


button : Elm.Docs.Module
button =
    { name = "Ui.Button"
    , comment = ""
    , unions = []
    , aliases = []
    , values = buttonValues
    , binops = []
    }


buttonTypes =
    { button = Elm.Type.Type "Ui.Button.Button" [ Elm.Type.Var "msg" ]
    , element = Elm.Type.Type "Element.Element" [ Elm.Type.Var "msg" ]
    , msg = Elm.Type.Var "msg"
    }


buttonValues =
    [ value "init"
        (Elm.Type.Lambda
            (types.record
                [ ( "text", types.string )

                -- , ( "onClick", types.bool )
                , ( "onClick", buttonTypes.msg )
                ]
            )
            buttonTypes.button
        )
    , value "view"
        (Elm.Type.Lambda
            buttonTypes.button
            buttonTypes.element
        )
    , value "withSmall"
        (Elm.Type.Lambda
            buttonTypes.button
            buttonTypes.button
        )
    , value "withDisabled"
        (Elm.Type.Lambda
            types.bool
            (Elm.Type.Lambda
                buttonTypes.button
                buttonTypes.button
            )
        )
    ]


values =
    ---- Simple
    -- [ value "query"
    --     (Elm.Type.Lambda
    --         types.string
    --         types.int
    --     )
    -- ]
    --- Moderately more complicated
    -- [ value "query"
    --     (Elm.Type.Lambda
    --         (types.list types.string)
    --         (Elm.Type.Lambda
    --             types.string
    --             types.int
    --         )
    --     )
    -- ]
    -------- Make values
    -- [ value "query"
    --     (Elm.Type.Lambda
    --         (types.list types.string)
    --         (Elm.Type.Lambda
    --             types.confabulator
    --             types.int
    --         )
    --     )
    -- , value "makeConfabulator"
    --     (Elm.Type.Lambda
    --         (types.record
    --             [ ( "confab", types.string )
    --             , ( "discombob", types.bool )
    --             ]
    --         )
    --         types.confabulator
    --     )
    -- ]
    -------- Builder
    [ value "query"
        (Elm.Type.Lambda
            (types.list types.string)
            (Elm.Type.Lambda
                types.confabulator
                types.int
            )
        )
    , value "adjustConfab"
        (Elm.Type.Lambda
            types.bool
            (Elm.Type.Lambda
                types.confabulator
                types.confabulator
            )
        )
    , value "adjustConfabAgain"
        (Elm.Type.Lambda
            (types.list types.int)
            (Elm.Type.Lambda
                types.bool
                (Elm.Type.Lambda
                    types.confabulator
                    types.confabulator
                )
            )
        )
    , value "makeConfabulator"
        (Elm.Type.Lambda
            (types.record
                [ ( "confab", types.string )
                , ( "discombob", types.bool )
                ]
            )
            types.confabulator
        )
    ]
