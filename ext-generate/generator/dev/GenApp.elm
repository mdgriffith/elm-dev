module GenApp exposing (element)

import Elm
import Elm.Annotation
import Elm.Arg
import Elm.Case
import Gen.Browser
import Gen.Json.Decode
import Gen.Platform.Cmd
import Gen.Platform.Sub


msgType =
    Elm.Annotation.named [] "Msg"


modelType =
    Elm.Annotation.named [] "Model"


updateTupleType =
    Elm.Annotation.tuple modelType (Gen.Platform.Cmd.annotation_.cmd msgType)


type alias MsgVariant =
    { name : String
    , data : List Elm.Annotation.Annotation
    , updateBranch : Elm.Expression -> Elm.Case.Branch
    }


element :
    List String
    ->
        { model : Elm.Annotation.Annotation
        , messageHandlers : List MsgVariant
        , init : Elm.Expression -> Elm.Expression
        , view : Elm.Expression -> Elm.Expression
        , subscriptions : Elm.Expression -> Elm.Expression
        , declarations : List Elm.Declaration
        }
    -> Elm.File
element modName app =
    Elm.file modName
        ([ Elm.declaration "main" appMain
         , Elm.declaration "init"
            (Elm.fn (Elm.Arg.varWith "flags" Gen.Json.Decode.annotation_.value)
                (\flags ->
                    app.init flags
                        |> Elm.withType updateTupleType
                )
            )
         , Elm.alias "Model" app.model
         , Elm.customType "Msg"
            (List.map
                (\msg ->
                    Elm.variantWith msg.name msg.data
                )
                app.messageHandlers
            )
         , Elm.declaration "update"
            (Elm.fn2
                (Elm.Arg.varWith "msg" msgType)
                (Elm.Arg.varWith "model" modelType)
                (\msg model ->
                    Elm.Case.custom msg
                        msgType
                        (List.map
                            (\messageHandlers ->
                                messageHandlers.updateBranch model
                            )
                            app.messageHandlers
                        )
                        |> Elm.withType updateTupleType
                )
            )
         , Elm.declaration "view" (Elm.fn (Elm.Arg.varWith "model" modelType) app.view)
         , Elm.declaration "subscriptions"
            (Elm.fn (Elm.Arg.varWith "model" modelType) app.subscriptions)
         ]
            ++ app.declarations
        )


appMain =
    Elm.record
        [ ( "init"
          , Elm.value
                { importFrom = []
                , name = "init"
                , annotation =
                    Just
                        (Elm.Annotation.function
                            [ Gen.Json.Decode.annotation_.value
                            ]
                            updateTupleType
                        )
                }
          )
        , ( "update"
          , Elm.value
                { importFrom = []
                , name = "update"
                , annotation =
                    Just
                        (Elm.Annotation.function
                            [ msgType
                            , modelType
                            ]
                            updateTupleType
                        )
                }
          )
        , ( "view", Elm.val "view" )
        , ( "subscriptions", Elm.val "subscriptions" )
        ]
        |> Gen.Browser.call_.element

