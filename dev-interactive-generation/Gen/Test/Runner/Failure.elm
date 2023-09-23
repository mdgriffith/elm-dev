module Gen.Test.Runner.Failure exposing (annotation_, call_, caseOf_, format, make_, moduleName_, values_)

{-| 
@docs values_, call_, caseOf_, make_, annotation_, format, moduleName_
-}


import Elm
import Elm.Annotation as Type
import Elm.Case


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Test", "Runner", "Failure" ]


{-| DEPRECATED. In the future, test runners should implement versions of this
that make sense for their own environments.

Format test run failures in a reasonable way.

format: String -> Test.Runner.Failure.Reason -> String
-}
format : String -> Elm.Expression -> Elm.Expression
format formatArg formatArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Test", "Runner", "Failure" ]
            , name = "format"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.namedWith
                            [ "Test", "Runner", "Failure" ]
                            "Reason"
                            []
                        ]
                        Type.string
                    )
            }
        )
        [ Elm.string formatArg, formatArg0 ]


annotation_ : { reason : Type.Annotation, invalidReason : Type.Annotation }
annotation_ =
    { reason = Type.namedWith [ "Test", "Runner", "Failure" ] "Reason" []
    , invalidReason =
        Type.namedWith [ "Test", "Runner", "Failure" ] "InvalidReason" []
    }


make_ :
    { custom : Elm.Expression
    , equality : Elm.Expression -> Elm.Expression -> Elm.Expression
    , comparison : Elm.Expression -> Elm.Expression -> Elm.Expression
    , listDiff : Elm.Expression -> Elm.Expression -> Elm.Expression
    , collectionDiff : Elm.Expression -> Elm.Expression
    , tODO : Elm.Expression
    , invalid : Elm.Expression -> Elm.Expression
    , emptyList : Elm.Expression
    , nonpositiveFuzzCount : Elm.Expression
    , invalidFuzzer : Elm.Expression
    , badDescription : Elm.Expression
    , duplicatedName : Elm.Expression
    }
make_ =
    { custom =
        Elm.value
            { importFrom = [ "Test", "Runner", "Failure" ]
            , name = "Custom"
            , annotation = Just (Type.namedWith [] "Reason" [])
            }
    , equality =
        \ar0 ar1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Test", "Runner", "Failure" ]
                    , name = "Equality"
                    , annotation = Just (Type.namedWith [] "Reason" [])
                    }
                )
                [ ar0, ar1 ]
    , comparison =
        \ar0 ar1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Test", "Runner", "Failure" ]
                    , name = "Comparison"
                    , annotation = Just (Type.namedWith [] "Reason" [])
                    }
                )
                [ ar0, ar1 ]
    , listDiff =
        \ar0 ar1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Test", "Runner", "Failure" ]
                    , name = "ListDiff"
                    , annotation = Just (Type.namedWith [] "Reason" [])
                    }
                )
                [ ar0, ar1 ]
    , collectionDiff =
        \ar0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Test", "Runner", "Failure" ]
                    , name = "CollectionDiff"
                    , annotation = Just (Type.namedWith [] "Reason" [])
                    }
                )
                [ ar0 ]
    , tODO =
        Elm.value
            { importFrom = [ "Test", "Runner", "Failure" ]
            , name = "TODO"
            , annotation = Just (Type.namedWith [] "Reason" [])
            }
    , invalid =
        \ar0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Test", "Runner", "Failure" ]
                    , name = "Invalid"
                    , annotation = Just (Type.namedWith [] "Reason" [])
                    }
                )
                [ ar0 ]
    , emptyList =
        Elm.value
            { importFrom = [ "Test", "Runner", "Failure" ]
            , name = "EmptyList"
            , annotation = Just (Type.namedWith [] "InvalidReason" [])
            }
    , nonpositiveFuzzCount =
        Elm.value
            { importFrom = [ "Test", "Runner", "Failure" ]
            , name = "NonpositiveFuzzCount"
            , annotation = Just (Type.namedWith [] "InvalidReason" [])
            }
    , invalidFuzzer =
        Elm.value
            { importFrom = [ "Test", "Runner", "Failure" ]
            , name = "InvalidFuzzer"
            , annotation = Just (Type.namedWith [] "InvalidReason" [])
            }
    , badDescription =
        Elm.value
            { importFrom = [ "Test", "Runner", "Failure" ]
            , name = "BadDescription"
            , annotation = Just (Type.namedWith [] "InvalidReason" [])
            }
    , duplicatedName =
        Elm.value
            { importFrom = [ "Test", "Runner", "Failure" ]
            , name = "DuplicatedName"
            , annotation = Just (Type.namedWith [] "InvalidReason" [])
            }
    }


caseOf_ :
    { reason :
        Elm.Expression
        -> { reasonTags_0_0
            | custom : Elm.Expression
            , equality : Elm.Expression -> Elm.Expression -> Elm.Expression
            , comparison : Elm.Expression -> Elm.Expression -> Elm.Expression
            , listDiff : Elm.Expression -> Elm.Expression -> Elm.Expression
            , collectionDiff : Elm.Expression -> Elm.Expression
            , tODO : Elm.Expression
            , invalid : Elm.Expression -> Elm.Expression
        }
        -> Elm.Expression
    , invalidReason :
        Elm.Expression
        -> { invalidReasonTags_1_0
            | emptyList : Elm.Expression
            , nonpositiveFuzzCount : Elm.Expression
            , invalidFuzzer : Elm.Expression
            , badDescription : Elm.Expression
            , duplicatedName : Elm.Expression
        }
        -> Elm.Expression
    }
caseOf_ =
    { reason =
        \reasonExpression reasonTags ->
            Elm.Case.custom
                reasonExpression
                (Type.namedWith [ "Test", "Runner", "Failure" ] "Reason" [])
                [ Elm.Case.branch0 "Custom" reasonTags.custom
                , Elm.Case.branch2
                    "Equality"
                    ( "string.String", Type.string )
                    ( "string.String", Type.string )
                    reasonTags.equality
                , Elm.Case.branch2
                    "Comparison"
                    ( "string.String", Type.string )
                    ( "string.String", Type.string )
                    reasonTags.comparison
                , Elm.Case.branch2
                    "ListDiff"
                    ( "list.List", Type.list Type.string )
                    ( "list.List", Type.list Type.string )
                    reasonTags.listDiff
                , Elm.Case.branch1
                    "CollectionDiff"
                    ( "one"
                    , Type.record
                        [ ( "expected", Type.string )
                        , ( "actual", Type.string )
                        , ( "extra", Type.list Type.string )
                        , ( "missing", Type.list Type.string )
                        ]
                    )
                    reasonTags.collectionDiff
                , Elm.Case.branch0 "TODO" reasonTags.tODO
                , Elm.Case.branch1
                    "Invalid"
                    ( "test.Runner.Failure.InvalidReason"
                    , Type.namedWith
                        [ "Test", "Runner", "Failure" ]
                        "InvalidReason"
                        []
                    )
                    reasonTags.invalid
                ]
    , invalidReason =
        \invalidReasonExpression invalidReasonTags ->
            Elm.Case.custom
                invalidReasonExpression
                (Type.namedWith
                    [ "Test", "Runner", "Failure" ]
                    "InvalidReason"
                    []
                )
                [ Elm.Case.branch0 "EmptyList" invalidReasonTags.emptyList
                , Elm.Case.branch0
                    "NonpositiveFuzzCount"
                    invalidReasonTags.nonpositiveFuzzCount
                , Elm.Case.branch0
                    "InvalidFuzzer"
                    invalidReasonTags.invalidFuzzer
                , Elm.Case.branch0
                    "BadDescription"
                    invalidReasonTags.badDescription
                , Elm.Case.branch0
                    "DuplicatedName"
                    invalidReasonTags.duplicatedName
                ]
    }


call_ : { format : Elm.Expression -> Elm.Expression -> Elm.Expression }
call_ =
    { format =
        \formatArg formatArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Test", "Runner", "Failure" ]
                    , name = "format"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string
                                , Type.namedWith
                                    [ "Test", "Runner", "Failure" ]
                                    "Reason"
                                    []
                                ]
                                Type.string
                            )
                    }
                )
                [ formatArg, formatArg0 ]
    }


values_ : { format : Elm.Expression }
values_ =
    { format =
        Elm.value
            { importFrom = [ "Test", "Runner", "Failure" ]
            , name = "format"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.namedWith
                            [ "Test", "Runner", "Failure" ]
                            "Reason"
                            []
                        ]
                        Type.string
                    )
            }
    }


