module Example.CallStack exposing
    ( CallStack(..)
    , find
    , getResultType
    , name
    , start
    )

{-| -}

import Elm
import Elm.Docs
import Elm.Type
import Example.Type
import Interactive


getResultType : CallStack -> Elm.Type.Type
getResultType (CallStack call) =
    call.result


type CallStack
    = CallStack
        { start : Elm.Docs.Value
        , steps : List { required : Bool, step : CallStack }
        , result : Elm.Type.Type
        }


name : CallStack -> String
name (CallStack call) =
    call.start.name


start : CallStack -> Elm.Docs.Value
start (CallStack call) =
    call.start


singleCall : Elm.Docs.Value -> CallStack
singleCall val =
    CallStack
        { start = val
        , steps = []
        , result = Example.Type.getResultType val.tipe
        }


{-| We have a start and we want to get to end.

If there are any builders for the result of `start`, we want to include those as optional steps.

There may be multiple paths through the code

-}
find :
    List Elm.Docs.Value
    -> List Elm.Docs.Value
    ->
        { start : Elm.Docs.Value
        , end : Elm.Type.Type -> Bool
        }
    -> Maybe (List CallStack)
find inScope built targeting =
    let
        resultType =
            Example.Type.getResultType targeting.start.tipe
    in
    if targeting.end resultType then
        Just
            [ CallStack
                { start = targeting.start
                , steps =
                    List.filterMap
                        (\val ->
                            if Example.Type.isBuilderOf resultType val.tipe then
                                Just
                                    { required = False
                                    , step =
                                        singleCall val
                                    }

                            else
                                Nothing
                        )
                        inScope
                , result = resultType
                }
            ]

    else
        List.foldl
            (\val gathered ->
                -- is val buildable?
                -- is not a builder itself
                if matchesResultType val targeting.start then
                    -- This value does not build a new thing
                    gathered
                    -- Have we already built one of these?

                else if List.any (matchesResultType val) built then
                    gathered

                else
                    -- We can now make val.result, but we want targeting.end
                    let
                        maybeSubCallStacks =
                            find inScope
                                (targeting.start :: built)
                                { start = val
                                , end = targeting.end
                                }
                    in
                    case maybeSubCallStacks of
                        Nothing ->
                            gathered

                        Just subCallStacks ->
                            List.map
                                (\((CallStack subCallDetails) as subCall) ->
                                    let
                                        -- We just found a call that takes us from targeting.start to subCallDetails.start
                                        -- Let's add any options steps for targeting.start
                                        optionalBuilders =
                                            List.filterMap
                                                (\optionalVal ->
                                                    if optionalVal.tipe |> Example.Type.isBuilderOf resultType then
                                                        Just
                                                            { required = False
                                                            , step =
                                                                singleCall optionalVal
                                                            }

                                                    else
                                                        Nothing
                                                )
                                                inScope
                                    in
                                    CallStack
                                        { start = targeting.start
                                        , steps =
                                            { required = True, step = singleCall subCallDetails.start }
                                                :: optionalBuilders
                                                ++ subCallDetails.steps
                                        , result = subCallDetails.result
                                        }
                                )
                                subCallStacks
                                |> Just
                                |> mergeCallStacks gathered
            )
            Nothing
            inScope


matchesResultType : Elm.Docs.Value -> Elm.Docs.Value -> Bool
matchesResultType one two =
    Example.Type.matches (Example.Type.getResultType one.tipe) (Example.Type.getResultType two.tipe)


mergeCallStacks : Maybe (List a) -> Maybe (List a) -> Maybe (List a)
mergeCallStacks maybeOne maybeTwo =
    case ( maybeOne, maybeTwo ) of
        ( Just one, Just two ) ->
            Just (one ++ two)

        ( Just one, Nothing ) ->
            Just one

        ( Nothing, Just two ) ->
            Just two

        ( Nothing, Nothing ) ->
            Nothing
