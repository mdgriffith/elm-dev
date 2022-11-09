module Test.Basic exposing (..)

import Html
import Test.Module exposing (InvalidReason(..), Reason(..))


type Test
    = UnitTest (() -> List Expectation)
      -- | FuzzTest (Random.Seed -> Int -> List Expectation)
    | Labeled String Test
    | Skipped Test
    | Only Test
    | Batch (List Test)


type Expectation
    = Pass
    | Fail { given : Maybe String, description : String, reason : Reason }


reasonToString : Reason -> String
reasonToString reason =
    case reason of
        Custom ->
            "custom"

        Equality a b ->
            a ++ "=" ++ b

        _ ->
            "Not implemented"


test desc bool =
    if bool then
        Ok ("✅ " ++ Test.Module.myOtherFunction desc)

    else
        Err BadDescription



--"❌ " ++ desc


equals v1 v2 =
    v1 == v2


addOne x =
    x + 1


suite =
    test "addOne adds one" <|
        equals (addOne 123) 124


view user age friends =
    let
        { first, last } =
            user

        friendsView =
            Html.ul []
                << List.map
                    (\( friendFirst, friendLast ) ->
                        Html.li []
                            [ Html.text friendFirst
                            , Html.text friendLast
                            ]
                    )
    in
    Html.div []
        [ Html.h1 [] [ Html.text (first ++ " " ++ last) ]
        , Html.p [] [ Html.text (String.fromInt age) ]
        , friendsView friends
        ]
