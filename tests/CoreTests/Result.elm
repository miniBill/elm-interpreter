module CoreTests.Result exposing (suite)

import Test exposing (Test, describe)
import TestUtils exposing (evalTest, result)
import Types exposing (Value(..))


isEven : Int -> Result String Int
isEven n =
    if modBy 2 n == 0 then
        Ok n

    else
        Err "number is odd"


withIsEven : String -> String
withIsEven source =
    "let isEven n = if modBy 2 n == 0 then Ok n else Err \"number is odd\" in " ++ source


toIntResult : String -> Result String Int
toIntResult s =
    case String.toInt s of
        Just i ->
            Ok i

        Nothing ->
            Err <| "could not convert string '" ++ s ++ "' to an Int"


withToIntResult : String -> String
withToIntResult source =
    """
let
    toIntResult s =
        case String.toInt s of
            Just i ->
                Ok i

            Nothing ->
                Err ("could not convert string '" ++ s ++ "' to an Int")
in """ ++ source


add3 : number -> number -> number -> number
add3 a b c =
    a + b + c


add4 : number -> number -> number -> number -> number
add4 a b c d =
    a + b + c + d


add5 : number -> number -> number -> number -> number -> number
add5 a b c d e =
    a + b + c + d + e


suite : Test
suite =
    describe "Result Tests"
        [ mapTests
        , mapNTests
        , andThenTests
        ]


mapTests : Test
mapTests =
    describe "map Tests"
        [ evalTest "map Ok"
            """Result.map ((+) 1) (Ok 2)"""
            (result String Int)
            (Result.map ((+) 1) (Ok 2))
        , evalTest "map Err"
            """Result.map ((+) 1) (Err "error")"""
            (result String Int)
            (Result.map ((+) 1) (Err "error"))
        ]


mapNTests : Test
mapNTests =
    describe "mapN Tests"
        [ evalTest "map2 Ok"
            """Result.map2 (+) (Ok 1) (Ok 2)"""
            (result String Int)
            (Result.map2 (+) (Ok 1) (Ok 2))
        , evalTest "map2 Err"
            """Result.map2 (+) (Ok 1) (Err "x")"""
            (result String Int)
            (Result.map2 (+) (Ok 1) (Err "x"))
        , evalTest "map3 Ok"
            """let add3 a b c = a + b + c in Result.map3 add3 (Ok 1) (Ok 2) (Ok 3)"""
            (result String Int)
            (Result.map3 add3 (Ok 1) (Ok 2) (Ok 3))
        , evalTest "map3 Err"
            """let add3 a b c = a + b + c in Result.map3 add3 (Ok 1) (Ok 2) (Err "x")"""
            (result String Int)
            (Result.map3 add3 (Ok 1) (Ok 2) (Err "x"))
        , evalTest "map4 Ok"
            """let add4 a b c d = a + b + c + d in Result.map4 add4 (Ok 1) (Ok 2) (Ok 3) (Ok 4)"""
            (result String Int)
            (Result.map4 add4 (Ok 1) (Ok 2) (Ok 3) (Ok 4))
        , evalTest "map4 Err"
            """let add4 a b c d = a + b + c + d in Result.map4 add4 (Ok 1) (Ok 2) (Ok 3) (Err "x")"""
            (result String Int)
            (Result.map4 add4 (Ok 1) (Ok 2) (Ok 3) (Err "x"))
        , evalTest "map5 Ok"
            """let add5 a b c d e = a + b + c + d + e in Result.map5 add5 (Ok 1) (Ok 2) (Ok 3) (Ok 4) (Ok 5)"""
            (result String Int)
            (Result.map5 add5 (Ok 1) (Ok 2) (Ok 3) (Ok 4) (Ok 5))
        , evalTest "map5 Err"
            """let add5 a b c d e = a + b + c + d + e in Result.map5 add5 (Ok 1) (Ok 2) (Ok 3) (Ok 4) (Err "x")"""
            (result String Int)
            (Result.map5 add5 (Ok 1) (Ok 2) (Ok 3) (Ok 4) (Err "x"))
        ]


andThenTests : Test
andThenTests =
    describe "andThen Tests"
        [ evalTest "andThen Ok"
            (withToIntResult <| withIsEven """toIntResult "42" |> Result.andThen isEven""")
            (result String Int)
            (toIntResult "42" |> Result.andThen isEven)
        , evalTest "andThen first Err"
            (withToIntResult <| withIsEven """toIntResult "4.2" |> Result.andThen isEven""")
            (result String Int)
            (toIntResult "4.2" |> Result.andThen isEven)
        , evalTest "andThen second Err"
            (withToIntResult <| withIsEven """toIntResult "41" |> Result.andThen isEven""")
            (result String Int)
            (toIntResult "41" |> Result.andThen isEven)
        ]
