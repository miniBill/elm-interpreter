module RandomTests exposing (suite)

import Bitwise
import Elm.Syntax.Expression as Expression
import Eval.Module
import Expect
import Test exposing (Test, describe, test)
import TestUtils exposing (evalTest, list)
import Types exposing (Value(..))


suite : Test
suite =
    describe "Random.int and related"
        [ describe "partial application of kernel functions"
            [ evalTest "remainderBy partial application"
                "let f = remainderBy 5 in f 12"
                Int
                (remainderBy 5 12)
            , evalTest "Bitwise.shiftRightZfBy partial application"
                "let f = Bitwise.shiftRightZfBy 0 in f (-32)"
                Int
                (Bitwise.shiftRightZfBy 0 -32)
            , evalTest "modBy partial application"
                "let f = modBy 1114112 in f 5000000"
                Int
                (modBy 1114112 5000000)
            , evalTest "modBy partial application passed to List.map"
                "List.map (modBy 1114112) [5000000, 1000000, 0]"
                (list Int)
                (List.map (modBy 1114112) [ 5000000, 1000000, 0 ])
            , evalTest "modBy with large range via let"
                "let range = 1114112 in modBy range 5000000"
                Int
                (modBy 1114112 5000000)
            , evalTestModule "modBy partial app in multi-module context"
                """module Temp exposing (main)

main =
    let
        range = 1114112
        f = modBy range
    in
    List.map f [5000000, 1000000, 0]
"""
                (list Int)
                (List.map (modBy 1114112) [ 5000000, 1000000, 0 ])
            , evalTestModule "List.indexedMap with toFloat"
                """module Temp exposing (main)

main =
    List.indexedMap (\\i w -> ( toFloat w, i + 1 )) [8, 2, 1]
"""
                (list (TestUtils.tuple Float Int))
                (List.indexedMap (\i w -> ( toFloat w, i + 1 )) [ 8, 2, 1 ])
            ]
        , describe "Random.int non-power-of-2 path components"
            [ evalTest "remainderBy with large numbers"
                "remainderBy 1114112 4293853184"
                Int
                (remainderBy 1114112 4293853184)
            , evalTest "Bitwise.shiftRightZfBy 0 of negative"
                "Bitwise.shiftRightZfBy 0 -1114112"
                Int
                (Bitwise.shiftRightZfBy 0 -1114112)
            , evalTest "chained: shiftRightZfBy 0 (remainderBy range (shiftRightZfBy 0 -range))"
                "let range = 6 in Bitwise.shiftRightZfBy 0 (remainderBy range (Bitwise.shiftRightZfBy 0 -range))"
                Int
                (let
                    range =
                        6
                 in
                 Bitwise.shiftRightZfBy 0 (remainderBy range (Bitwise.shiftRightZfBy 0 -range))
                )
            ]
        , describe "local recursive function with kernel calls"
            [ evalTestModule "recursive function using remainderBy"
                """module Temp exposing (main)

import Bitwise

main =
    let
        range = 6
        threshold = Bitwise.shiftRightZfBy 0 (remainderBy range (Bitwise.shiftRightZfBy 0 -range))

        loop x =
            if x < threshold then
                loop (x + 1)
            else
                remainderBy range x
    in
    loop 0
"""
                Int
                (let
                    range =
                        6

                    threshold =
                        Bitwise.shiftRightZfBy 0 (remainderBy range (Bitwise.shiftRightZfBy 0 -range))

                    loop x =
                        if x < threshold then
                            loop (x + 1)

                        else
                            remainderBy range x
                 in
                 loop 0
                )
            , evalTestModule "custom type destructuring with kernel function"
                """module Temp exposing (main)

type Wrapper = Wrapper Int Int

main =
    let
        wrapped = Wrapper 42 7
        unwrap (Wrapper state extra) =
            remainderBy 5 state + extra
    in
    unwrap wrapped
"""
                Int
                (remainderBy 5 42 + 7)
            , evalTestModule "local function calling partially-applied kernel fn"
                """module Temp exposing (main)

import Bitwise

main =
    let
        toUnsigned = Bitwise.shiftRightZfBy 0
        negate x = 0 - x
        range = 1114112
        result = toUnsigned (negate range)
    in
    result
"""
                Int
                (Bitwise.shiftRightZfBy 0 (0 - 1114112))
            ]
        , describe "pipe operator with kernel functions"
            [ evalTest "pipe into kernel function"
                "-32 |> Bitwise.shiftRightZfBy 0"
                Int
                (-32 |> Bitwise.shiftRightZfBy 0)
            , evalTest "pipe chain with kernel functions"
                "let range = 6 in (0 - range) |> Bitwise.shiftRightZfBy 0 |> remainderBy range |> Bitwise.shiftRightZfBy 0"
                Int
                (let
                    range =
                        6
                 in
                 (0 - range) |> Bitwise.shiftRightZfBy 0 |> remainderBy range |> Bitwise.shiftRightZfBy 0
                )
            ]
        , describe "mini Random.int (multi-module)"
            [ evalProjectTest "Random.step with non-power-of-2 range"
                [ """module MiniRandom exposing (Generator(..), Seed(..), int, step, initialSeed)

import Bitwise


type Generator a
    = Generator (Seed -> ( a, Seed ))


type Seed
    = Seed Int Int


step : Generator a -> Seed -> ( a, Seed )
step (Generator generator) seed =
    generator seed


initialSeed : Int -> Seed
initialSeed x =
    let
        (Seed state1 incr) =
            next (Seed 0 1013904223)

        state2 =
            Bitwise.shiftRightZfBy 0 (state1 + x)
    in
    next (Seed state2 incr)


next : Seed -> Seed
next (Seed state0 incr) =
    Seed (Bitwise.shiftRightZfBy 0 ((state0 * 1664525) + incr)) incr


peel : Seed -> Int
peel (Seed state _) =
    let
        word =
            (Bitwise.xor state (Bitwise.shiftRightZfBy ((Bitwise.shiftRightZfBy 28 state) + 4) state)) * 277803737
    in
    Bitwise.shiftRightZfBy 0 (Bitwise.xor (Bitwise.shiftRightZfBy 22 word) word)


int : Int -> Int -> Generator Int
int a b =
    Generator
        (\\seed0 ->
            let
                ( lo, hi ) =
                    if a < b then
                        ( a, b )
                    else
                        ( b, a )

                range =
                    hi - lo + 1
            in
            if Bitwise.and (range - 1) range == 0 then
                ( Bitwise.shiftRightZfBy 0 (Bitwise.and (range - 1) (peel seed0)) + lo, next seed0 )

            else
                let
                    threshold =
                        Bitwise.shiftRightZfBy 0 (remainderBy range (Bitwise.shiftRightZfBy 0 (0 - range)))

                    accountForBias : Seed -> ( Int, Seed )
                    accountForBias seed =
                        let
                            x =
                                peel seed

                            seedN =
                                next seed
                        in
                        if x < threshold then
                            accountForBias seedN

                        else
                            ( remainderBy range x + lo, seedN )
                in
                accountForBias seed0
        )
"""
                , """module Main exposing (main)

import MiniRandom

main =
    MiniRandom.step (MiniRandom.int 0 5) (MiniRandom.initialSeed 42)
        |> Tuple.first
"""
                ]
                Int
                0

            -- value doesn't matter; evalProjectTest just checks for Int, not crash
            ]
        ]


evalTestModule : String -> String -> (a -> Value) -> a -> Test
evalTestModule name source toValue a =
    test name <|
        \_ ->
            Eval.Module.eval source (Expression.FunctionOrValue [] "main")
                |> Expect.equal (Ok (toValue a))


evalProjectTest : String -> List String -> (a -> Value) -> a -> Test
evalProjectTest name sources toValue a =
    test name <|
        \_ ->
            case Eval.Module.evalProject sources (Expression.FunctionOrValue [] "main") of
                Err e ->
                    Expect.fail (Debug.toString e)

                Ok value ->
                    -- Just check it's an Int (not erroring is the main test)
                    case value of
                        Int _ ->
                            Expect.pass

                        _ ->
                            Expect.fail ("Expected Int, got " ++ Debug.toString value)
