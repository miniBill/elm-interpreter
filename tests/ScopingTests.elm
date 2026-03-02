module ScopingTests exposing (suite)

import Elm.Syntax.Expression as Expression
import Eval.Module
import Expect
import Test exposing (Test, describe, test)
import TestUtils exposing (list)
import Types exposing (Value(..))


suite : Test
suite =
    describe "Variable scoping across module calls"
        [ recordAliasConstructorTests
        , describe "caller's local variables should not leak into callee"
            [ evalProjectTest "local var does not shadow callee's module-level function"
                [ """module Foo exposing (greet, helper)

helper name =
    "Hello " ++ name

greet name =
    helper name
"""
                , """module Main exposing (main)

import Foo

main =
    let
        helper = 42
    in
    Foo.greet "world"
"""
                ]
                String
                "Hello world"
            , evalProjectTest "local var does not shadow callee's unqualified reference"
                [ """module Mathlib exposing (double, addOne)

addOne x =
    x + 1

double x =
    addOne x + addOne x
"""
                , """module Main exposing (main)

import Mathlib

main =
    let
        addOne = 999
    in
    Mathlib.double 5
"""
                ]
                Int
                12
            , evalProjectTest "deeply nested: caller's range doesn't shadow List-like range"
                [ """module MyList exposing (indexedMap, range)

range lo hi =
    if lo > hi then
        []
    else
        lo :: range (lo + 1) hi

indexedMap f xs =
    map2 f (range 0 (length xs - 1)) xs

length xs =
    case xs of
        [] -> 0
        _ :: rest -> 1 + length rest

map2 f as_ bs =
    case (as_, bs) of
        (a :: restA, b :: restB) ->
            f a b :: map2 f restA restB
        _ ->
            []
"""
                , """module Main exposing (main)

import MyList

main =
    let
        range = 1114112
    in
    MyList.indexedMap (\\i x -> i + x) [10, 20, 30]
"""
                ]
                (list Int)
                [ 10, 21, 32 ]
            ]
        ]


recordAliasConstructorTests : Test
recordAliasConstructorTests =
    describe "Record type alias constructors"
        [ test "Record alias constructor field access .x" <| \_ ->
        Eval.Module.eval
            """module Test exposing (main)

type alias Point = { x : Int, y : Int }

main = (Point 1 2).x
"""
                    (Expression.FunctionOrValue [] "main")
                    |> Expect.equal (Ok (Int 1))
        , test "Record alias constructor field access .y" <| \_ ->
        Eval.Module.eval
            """module Test exposing (main)

type alias Point = { x : Int, y : Int }

main = (Point 3 4).y
"""
                    (Expression.FunctionOrValue [] "main")
                    |> Expect.equal (Ok (Int 4))
        , test "Record alias constructor used as function" <| \_ ->
        Eval.Module.eval
            """module Test exposing (main)

type alias Point = { x : Int, y : Int }

main = List.map .x [Point 10 20, Point 30 40]
"""
                    (Expression.FunctionOrValue [] "main")
                    |> Expect.equal (Ok (List [ Int 10, Int 30 ]))
        ]


evalProjectTest : String -> List String -> (a -> Value) -> a -> Test
evalProjectTest name sources toValue a =
    test name <| \_ ->
    case Eval.Module.evalProject sources (Expression.FunctionOrValue [] "main") of
        Err e ->
            Expect.fail (Debug.toString e)

        Ok value ->
            Expect.equal (toValue a) value
