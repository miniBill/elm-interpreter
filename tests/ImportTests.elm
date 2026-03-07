module ImportTests exposing (suite)

import Elm.Syntax.Expression as Expression
import Eval.Module
import Expect
import Test exposing (Test, describe, test)
import TestUtils exposing (list)
import Types exposing (Value(..))


suite : Test
suite =
    describe "Import tests"
        [ importAliasTests
        , exposingFunctionTests
        , exposingAllTests
        , defaultImportTests
        , combinedTests
        , shadowingTests
        , multiModuleTests
        , userCustomTypeTests
        ]


evalTestModule : String -> String -> (a -> Value) -> a -> Test
evalTestModule name expression toValue a =
    test name <|
        \_ ->
            Eval.Module.eval expression (Expression.FunctionOrValue [] "main")
                |> Expect.equal (Ok (toValue a))


importAliasTests : Test
importAliasTests =
    describe "Import alias"
        [ evalTestModule "Import alias with L.map"
            """module Test exposing (main)

import List as L

main =
    L.map (\\x -> x + 1) [1, 2, 3]
"""
            (list Int)
            [ 2, 3, 4 ]
        , evalTestModule "Import alias with L.filter"
            """module Test exposing (main)

import List as L

main =
    L.filter (\\x -> x > 1) [1, 2, 3]
"""
            (list Int)
            [ 2, 3 ]
        ]


exposingFunctionTests : Test
exposingFunctionTests =
    describe "Exposing specific function"
        [ evalTestModule "Import exposing map"
            """module Test exposing (main)

import List exposing (map)

main =
    map (\\x -> x + 1) [1, 2, 3]
"""
            (list Int)
            [ 2, 3, 4 ]
        , evalTestModule "Import exposing multiple functions"
            """module Test exposing (main)

import List exposing (map, filter)

main =
    filter (\\x -> x > 2) (map (\\x -> x + 1) [1, 2, 3])
"""
            (list Int)
            [ 3, 4 ]
        ]


exposingAllTests : Test
exposingAllTests =
    describe "Exposing all"
        [ evalTestModule "Import exposing all from List"
            """module Test exposing (main)

import List exposing (..)

main =
    map (\\x -> x + 1) (filter (\\x -> x > 1) [1, 2, 3])
"""
            (list Int)
            [ 3, 4 ]
        ]


defaultImportTests : Test
defaultImportTests =
    describe "Default imports"
        [ evalTestModule "identity (from Basics exposing (..))"
            """module Test exposing (main)
main = identity 42
"""
            Int
            42
        , evalTestModule "List.map without explicit import"
            """module Test exposing (main)
main = List.map (\\x -> x + 1) [1, 2, 3]
"""
            (list Int)
            [ 2, 3, 4 ]
        , evalTestModule "Just/Nothing without explicit import"
            """module Test exposing (main)
main = case Just 1 of
    Nothing -> 0
    Just x -> x
"""
            Int
            1
        , evalTestModule "LT/EQ/GT without explicit import"
            """module Test exposing (main)
main = case compare 1 2 of
    LT -> 1
    EQ -> 2
    GT -> 3
"""
            Int
            1
        , evalTestModule "Ok/Err without explicit import"
            """module Test exposing (main)
main = case Ok 42 of
    Err _ -> 0
    Ok x -> x
"""
            Int
            42
        , evalTestModule "String.length without explicit import"
            """module Test exposing (main)
main = String.length "hello"
"""
            Int
            5
        ]


combinedTests : Test
combinedTests =
    describe "Alias and exposing combined"
        [ evalTestModule "Alias and exposing together"
            """module Test exposing (main)

import List as L exposing (map)

main =
    L.length (map (\\x -> x + 1) [1, 2, 3])
"""
            Int
            3
        , evalTestModule "Original module name still works alongside alias"
            """module Test exposing (main)

import List as L

main =
    List.map (\\x -> x + 1) (L.filter (\\x -> x > 1) [1, 2, 3])
"""
            (list Int)
            [ 3, 4 ]
        ]


shadowingTests : Test
shadowingTests =
    describe "Import shadowing"
        [ evalTestModule "Later import shadows earlier for exposed names"
            """module Test exposing (main)

import List exposing (map)

main =
    map (\\x -> x + 1) [1, 2, 3]
"""
            (list Int)
            [ 2, 3, 4 ]
        ]


multiModuleTests : Test
multiModuleTests =
    describe "Multi-module evaluation"
        [ test "Import function from user module" <|
            \_ ->
                Eval.Module.evalProject
                    [ """module Helpers exposing (double)

double x = x * 2
"""
                    , """module Main exposing (main)

import Helpers exposing (double)

main = double 21
"""
                    ]
                    (Expression.FunctionOrValue [] "main")
                    |> Expect.equal (Ok (Int 42))
        , test "Qualified access to user module function" <|
            \_ ->
                Eval.Module.evalProject
                    [ """module Helpers exposing (double)

double x = x * 2
"""
                    , """module Main exposing (main)

import Helpers

main = Helpers.double 21
"""
                    ]
                    (Expression.FunctionOrValue [] "main")
                    |> Expect.equal (Ok (Int 42))
        , test "User module with alias" <|
            \_ ->
                Eval.Module.evalProject
                    [ """module Helpers exposing (double)

double x = x * 2
"""
                    , """module Main exposing (main)

import Helpers as H

main = H.double 21
"""
                    ]
                    (Expression.FunctionOrValue [] "main")
                    |> Expect.equal (Ok (Int 42))
        , test "Three modules chained" <|
            \_ ->
                Eval.Module.evalProject
                    [ """module Math exposing (square)

square x = x * x
"""
                    , """module Helpers exposing (squareAndDouble)

import Math exposing (square)

squareAndDouble x = square x * 2
"""
                    , """module Main exposing (main)

import Helpers exposing (squareAndDouble)

main = squareAndDouble 3
"""
                    ]
                    (Expression.FunctionOrValue [] "main")
                    |> Expect.equal (Ok (Int 18))
        ]


userCustomTypeTests : Test
userCustomTypeTests =
    describe "User custom type constructors"
        [ test "Zero-arg constructors" <|
            \_ ->
                Eval.Module.evalProject
                    [ """module Types exposing (Color(..))

type Color = Red | Green | Blue
"""
                    , """module Main exposing (main)

import Types exposing (Color(..))

main =
    case Red of
        Red -> 1
        Green -> 2
        Blue -> 3
"""
                    ]
                    (Expression.FunctionOrValue [] "main")
                    |> Expect.equal (Ok (Int 1))
        , test "Constructor with arguments" <|
            \_ ->
                Eval.Module.evalProject
                    [ """module Types exposing (Shape(..))

type Shape = Circle Float | Rect Float Float
"""
                    , """module Main exposing (main)

import Types exposing (Shape(..))

main =
    case Circle 5.0 of
        Circle r -> r
        Rect _ _ -> 0
"""
                    ]
                    (Expression.FunctionOrValue [] "main")
                    |> Expect.equal (Ok (Float 5.0))
        , test "Constructor with two arguments" <|
            \_ ->
                Eval.Module.evalProject
                    [ """module Types exposing (Shape(..))

type Shape = Circle Float | Rect Float Float
"""
                    , """module Main exposing (main)

import Types exposing (Shape(..))

main =
    case Rect 3.0 4.0 of
        Circle _ -> 0
        Rect w h -> w + h
"""
                    ]
                    (Expression.FunctionOrValue [] "main")
                    |> Expect.equal (Ok (Float 7.0))
        , test "Qualified constructor access" <|
            \_ ->
                Eval.Module.evalProject
                    [ """module Types exposing (Color(..))

type Color = Red | Green | Blue
"""
                    , """module Main exposing (main)

import Types

main =
    case Types.Red of
        Types.Red -> 1
        Types.Green -> 2
        Types.Blue -> 3
"""
                    ]
                    (Expression.FunctionOrValue [] "main")
                    |> Expect.equal (Ok (Int 1))
        , test "Custom type in single module" <|
            \_ ->
                Eval.Module.eval
                    """module Test exposing (main)

type Color = Red | Green | Blue

main =
    case Green of
        Red -> 1
        Green -> 2
        Blue -> 3
"""
                    (Expression.FunctionOrValue [] "main")
                    |> Expect.equal (Ok (Int 2))
        , test "Single module constructor with args" <|
            \_ ->
                Eval.Module.eval
                    """module Test exposing (main)

type Shape = Circle Float | Rect Float Float

main =
    case Circle 5.0 of
        Circle r -> r
        Rect _ _ -> 0
"""
                    (Expression.FunctionOrValue [] "main")
                    |> Expect.equal (Ok (Float 5.0))
        ]
