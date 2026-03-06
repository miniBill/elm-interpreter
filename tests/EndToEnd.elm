module EndToEnd exposing (caseBoolPatternTest, suite)

import Array
import Elm.Syntax.Expression as Expression
import Eval.Module
import Expect
import FastDict as Dict
import Test exposing (Test, describe, test)
import TestUtils exposing (evalTest, evalTest_, list, slowTest)
import Types exposing (Value(..))


suite : Test
suite =
    describe "Some end to end tests"
        [ helloWorldTest
        , sumTest
        , fibonacciTest
        , recordTest
        , customTypeTest
        , standardLibraryTest
        , tailCallTest
        , closureTest
        , tooMuchApplyTest
        , mutualRecursionTest
        , tuplesTest
        , negationTest
        , kernelTest
        , joinTest
        , modulesTest
        , higherOrderTest
        , shadowingTest
        , recordUpdateTest
        , caseBoolPatternTest
        , kernelFunctionAsArgTest
        , edgeCaseTests
        ]


helloWorldTest : Test
helloWorldTest =
    evalTest_ "\"Hello, World\"" String "Hello, World"


sumTest : Test
sumTest =
    evalTest "2 + 3" "2 + 3" Int 5


fibonacciTest : Test
fibonacciTest =
    evalTest "Fibonacci"
        "let fib n = if n <= 2 then 1 else fib (n - 1) + fib (n - 2) in fib 7"
        Int
        13


recordTest : Test
recordTest =
    evalTest "Record" "{ a = 13, b = 'c'}.b" Char 'c'


customTypeTest : Test
customTypeTest =
    evalTest "Custom type"
        """let
    foo = Just []
in
case foo of
    Nothing -> -1
    Just [ x ] -> 1
    Just [] -> 0
"""
        Int
        0


standardLibraryTest : Test
standardLibraryTest =
    evalTest "Stdlib"
        "List.isEmpty [()]"
        Bool
        False


tailCallTest : Test
tailCallTest =
    slowTest <|
        \i ->
            describe "Tail call"
                [ evalTest "Inline"
                    ("let boom x = if x <= 0 then False else boom (x - 1) in boom " ++ String.fromInt i)
                    Bool
                    False
                , evalTestModule "As module"
                    ("""module TailCall exposing (boom)

boom : Int -> Bool
boom x =
    let
        a = 0
    in
    if x <= 0 then
        False
    else
        boom (x - 1)

main : Bool
main =
    boom """ ++ String.fromInt i)
                    Bool
                    False
                ]


closureTest : Test
closureTest =
    describe "Closures"
        [ evalTest "Simple"
            "let a = 3 in let closed x = a + x in closed 2"
            Int
            5
        , evalTest "Recursive" """let
    closure =
        let
            odd x =
                x < 0 || even (x - 1)
            even x =
                x <= 0 || odd (x - 1)
        in
        odd
in
closure 3""" Bool True
        ]


tooMuchApplyTest : Test
tooMuchApplyTest =
    evalTest "Too much apply"
        "(\\a -> Foo a) 0 1 2"
        identity
    <|
        Custom
            { moduleName = [ "Main" ], name = "Foo" }
            [ Int 0, Int 1, Int 2 ]


mutualRecursionTest : Test
mutualRecursionTest =
    describe "Mutual recursion"
        [ evalTestModule "At the top level"
            """module Test exposing (..)

fib1 n =
    if n <= 2 then
        1
    else
        fib2 (n - 1) + fib2 (n - 2)

fib2 n =
    if n <= 2 then
        1
    else
        fib1 (n - 1) + fib1 (n - 2)

main =
    fib1 7"""
            Int
            13
        , evalTest "Inside a let" """let
    fib1 n =
        if n <= 2 then
            1
        else
            fib2 (n - 1) + fib2 (n - 2)

    fib2 n =
        if n <= 2 then
            1
        else
            fib1 (n - 1) + fib1 (n - 2)
in
fib1 7""" Int 13
        , evalTest "[let] Constant using a function" """let
    a = foo 0
    foo x = x
in
a
""" Int 0
        , evalTest "[let] Constant using a constant before it" """let
    a = 0
    b = a
in
b
""" Int 0
        , evalTest "[let] Constant using a constant after it" """let
    a = b
    b = 0
in
b
""" Int 0
        ]


tuplesTest : Test
tuplesTest =
    evalTest "Tuples"
        """let (a, b) = (2, 3) in let (c, d, e) = (4, 5, 6) in a + b + c + d + e"""
        Int
        20


negationTest : Test
negationTest =
    evalTest_ "-2" Int -2


kernelTest : Test
kernelTest =
    describe "Kernel"
        [ evalTest_ "String.length \"a\"" Int 1
        , evalTest_ "Basics.e" Float e
        ]


joinTest : Test
joinTest =
    let
        list : List Value
        list =
            [ String "0"
            , String "1"
            , String "2"
            ]
    in
    describe "String.join"
        [ evalTest_ """["0","1","2"]""" List list
        , evalTest_ """String.join "." ["0","1","2"]""" String "0.1.2"
        ]


modulesTest : Test
modulesTest =
    evalTest_ "List.sum [ 1, 2, 3 ]" Int 6


higherOrderTest : Test
higherOrderTest =
    evalTest_ "String.map Char.toUpper \"Hello, world!\"" String <|
        String.map Char.toUpper "Hello, world!"


evalTestModule : String -> String -> (a -> Value) -> a -> Test
evalTestModule name expression toValue a =
    test name <|
        \_ ->
            Eval.Module.eval expression (Expression.FunctionOrValue [] "main")
                |> Expect.equal (Ok (toValue a))


shadowingTest : Test
shadowingTest =
    evalTestModule "shadowing in let/in" """module Temp exposing (main)

foo : a -> List a -> List a
foo nodes acc =
    let
        node =
            nodes

        newAcc =
            node :: acc
    in
    newAcc


main : List (List number)
main =
    let
        node =
            [ 0, 1 ]
    in
    foo [ 4, 5 ] [ node ]""" (list (list Int)) [ [ 4, 5 ], [ 0, 1 ] ]


recordUpdateTest : Test
recordUpdateTest =
    describe "Record update"
        [ evalTest "preserves original fields"
            """let rec = { a = 1, b = 2 } in { rec | a = 10 }"""
            Record
            (Dict.fromList [ ( "a", Int 10 ), ( "b", Int 2 ) ])
        , evalTest "updates multiple fields"
            """let rec = { x = 1, y = 2, z = 3 } in { rec | x = 10, z = 30 }"""
            Record
            (Dict.fromList [ ( "x", Int 10 ), ( "y", Int 2 ), ( "z", Int 30 ) ])
        ]


caseBoolPatternTest : Test
caseBoolPatternTest =
    describe "Case matching on Bool constructors"
        [ evalTest "True branch"
            """case True of
    True -> 1
    False -> 0"""
            Int
            1
        , evalTest "False branch"
            """case False of
    True -> 1
    False -> 0"""
            Int
            0
        , evalTest "Bool variable"
            """let x = True in case x of
    True -> "yes"
    False -> "no" """
            String
            "yes"
        ]


kernelFunctionAsArgTest : Test
kernelFunctionAsArgTest =
    describe "Kernel function as higher-order argument"
        [ evalTest "List.map String.fromInt"
            "List.map String.fromInt [1, 2, 3]"
            (list String)
            (List.map String.fromInt [ 1, 2, 3 ])
        , evalTest "List.map negate"
            "List.map negate [1, 2, 3]"
            (list Int)
            (List.map negate [ 1, 2, 3 ])
        , evalTest "List.filterMap String.toInt"
            """List.filterMap String.toInt ["1", "a", "3"]"""
            (list Int)
            (List.filterMap String.toInt [ "1", "a", "3" ])
        , evalTest "Array.map with kernel function"
            "Array.map negate (Array.fromList [1, 2, 3]) |> Array.toList"
            (list Int)
            (Array.map negate (Array.fromList [ 1, 2, 3 ]) |> Array.toList)
        , evalTest "Array.map with 0-arg kernel wrapper (Char.toUpper)"
            "Array.map Char.toUpper (Array.fromList ['a', 'b', 'c']) |> Array.toList"
            (list Char)
            [ 'A', 'B', 'C' ]
        , evalTest "String.filter with kernel predicate"
            "String.filter Char.isUpper \"Hello World\""
            String
            "HW"
        ]


edgeCaseTests : Test
edgeCaseTests =
    describe "Edge cases"
        [ -- Nested destructuring
          evalTest "nested tuple destructuring"
            "let (a, (b, c)) = (1, (2, 3)) in a + b + c"
            Int
            6
        , evalTest "let record destructuring"
            "let { x, y } = { x = 10, y = 20 } in x + y"
            Int
            30

        -- Wildcard pattern
        , evalTest "wildcard in case"
            "case Just 42 of\n    Nothing -> 0\n    _ -> 1"
            Int
            1

        -- String operations
        , evalTest "String.fromFloat"
            "String.fromFloat 3.14"
            String
            "3.14"
        , evalTest "String.fromInt"
            "String.fromInt 42"
            String
            "42"
        , evalTest "String.toInt valid"
            """String.toInt "123" """
            (TestUtils.maybe Int)
            (Just 123)
        , evalTest "String.toInt invalid"
            """String.toInt "abc" """
            (TestUtils.maybe Int)
            Nothing
        , evalTest "append strings"
            """"hello" ++ " world" """
            String
            "hello world"
        , evalTest "append lists"
            "[1, 2] ++ [3, 4]"
            (list Int)
            [ 1, 2, 3, 4 ]

        -- Composing functions
        , evalTest "function composition >>"
            "(String.fromInt >> String.length) 123"
            Int
            3
        , evalTest "function composition <<"
            "(String.length << String.fromInt) 123"
            Int
            3
        ]
