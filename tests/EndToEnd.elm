module EndToEnd exposing (suite)

import Elm.Syntax.Expression as Expression
import Eval
import Expect
import Test exposing (Test, describe, test)
import Value exposing (Value(..))


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
        ]


helloWorldTest : Test
helloWorldTest =
    evalTest "Hello world"
        "\"Hello, World\""
        (String "Hello, World")


sumTest : Test
sumTest =
    evalTest "2 + 3"
        "2 + 3"
        (Int 5)


fibonacciTest : Test
fibonacciTest =
    evalTest "Fibonacci"
        "let fib n = if n <= 2 then 1 else fib (n - 1) + fib (n - 2) in fib 7"
        (Int 13)


recordTest : Test
recordTest =
    evalTest "Record"
        "{ a = 13, b = 'c'}.b"
        (Char 'c')


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
        (Int 0)


standardLibraryTest : Test
standardLibraryTest =
    evalTest "Stdlib"
        "List.isEmpty [()]"
        (Bool False)


tailCallTest : Test
tailCallTest =
    evalTest "Tail Call"
        "let boom x = if x <= 0 then False else boom (x - 1) in boom 100000"
        (Bool False)


closureTest : Test
closureTest =
    describe "Closures"
        [ evalTest "Simple"
            "let a = 3 in let closed x = a + x in closed 2"
            (Int 5)
        , Test.skip <| evalTest "Recursive" """let
    closure =
        let
            odd x =
                x < 0 || even (x - 1)
            even x =
                x <= 0 || odd (x - 1)
        in
        odd
in
closure 3""" (Bool True)
        ]


tooMuchApplyTest : Test
tooMuchApplyTest =
    evalTest "Too much apply"
        "(\\a -> Foo a) 0 1 2"
        (Custom { moduleName = [ "Main" ], name = "Foo" } [ Int 0, Int 1, Int 2 ])


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
            (Int 13)
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
fib1 7""" (Int 13)
        , evalTest "[let] Constant using a function" """let
    a = foo 0
    foo x = x
in
a
""" (Int 0)
        , evalTest "[let] Constant using a constant before it" """let
    a = 0
    b = a
in
b
""" (Int 0)
        , evalTest "[let] Constant using a constant after it" """let
    a = b
    b = 0
in
b
""" (Int 0)
        ]


tuplesTest : Test
tuplesTest =
    evalTest "Tuples"
        """let (a, b) = (2, 3) in let (c, d, e) = (4, 5, 6) in a + b + c + d + e"""
        (Int 20)


negationTest : Test
negationTest =
    evalTest_ "-2" (Int -2)


evalTest_ : String -> Value -> Test
evalTest_ expr value =
    evalTest expr expr value


kernelTest : Test
kernelTest =
    describe "Kernel"
        [ evalTest_ "String.length \"a\"" (Int 1)
        , evalTest_ "Basics.e" (Float e)
        ]


joinTest : Test
joinTest =
    let
        list : Value
        list =
            List
                [ String "0"
                , String "1"
                , String "2"
                ]
    in
    describe "String.join"
        [ evalTest_ """["0","1","2"]"""
            list
        , evalTest_ """String.join "." ["0","1","2"]""" (String "0.1.2")
        ]


modulesTest : Test
modulesTest =
    evalTest_ "List.sum [ 1, 2, 3 ]" (Int 6)


evalTest : String -> String -> Value -> Test
evalTest name expression result =
    test name <|
        \_ ->
            Eval.eval expression
                |> Expect.equal (Ok result)


evalTestModule : String -> String -> Value -> Test
evalTestModule name expression result =
    test name <|
        \_ ->
            Eval.evalModule expression (Expression.FunctionOrValue [] "main")
                |> Expect.equal (Ok result)
