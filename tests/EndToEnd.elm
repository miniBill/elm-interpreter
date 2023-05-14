module EndToEnd exposing (suite)

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
        , standardLibraryTest
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


standardLibraryTest : Test
standardLibraryTest =
    evalTest "Stdlib"
        "List.isEmpty [()]"
        (Bool False)


evalTest : String -> String -> Value -> Test
evalTest name expression result =
    test name <|
        \_ ->
            Eval.eval expression
                |> Expect.equal (Ok result)
