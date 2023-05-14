module EndToEnd exposing (suite)

import Eval
import Expect
import Test exposing (..)
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


evalTest : String -> String -> Value -> Test
evalTest name expression result =
    test name <|
        \_ ->
            Eval.eval expression
                |> Expect.equal (Ok result)


sumTest : Test
sumTest =
    Debug.todo "TODO"


fibonacciTest : Test
fibonacciTest =
    Debug.todo "TODO"


standardLibraryTest : Test
standardLibraryTest =
    Debug.todo "TODO"
