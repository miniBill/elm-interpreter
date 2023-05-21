module Utils exposing (evalTest, evalTest_)

import Eval
import Expect
import Test exposing (Test, test)
import Value exposing (Value)


evalTest_ : String -> Value -> Test
evalTest_ expr value =
    evalTest expr expr value


evalTest : String -> String -> Value -> Test
evalTest name expression result =
    test name <|
        \_ ->
            Eval.eval expression
                |> Expect.equal (Ok result)
