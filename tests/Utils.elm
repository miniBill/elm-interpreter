module Utils exposing (evalTest, evalTest_)

import Eval
import Expect
import Test exposing (Test, test)
import Value exposing (Value(..))


evalTest_ : String -> (a -> Value) -> a -> Test
evalTest_ expr toValue a =
    evalTest expr expr toValue a


evalTest : String -> String -> (a -> Value) -> a -> Test
evalTest name expression toValue a =
    let
        result =
            toValue a
    in
    test name <|
        \_ ->
            case ( Eval.eval expression, result ) of
                ( Ok (Int i), Float _ ) ->
                    (Float <| toFloat i)
                        |> Expect.equal result

                ( v, _ ) ->
                    v |> Expect.equal (Ok result)
