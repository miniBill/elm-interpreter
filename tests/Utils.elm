module Utils exposing (evalTest, evalTest_, slowTest)

import Eval
import Expect
import Syntax
import Test exposing (Test, test)
import Value exposing (Value(..))


evalTest_ : String -> (a -> Value) -> a -> Test
evalTest_ expr toValue a =
    evalTest expr expr toValue a


evalTest : String -> String -> (a -> Value) -> a -> Test
evalTest name expression toValue a =
    let
        result : Value
        result =
            toValue a
    in
    test name <|
        \_ ->
            case ( Eval.eval expression, result ) of
                ( Ok (Int i), Float _ ) ->
                    (Float <| toFloat i)
                        |> Expect.equal result

                ( Err (Eval.EvalError e), _ ) ->
                    Expect.fail <|
                        Debug.toString e.error
                            ++ "\nCall stack:\n - "
                            ++ String.join
                                "\n - "
                                (List.reverse <| List.map Syntax.qualifiedNameToString e.callStack)

                ( Err e, _ ) ->
                    Expect.fail <| Debug.toString e

                ( v, _ ) ->
                    v |> Expect.equal (Ok result)


slowTest : (Int -> Test) -> Test
slowTest test =
    -- Change this to 10 to make it fast
    test 100000
