module TestUtils exposing (evalExpect, evalTest, evalTest_, list, maybe, slowTest)

import Eval
import Eval.Types exposing (Error(..))
import Expect
import Syntax
import Test exposing (Test, test)
import Value exposing (Value(..))


evalTest_ : String -> (a -> Value) -> a -> Test
evalTest_ expr toValue a =
    evalTest expr expr toValue a


evalTest : String -> String -> (a -> Value) -> a -> Test
evalTest name expression toValue a =
    test name <|
        \_ ->
            evalExpect expression toValue a


evalExpect : String -> (a -> Value) -> a -> Expect.Expectation
evalExpect expression toValue a =
    let
        result : Value
        result =
            toValue a
    in
    case ( Eval.eval expression, result ) of
        ( Ok (Int i), Float _ ) ->
            (Float <| toFloat i)
                |> Expect.equal result

        ( Err (EvalError e), _ ) ->
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
    test 10


list : (a -> Value) -> List a -> Value
list f xs =
    List (List.map f xs)


maybe : (a -> Value) -> Maybe a -> Value
maybe f mx =
    case mx of
        Nothing ->
            Custom { moduleName = [ "Maybe" ], name = "Nothing" } []

        Just x ->
            Custom { moduleName = [ "Maybe" ], name = "Just" } [ f x ]
