module Kernel.String exposing (fromNumber)

import Value exposing (Env, EvalResult, Value(..), typeError)


fromNumber : Env -> Value -> EvalResult String
fromNumber env s =
    case s of
        Int i ->
            Ok <| String.fromInt i

        Float f ->
            Ok <| String.fromFloat f

        _ ->
            typeError env <| "Cannot convert " ++ Value.toString s ++ " to a number"
