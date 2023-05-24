module Kernel.Utils exposing (append)

import Value exposing (Env, EvalResult, Value(..), typeError)


append : Env -> Value -> Value -> EvalResult Value
append env l r =
    case ( l, r ) of
        ( String ls, String rs ) ->
            Ok <| String (ls ++ rs)

        ( List ll, List rl ) ->
            Ok <| List (ll ++ rl)

        _ ->
            typeError env <| "Cannot append " ++ Value.toString l ++ " and " ++ Value.toString r
