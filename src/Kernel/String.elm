module Kernel.String exposing (fromNumber)

import Eval.Types as Types exposing (Eval)
import Value exposing (Value(..), typeError)


fromNumber : Value -> Eval String
fromNumber s _ env =
    case s of
        Int i ->
            Types.succeed <| String.fromInt i

        Float f ->
            Types.succeed <| String.fromFloat f

        _ ->
            Types.fail <| typeError env <| "Cannot convert " ++ Value.toString s ++ " to a number"
