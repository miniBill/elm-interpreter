module Eval.PartialResult exposing (fromValue)

import Eval.Types exposing (CallTree, PartialResult(..))
import Value exposing (EvalResult, Value)


fromValue : ( EvalResult Value, List CallTree ) -> PartialResult
fromValue ( result, callTrees ) =
    case result of
        Err e ->
            PartialErr callTrees e

        Ok value ->
            PartialValue callTrees value
