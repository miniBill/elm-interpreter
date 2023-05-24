module Kernel.List exposing (sortBy)

import Kernel.Utils
import Result.Extra
import Value exposing (Env, EvalResult, Value)


sortBy : Env -> (Value -> EvalResult Value) -> List Value -> EvalResult (List Value)
sortBy env toComparable list =
    list
        |> Result.Extra.combineMap
            (\value -> Result.map (Tuple.pair value) (toComparable value))
        |> Result.map
            (List.sortWith
                (\( _, lc ) ( _, rc ) ->
                    Kernel.Utils.compare env lc rc
                        -- TODO: find out how to deal with errors
                        |> Result.withDefault EQ
                )
                >> List.map Tuple.first
            )
