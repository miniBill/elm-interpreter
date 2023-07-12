module Kernel.Debug exposing (log, todo)

import EvalResult
import Rope
import Types exposing (Eval, Value)
import Value


log : String -> Value -> Eval Value
log key value _ _ =
    let
        message : String
        message =
            key ++ ": " ++ Value.toString value
    in
    ( Ok value
    , Rope.empty
    , Rope.singleton message
    )


todo : String -> Eval Value
todo msg _ env =
    EvalResult.fail <| Value.todo env msg
