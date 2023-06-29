module Kernel.Debug exposing (log, todo)

import Eval.Types as Types exposing (Eval)
import Rope
import Value exposing (Value)


log : String -> Value -> Eval Value
log key value _ _ =
    let
        message =
            key ++ ": " ++ Value.toString value
    in
    ( Ok value
    , Rope.empty
    , Rope.singleton message
    )


todo : String -> Eval Value
todo msg _ env =
    Types.fail <| Value.todo env msg
