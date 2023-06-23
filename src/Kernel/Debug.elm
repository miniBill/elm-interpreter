module Kernel.Debug exposing (..)

import Eval.Log as Log
import Eval.Types as Types exposing (Eval)
import FastDict as Dict
import Rope
import Value exposing (Value)


log : String -> Value -> Eval Value
log key value _ env =
    let
        line : Log.Line
        line =
            { stack = env.callStack
            , message = key ++ ": " ++ Value.toString value
            , env = Dict.empty
            }
    in
    ( Ok value
    , Rope.empty
    , Rope.singleton line
    )


todo : String -> Eval Value
todo msg _ env =
    Types.fail <| Value.todo env msg
