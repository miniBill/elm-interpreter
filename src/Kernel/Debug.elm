module Kernel.Debug exposing (log, todo)

import Environment
import Eval
import Expr
import Rope
import Types exposing (Eval, Expr, LogLine)


log : String -> Expr -> Eval Expr
log key value _ _ =
    let
        line : LogLine
        line =
            { message = key ++ ": " ++ Expr.toString value
            , env = Environment.empty
            }
    in
    ( Ok value
    , Rope.singleton line
    )


todo : String -> Eval Expr
todo msg _ env =
    Eval.fail <| Eval.todo env msg
