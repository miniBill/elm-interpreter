module Eval exposing (eval, trace)

import Elm.Syntax.Expression as Expression exposing (Expression)
import Eval.Log as Log
import Eval.Module
import Eval.Types exposing (CallTree, Error)
import Rope exposing (Rope)
import Value exposing (Value)


eval : String -> Result Error Value
eval expressionSource =
    let
        source : String
        source =
            toModule expressionSource

        expression : Expression
        expression =
            Expression.FunctionOrValue [] "main"
    in
    Eval.Module.eval source expression


trace : String -> ( Result Error Value, Rope CallTree, Rope Log.Line )
trace expressionSource =
    let
        source : String
        source =
            toModule expressionSource

        expression : Expression
        expression =
            Expression.FunctionOrValue [] "main"
    in
    Eval.Module.trace source expression


toModule : String -> String
toModule expression =
    "module Main exposing (main)\n\nmain =\n"
        ++ indent 4 expression


indent : Int -> String -> String
indent count input =
    let
        prefix : String
        prefix =
            String.repeat count " "
    in
    input
        |> String.split "\n"
        |> List.map
            (\line ->
                if String.isEmpty line then
                    line

                else
                    prefix ++ line
            )
        |> String.join "\n"
