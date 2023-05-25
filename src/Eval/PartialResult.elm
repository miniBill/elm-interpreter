module Eval.PartialResult exposing (PartialResult(..), fromValue)

{-| -}

import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.Node exposing (Node)
import Value exposing (Env, EvalError, Value)


{-| Represent the result of a computation inside one of the branches of `evalExpression`.

This is needed because to get TCO we need to return an expression, rather than calling `evalExpression` recursively.

-}
type PartialResult
    = PartialExpression Env (Node Expression)
    | PartialValue Value
    | PartialErr EvalError


fromValue : Result EvalError Value -> PartialResult
fromValue result =
    case result of
        Err e ->
            PartialErr e

        Ok value ->
            PartialValue value
