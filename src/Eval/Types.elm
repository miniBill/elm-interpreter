module Eval.Types exposing (CallTree(..), CallTreeContinuation, Config, Error(..), Eval, EvalResult, PartialEval, PartialResult(..), TraceContinuation, TraceLine, andThen, andThenPartial, combineMap, fail, failPartial, fromResult, map, map2, onValue, succeed, succeedPartial)

import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.Node exposing (Node)
import Elm.Syntax.Pattern exposing (QualifiedNameRef)
import Parser exposing (DeadEnd)
import Rope exposing (Rope)
import Value exposing (Env, EvalError, Value)


type alias PartialEval =
    Config -> Env -> PartialResult


type alias Eval out =
    Config -> Env -> EvalResult out


type alias EvalResult out =
    ( Result EvalError out
    , List CallTree
    , Rope TraceLine
    )


type alias TraceLine =
    ( Expression
    , PartialResult
    )


onValue : (a -> Result EvalError out) -> EvalResult a -> EvalResult out
onValue f ( x, callTrees, expressions ) =
    ( Result.andThen f x
    , callTrees
    , expressions
    )


andThen : (a -> EvalResult b) -> EvalResult a -> EvalResult b
andThen f ( v, callTrees, expressions ) =
    case v of
        Err e ->
            ( Err e, callTrees, expressions )

        Ok w ->
            let
                ( y, fxCallTrees, fxExpressions ) =
                    f w
            in
            ( y, fxCallTrees ++ callTrees, Rope.appendTo expressions fxExpressions )


map : (a -> out) -> EvalResult a -> EvalResult out
map f ( x, callTrees, expressions ) =
    ( Result.map f x
    , callTrees
    , expressions
    )


map2 : (a -> b -> out) -> EvalResult a -> EvalResult b -> EvalResult out
map2 f ( lv, lc, le ) ( rv, rc, re ) =
    ( Result.map2 f lv rv
    , lc ++ rc
    , Rope.appendTo le re
    )


combineMap : (a -> Eval b) -> List a -> Eval (List b)
combineMap f xs cfg env =
    List.foldr
        (\el (( listAcc, _, _ ) as acc) ->
            case listAcc of
                Err _ ->
                    acc

                Ok _ ->
                    map2 (::)
                        (f el cfg env)
                        acc
        )
        (succeed [])
        xs


succeed : a -> EvalResult a
succeed x =
    fromResult <| Ok x


fail : EvalError -> EvalResult a
fail e =
    fromResult <| Err e


fromResult : Result EvalError a -> EvalResult a
fromResult x =
    ( x, [], Rope.empty )


type alias Config =
    { trace : Bool
    , callTreeContinuation : CallTreeContinuation
    , traceContinuation : TraceContinuation
    }


type alias CallTreeContinuation =
    List CallTree -> Result EvalError Value -> List CallTree


type alias TraceContinuation =
    Rope TraceLine -> Rope TraceLine


type CallTree
    = CallNode
        String
        QualifiedNameRef
        { args : List Value
        , result : Result EvalError Value
        , children : List CallTree
        }


type Error
    = ParsingError (List DeadEnd)
    | EvalError EvalError


{-| Represent the result of a computation inside one of the branches of `evalExpression`.

This is needed because to get TCO we need to return an expression, rather than calling `evalExpression` recursively.

-}
type PartialResult
    = PartialExpression Env (Node Expression) CallTreeContinuation TraceContinuation
    | PartialValue (EvalResult Value)


succeedPartial : Value -> PartialResult
succeedPartial v =
    PartialValue (succeed v)


failPartial : EvalError -> PartialResult
failPartial e =
    PartialValue (fail e)


andThenPartial : (a -> PartialResult) -> EvalResult a -> PartialResult
andThenPartial f x =
    case x of
        ( Err e, callTrees, expressions ) ->
            PartialValue ( Err e, callTrees, expressions )

        ( Ok w, callTrees, expressions ) ->
            case f w of
                PartialValue y ->
                    PartialValue <| map2 (\_ vy -> vy) x y

                PartialExpression env expr cont e ->
                    PartialExpression env
                        expr
                        (\children result -> cont (callTrees ++ children) result)
                        (\n -> Rope.appendTo (e n) expressions)
