module Eval.Types exposing (CallTree(..), CallTreeContinuation, Config, Error(..), Eval, EvalResult, PartialEval, PartialResult(..), andThen, andThenPartial, combineMap, fail, failPartial, map, map2, onValue, succeed, succeedPartial)

import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.Node exposing (Node)
import Elm.Syntax.Pattern exposing (QualifiedNameRef)
import Parser exposing (DeadEnd)
import Value exposing (Env, EvalError, Value)


type alias PartialEval =
    Config -> Env -> PartialResult


type alias Eval out =
    Config -> Env -> EvalResult out


type alias EvalResult out =
    ( Result EvalError out, List CallTree, List ( Expression, PartialResult ) )


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
            ( y, fxCallTrees ++ callTrees, fxExpressions ++ expressions )


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
    , le ++ re
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
    ( Ok x, [], [] )


fail : EvalError -> EvalResult a
fail x =
    ( Err x, [], [] )


type alias Config =
    { trace : Bool
    , callTreeContinuation : CallTreeContinuation
    }


type alias CallTreeContinuation =
    List CallTree -> Result EvalError Value -> List CallTree


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
    = PartialExpression Env (Node Expression) CallTreeContinuation
    | PartialValue (EvalResult Value)


succeedPartial : Value -> PartialResult
succeedPartial v =
    PartialValue (succeed v)


failPartial : EvalError -> PartialResult
failPartial e =
    PartialValue (fail e)


andThenPartial : (a -> PartialResult) -> EvalResult a -> PartialResult
andThenPartial f (( vx, callTrees, expressions ) as x) =
    case vx of
        Err e ->
            PartialValue ( Err e, callTrees, expressions )

        Ok w ->
            case f w of
                PartialValue y ->
                    PartialValue <| map2 (\_ vy -> vy) x y

                PartialExpression env expr cont ->
                    let
                        _ =
                            Debug.todo
                    in
                    PartialExpression env expr cont
