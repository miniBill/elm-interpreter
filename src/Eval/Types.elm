module Eval.Types exposing (CallTree(..), CallTreeContinuation, Config, Error(..), Eval, EvalResult, LogContinuation, LogLine, PartialEval, PartialResult(..), andThen, andThenPartial, combineMap, errorToString, evalErrorToString, fail, failPartial, fromResult, map, map2, onValue, partialResultToString, succeed, succeedPartial, toResult)

import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.Node exposing (Node)
import Elm.Syntax.Pattern exposing (QualifiedNameRef)
import Elm.Writer
import FastDict exposing (Dict)
import Parser exposing (DeadEnd)
import Rope exposing (Rope)
import Syntax
import Value exposing (Env, EvalError, EvalErrorKind(..), Value)


type alias PartialEval =
    Config -> Env -> PartialResult


type alias Eval out =
    Config -> Env -> EvalResult out


type alias EvalResult out =
    ( Result EvalError out
    , Rope CallTree
    , Rope LogLine
    )


type alias LogLine =
    { stack : List QualifiedNameRef
    , message : String
    , env : Dict String Value
    }


onValue : (a -> Result EvalError out) -> EvalResult a -> EvalResult out
onValue f ( x, callTrees, logs ) =
    ( Result.andThen f x
    , callTrees
    , logs
    )


andThen : (a -> EvalResult b) -> EvalResult a -> EvalResult b
andThen f ( v, callTrees, logs ) =
    case v of
        Err e ->
            ( Err e, callTrees, logs )

        Ok w ->
            let
                ( y, fxCallTrees, fxLogs ) =
                    f w
            in
            ( y
            , Rope.appendTo callTrees fxCallTrees
            , Rope.appendTo logs fxLogs
            )


map : (a -> out) -> EvalResult a -> EvalResult out
map f ( x, callTrees, logs ) =
    ( Result.map f x
    , callTrees
    , logs
    )


map2 : (a -> b -> out) -> EvalResult a -> EvalResult b -> EvalResult out
map2 f ( lv, lc, ll ) ( rv, rc, rl ) =
    ( Result.map2 f lv rv
    , Rope.appendTo lc rc
    , Rope.appendTo ll rl
    )


combineMap : (a -> Eval b) -> List a -> Eval (List b)
combineMap f xs cfg env =
    List.foldr
        (\el acc ->
            case toResult acc of
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
    ( x, Rope.empty, Rope.empty )


type alias Config =
    { trace : Bool
    , callTreeContinuation : CallTreeContinuation
    , logContinuation : LogContinuation
    }


type alias CallTreeContinuation =
    Rope CallTree -> Result EvalError Value -> Rope CallTree


type alias LogContinuation =
    Rope LogLine -> Rope LogLine


type CallTree
    = CallNode
        String
        QualifiedNameRef
        { args : List Value
        , result : Result EvalError Value
        , children : Rope CallTree
        }


type Error
    = ParsingError (List DeadEnd)
    | EvalError EvalError


{-| Represent the result of a computation inside one of the branches of `evalExpression`.

This is needed because to get TCO we need to return an expression, rather than calling `evalExpression` recursively.

-}
type PartialResult
    = PartialExpression (Node Expression) Config Env
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
        ( Err e, callTrees, logs ) ->
            PartialValue ( Err e, callTrees, logs )

        ( Ok w, callTrees, logs ) ->
            case f w of
                PartialValue y ->
                    PartialValue <| map2 (\_ vy -> vy) x y

                PartialExpression expr newConfig newEnv ->
                    PartialExpression
                        expr
                        { newConfig
                            | callTreeContinuation = \children result -> newConfig.callTreeContinuation (Rope.appendTo callTrees children) result
                            , logContinuation = Rope.appendTo logs
                        }
                        newEnv


toResult : EvalResult out -> Result EvalError out
toResult ( res, _, _ ) =
    res


partialResultToString : PartialResult -> String
partialResultToString result =
    case result of
        PartialValue evalResult ->
            case toResult evalResult of
                Ok v ->
                    Value.toString v

                Err e ->
                    errorToString (EvalError e)

        PartialExpression expr _ _ ->
            Elm.Writer.write (Elm.Writer.writeExpression expr)


errorToString : Error -> String
errorToString err =
    case err of
        ParsingError deadEnds ->
            "Parsing error: " ++ Parser.deadEndsToString deadEnds

        EvalError evalError ->
            evalErrorToString evalError


evalErrorToString : EvalError -> String
evalErrorToString { callStack, error } =
    let
        messageWithType : String
        messageWithType =
            case error of
                TypeError message ->
                    "Type error: " ++ message

                Unsupported message ->
                    "Unsupported: " ++ message

                NameError name ->
                    "Name error: " ++ name ++ " not found"
    in
    messageWithType
        ++ "\nCall stack:\n - "
        ++ String.join "\n - " (List.reverse <| List.map Syntax.qualifiedNameToString callStack)
