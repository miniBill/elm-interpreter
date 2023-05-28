module Eval.Types exposing (CallTree(..), CallTreeContinuation, Config, Error(..), Eval, EvalResult, PartialEval, PartialResult(..), andThen, andThenPartial, combineMap, errorToString, evalErrorToString, fail, failPartial, fromResult, map, map2, onValue, partialResultToString, succeed, succeedPartial)

import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.Node exposing (Node)
import Elm.Syntax.Pattern exposing (QualifiedNameRef)
import Elm.Writer
import Parser exposing (DeadEnd)
import Syntax
import Value exposing (Env, EvalError, EvalErrorKind(..), Value)


type alias PartialEval =
    Config -> Env -> PartialResult


type alias Eval out =
    Config -> Env -> EvalResult out


type alias EvalResult out =
    ( Result EvalError out
    , List CallTree
    )


onValue : (a -> Result EvalError out) -> EvalResult a -> EvalResult out
onValue f ( x, callTrees ) =
    ( Result.andThen f x
    , callTrees
    )


andThen : (a -> EvalResult b) -> EvalResult a -> EvalResult b
andThen f ( v, callTrees ) =
    case v of
        Err e ->
            ( Err e, callTrees )

        Ok w ->
            let
                ( y, fxCallTrees ) =
                    f w
            in
            ( y, fxCallTrees ++ callTrees )


map : (a -> out) -> EvalResult a -> EvalResult out
map f ( x, callTrees ) =
    ( Result.map f x
    , callTrees
    )


map2 : (a -> b -> out) -> EvalResult a -> EvalResult b -> EvalResult out
map2 f ( lv, lc ) ( rv, rc ) =
    ( Result.map2 f lv rv
    , lc ++ rc
    )


combineMap : (a -> Eval b) -> List a -> Eval (List b)
combineMap f xs cfg env =
    List.foldr
        (\el (( listAcc, _ ) as acc) ->
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
    ( x, [] )


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
        ( Err e, callTrees ) ->
            PartialValue ( Err e, callTrees )

        ( Ok w, callTrees ) ->
            case f w of
                PartialValue y ->
                    PartialValue <| map2 (\_ vy -> vy) x y

                PartialExpression expr newConfig newEnv ->
                    PartialExpression
                        expr
                        { newConfig
                            | callTreeContinuation = \children result -> newConfig.callTreeContinuation (callTrees ++ children) result
                        }
                        newEnv


partialResultToString : PartialResult -> String
partialResultToString result =
    case result of
        PartialValue ( Ok v, _ ) ->
            Value.toString v

        PartialExpression expr _ _ ->
            Elm.Writer.write (Elm.Writer.writeExpression expr)

        PartialValue ( Err e, _ ) ->
            errorToString (EvalError e)


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
