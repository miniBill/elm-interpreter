module Eval.Types exposing (andThen, combineMap, errorToString, evalErrorToString, fail, failPartial, foldl, foldr, fromResult, map, map2, onValue, recurseMapThen, recurseThen, succeed, succeedPartial, toResult)

import Browser.Dom exposing (Error)
import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.Node exposing (Node)
import Parser
import Recursion exposing (Rec)
import Recursion.Traverse
import Rope exposing (Rope)
import Syntax
import Types exposing (CallTree, Config, Env, Error(..), Eval, EvalErrorData, EvalErrorKind(..), EvalResult, PartialResult)


onValue : (a -> Result EvalErrorData out) -> EvalResult a -> EvalResult out
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


foldl : (a -> out -> Eval out) -> out -> List a -> Eval out
foldl f init xs cfg env =
    List.foldl
        (\el acc ->
            case toResult acc of
                Err _ ->
                    acc

                Ok a ->
                    f el a cfg env
        )
        (succeed init)
        xs


foldr : (a -> out -> Eval out) -> out -> List a -> Eval out
foldr f init xs cfg env =
    List.foldr
        (\el acc ->
            case toResult acc of
                Err _ ->
                    acc

                Ok a ->
                    f el a cfg env
        )
        (succeed init)
        xs


succeed : a -> EvalResult a
succeed x =
    fromResult <| Ok x


fail : EvalErrorData -> EvalResult a
fail e =
    fromResult <| Err e


succeedPartial : v -> PartialResult v
succeedPartial v =
    Recursion.base (succeed v)


failPartial : EvalErrorData -> PartialResult v
failPartial e =
    Recursion.base (fail e)


fromResult : Result EvalErrorData a -> EvalResult a
fromResult x =
    ( x, Rope.empty, Rope.empty )


toResult : EvalResult out -> Result EvalErrorData out
toResult ( res, _, _ ) =
    res


errorToString : Error -> String
errorToString err =
    case err of
        ParsingError deadEnds ->
            "Parsing error: " ++ Parser.deadEndsToString deadEnds

        EvalError evalError ->
            evalErrorToString evalError


evalErrorToString : EvalErrorData -> String
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

                Todo message ->
                    "Todo: " ++ message
    in
    messageWithType
        ++ "\nCall stack:\n - "
        ++ String.join "\n - " (List.reverse <| List.map Syntax.qualifiedNameToString callStack)


recurseThen :
    ( Node Expression, Config, Env )
    -> (out -> PartialResult out)
    -> PartialResult out
recurseThen expr f =
    Recursion.recurseThen expr
        (wrapThen f)


wrapThen :
    (value
     -> Rec r t (EvalResult a)
    )
    -> EvalResult value
    -> Rec r t (EvalResult a)
wrapThen f ( value, trees, logs ) =
    case value of
        Err e ->
            Recursion.base ( Err e, trees, logs )

        Ok v ->
            f v
                |> Recursion.map
                    (\( result, ftrees, flogs ) ->
                        ( result
                        , Rope.appendTo trees ftrees
                        , Rope.appendTo logs flogs
                        )
                    )


recurseMapThen :
    ( List (Node Expression), Config, Env )
    -> (List out -> PartialResult out)
    -> PartialResult out
recurseMapThen ( exprs, cfg, env ) f =
    Recursion.Traverse.sequenceListThen (List.map (\e -> ( e, cfg, env )) exprs)
        (\results ->
            let
                ( values, trees, logs ) =
                    combine results
            in
            case values of
                Err e ->
                    Recursion.base ( Err e, trees, logs )

                Ok vs ->
                    f vs
                        |> Recursion.map
                            (\( result, ftrees, flogs ) ->
                                ( result
                                , Rope.appendTo trees ftrees
                                , Rope.appendTo logs flogs
                                )
                            )
        )


combine : List (EvalResult t) -> EvalResult (List t)
combine ls =
    let
        go : List (EvalResult t) -> ( List t, Rope CallTree, Rope String ) -> EvalResult (List t)
        go queue ( vacc, tacc, lacc ) =
            case queue of
                [] ->
                    ( Ok <| List.reverse vacc, tacc, lacc )

                ( Err e, trees, logs ) :: _ ->
                    ( Err e, Rope.appendTo tacc trees, Rope.appendTo lacc logs )

                ( Ok v, trees, logs ) :: tail ->
                    go tail ( v :: vacc, Rope.appendTo tacc trees, Rope.appendTo lacc logs )
    in
    go ls ( [], Rope.empty, Rope.empty )
