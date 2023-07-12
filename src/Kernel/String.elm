module Kernel.String exposing (filter, foldl, foldr, fromNumber)

import Eval.Types as Types
import EvalResult
import Types exposing (Eval, Value(..))
import Value exposing (typeError)


fromNumber : Value -> Eval String
fromNumber s _ env =
    case s of
        Int i ->
            EvalResult.succeed <| String.fromInt i

        Float f ->
            EvalResult.succeed <| String.fromFloat f

        _ ->
            EvalResult.fail <| typeError env <| "Cannot convert " ++ Value.toString s ++ " to a string"


foldr : (Char -> Eval (Value -> Eval Value)) -> Value -> String -> Eval Value
foldr f i xs =
    Types.foldr
        (\el acc c e ->
            EvalResult.andThen (\fe -> fe acc c e) (f el c e)
        )
        i
        (String.toList xs)


foldl : (Char -> Eval (Value -> Eval Value)) -> Value -> String -> Eval Value
foldl f i xs =
    Types.foldl
        (\el acc c e ->
            EvalResult.andThen (\fe -> fe acc c e) (f el c e)
        )
        i
        (String.toList xs)


filter : (Char -> Eval Bool) -> String -> Eval String
filter f s cfg env =
    Types.foldr
        (\char acc c e ->
            EvalResult.map
                (\fc ->
                    if fc then
                        char :: acc

                    else
                        acc
                )
                (f char c e)
        )
        []
        (String.toList s)
        cfg
        env
        |> EvalResult.map String.fromList
