module Kernel.String exposing (filter, foldl, foldr, fromNumber)

import Eval.Types as Types exposing (Eval)
import Value exposing (Value(..), typeError)


fromNumber : Value -> Eval String
fromNumber s _ env =
    case s of
        Int i ->
            Types.succeed <| String.fromInt i

        Float f ->
            Types.succeed <| String.fromFloat f

        _ ->
            Types.fail <| typeError env <| "Cannot convert " ++ Value.toString s ++ " to a number"


foldr : (Char -> Eval (Value -> Eval Value)) -> Value -> String -> Eval Value
foldr f i xs =
    Types.foldr
        (\el acc c e ->
            Types.andThen (\fe -> fe acc c e) (f el c e)
        )
        i
        (String.toList xs)


foldl : (Char -> Eval (Value -> Eval Value)) -> Value -> String -> Eval Value
foldl f i xs =
    Types.foldl
        (\el acc c e ->
            Types.andThen (\fe -> fe acc c e) (f el c e)
        )
        i
        (String.toList xs)


filter : (Char -> Eval Bool) -> String -> Eval String
filter f s cfg env =
    Types.foldr
        (\char acc c e ->
            Types.map
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
        |> Types.map String.fromList
