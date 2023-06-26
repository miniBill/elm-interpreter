module Kernel.String exposing (filter, foldl, foldr, fromNumber)

import Eval exposing (typeError)
import Expr
import Types exposing (Eval, Expr(..))


fromNumber : Expr -> Eval String
fromNumber s _ env =
    case s of
        Int i ->
            Eval.succeed <| String.fromInt i

        Float f ->
            Eval.succeed <| String.fromFloat f

        _ ->
            Eval.fail <| typeError env <| "Cannot convert " ++ Expr.toString s ++ " to a string"


foldr : (Char -> Eval (Expr -> Eval Expr)) -> Expr -> String -> Eval Expr
foldr f i xs =
    Eval.foldr
        (\el acc c e ->
            Eval.andThen (\fe -> fe acc c e) (f el c e)
        )
        i
        (String.toList xs)


foldl : (Char -> Eval (Expr -> Eval Expr)) -> Expr -> String -> Eval Expr
foldl f i xs =
    Eval.foldl
        (\el acc c e ->
            Eval.andThen (\fe -> fe acc c e) (f el c e)
        )
        i
        (String.toList xs)


filter : (Char -> Eval Bool) -> String -> Eval String
filter f s cfg env =
    Eval.foldr
        (\char acc c e ->
            Eval.map
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
        |> Eval.map String.fromList
