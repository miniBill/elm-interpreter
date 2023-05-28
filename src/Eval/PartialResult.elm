module Eval.PartialResult exposing
    ( fromValue
    , mapCombine
    )

import Eval.Types exposing (CallTree, Eval, Eval2, PartialResult(..))
import Value exposing (EvalResult, Value)


fromValue : ( EvalResult Value, List CallTree ) -> PartialResult
fromValue ( result, callTrees ) =
    case result of
        Err e ->
            PartialErr callTrees e

        Ok value ->
            PartialValue callTrees value


mapCombine : Eval2 (Eval a b) (List a) (List b)
mapCombine cfg env f xs =
    List.foldr
        (\el ( lacc, callTrees ) ->
            case lacc of
                Err _ ->
                    ( lacc, callTrees )

                Ok acc ->
                    let
                        ( g, callTree ) =
                            f cfg env el
                    in
                    ( Result.map (\h -> h :: acc) g
                    , callTree ++ callTrees
                    )
        )
        ( Ok [], [] )
        xs
