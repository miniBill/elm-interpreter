module Elm.Kernel exposing (functions)

import Bitwise
import FastDict as Dict exposing (Dict)
import Value exposing (EvalError(..), Value(..))


functions : Dict String ( Int, List Value -> Result EvalError Value )
functions =
    [ ( "Elm.Bitwise.xor", twoInt Bitwise.xor ) ]
        |> Dict.fromList


twoInt : (Int -> Int -> Int) -> ( Int, List Value -> Result EvalError Value )
twoInt f =
    ( 2
    , \args ->
        case args of
            [ Int li, Int ri ] ->
                Ok <| Int (f li ri)

            _ ->
                Err <| TypeError "Expected two Ints"
    )
