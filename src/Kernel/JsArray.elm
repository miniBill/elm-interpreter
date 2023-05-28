module Kernel.JsArray exposing (appendN, foldr, initialize, initializeFromList)

import Array exposing (Array)
import Eval.Types as Types exposing (Eval)
import List.Extra
import Value exposing (Value)


appendN : Int -> Array Value -> Array Value -> Array Value
appendN n dest source =
    let
        itemsToCopy : Int
        itemsToCopy =
            n - Array.length dest
    in
    Array.append
        dest
        (Array.slice 0 itemsToCopy source)


initializeFromList : Int -> List Value -> ( Array Value, List Value )
initializeFromList n values =
    let
        ( before, after ) =
            List.Extra.splitAt n values
    in
    ( Array.fromList before, after )


{-| Initialize an array. `initalize n offset fn` creates an array of length `n`
with the element at index `i` initialized to the result of `(f (i + offset))`.

The offset parameter is there so one can avoid creating a closure for this use
case. This is an optimization that has proved useful in the `Array` module.

    initialize 3 5 identity == [ 5, 6, 7 ]

-}
initialize : Int -> Int -> (Int -> Eval Value) -> Eval (Array Value)
initialize len offset f cfg env =
    Types.combineMap f (List.range offset (offset + len - 1)) cfg env
        |> Types.map Array.fromList


foldr : (Value -> Eval (Value -> Eval Value)) -> Value -> Array Value -> Eval Value
foldr f init arr cfg env =
    Array.foldr
        (\e (( resultAcc, _, _ ) as acc) ->
            case resultAcc of
                Err _ ->
                    acc

                Ok _ ->
                    Types.map2 Tuple.pair (f e cfg env) acc
                        |> Types.andThen (\( g, y ) -> g y cfg env)
        )
        (Types.succeed init)
        arr
