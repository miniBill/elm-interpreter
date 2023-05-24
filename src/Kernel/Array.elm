module Kernel.Array exposing (appendN, initialize, initializeFromList)

import Array exposing (Array)
import List.Extra
import Result.Extra
import Value exposing (Env, EvalResult, Value)


appendN : Int -> Array Value -> Array Value -> Array Value
appendN n dest source =
    let
        itemsToCopy : Int
        itemsToCopy =
            min (Array.length source) (n - Array.length dest)
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
initialize : Env -> Int -> Int -> (Env -> Int -> EvalResult Value) -> EvalResult (Array Value)
initialize env len offset f =
    List.range offset (offset + len - 1)
        |> Result.Extra.combineMap (f env)
        |> Result.map Array.fromList
