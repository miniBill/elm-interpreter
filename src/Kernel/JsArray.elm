module Kernel.JsArray exposing (appendN, foldr, initialize, initializeFromList)

import Array exposing (Array)
import List.Extra
import Result.Extra
import Value exposing (Env, EvalResult, Value)


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
initialize : Env -> Int -> Int -> (Int -> EvalResult Value) -> EvalResult (Array Value)
initialize _ len offset f =
    List.range offset (offset + len - 1)
        |> Result.Extra.combineMap f
        |> Result.map Array.fromList


foldr : Env -> (Value -> EvalResult (Value -> EvalResult Value)) -> Value -> Array Value -> EvalResult Value
foldr _ f init arr =
    Array.foldr
        (\e racc ->
            case racc of
                Err err ->
                    Err err

                Ok acc ->
                    case f e of
                        Ok g ->
                            g acc

                        Err err ->
                            Err err
        )
        (Ok init)
        arr
