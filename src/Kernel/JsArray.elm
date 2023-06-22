module Kernel.JsArray exposing (appendN, foldl, foldr, indexedMap, initialize, initializeFromList, map, unsafeGet)

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


{-| Initialize an array from a list. `initializeFromList n ls` creates an array of,
at most, `n` elements from the list. The return value is a tuple containing the
created array as well as a list without the first `n` elements.

This function was created specifically for the `Array` module, which never wants
to create `JsArray`s above a certain size. That being said, because every
manipulation of `JsArray` results in a copy, users should always try to keep
these as small as possible. The `n` parameter should always be set to a
reasonably small value.

-}
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
        (\e acc ->
            case Types.toResult acc of
                Err _ ->
                    acc

                Ok _ ->
                    Types.map2 Tuple.pair (f e cfg env) acc
                        |> Types.andThen (\( g, y ) -> g y cfg env)
        )
        (Types.succeed init)
        arr


foldl : (Value -> Eval (Value -> Eval Value)) -> Value -> Array Value -> Eval Value
foldl f init arr cfg env =
    Array.foldl
        (\e acc ->
            case Types.toResult acc of
                Err _ ->
                    acc

                Ok _ ->
                    Types.map2 Tuple.pair (f e cfg env) acc
                        |> Types.andThen (\( g, y ) -> g y cfg env)
        )
        (Types.succeed init)
        arr


map : (Value -> Eval Value) -> Array Value -> Eval (Array Value)
map f array cfg env =
    Types.combineMap f (Array.toList array) cfg env
        |> Types.map Array.fromList


indexedMap : (Int -> Eval (Value -> Eval Value)) -> Array Value -> Eval (Array Value)
indexedMap f array cfg env =
    Types.combineMap f (List.range 0 (Array.length array - 1)) cfg env
        |> Types.andThen
            (\fs ->
                Types.combineMap
                    (\( ef, ex ) -> ef ex)
                    (List.map2 Tuple.pair fs (Array.toList array))
                    cfg
                    env
            )
        |> Types.map Array.fromList


unsafeGet : Int -> Array Value -> Eval Value
unsafeGet index array _ env =
    case Array.get index array of
        Just v ->
            Types.succeed v

        Nothing ->
            Types.fail <| Value.typeError env "Out of bounds access"
