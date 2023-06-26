module Kernel.JsArray exposing (appendN, foldl, foldr, indexedMap, initialize, initializeFromList, map, unsafeGet)

import Array exposing (Array)
import Eval
import List.Extra
import Types exposing (Eval, Expr)


appendN : Int -> Array Expr -> Array Expr -> Array Expr
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
initializeFromList : Int -> List Expr -> ( Array Expr, List Expr )
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
initialize : Int -> Int -> (Int -> Eval Expr) -> Eval (Array Expr)
initialize len offset f cfg env =
    Eval.combineMap f (List.range offset (offset + len - 1)) cfg env
        |> Eval.map Array.fromList


foldr : (Expr -> Eval (Expr -> Eval Expr)) -> Expr -> Array Expr -> Eval Expr
foldr f init arr cfg env =
    Array.foldr
        (\e acc ->
            case Eval.toResult acc of
                Err _ ->
                    acc

                Ok _ ->
                    Eval.map2 Tuple.pair (f e cfg env) acc
                        |> Eval.andThen (\( g, y ) -> g y cfg env)
        )
        (Eval.succeed init)
        arr


foldl : (Expr -> Eval (Expr -> Eval Expr)) -> Expr -> Array Expr -> Eval Expr
foldl f init arr cfg env =
    Array.foldl
        (\e acc ->
            case Eval.toResult acc of
                Err _ ->
                    acc

                Ok _ ->
                    Eval.map2 Tuple.pair (f e cfg env) acc
                        |> Eval.andThen (\( g, y ) -> g y cfg env)
        )
        (Eval.succeed init)
        arr


map : (Expr -> Eval Expr) -> Array Expr -> Eval (Array Expr)
map f array cfg env =
    Eval.combineMap f (Array.toList array) cfg env
        |> Eval.map Array.fromList


indexedMap : (Int -> Eval (Expr -> Eval Expr)) -> Array Expr -> Eval (Array Expr)
indexedMap f array cfg env =
    Eval.combineMap f (List.range 0 (Array.length array - 1)) cfg env
        |> Eval.andThen
            (\fs ->
                Eval.combineMap
                    (\( ef, ex ) -> ef ex)
                    (List.map2 Tuple.pair fs (Array.toList array))
                    cfg
                    env
            )
        |> Eval.map Array.fromList


unsafeGet : Int -> Array Expr -> Eval Expr
unsafeGet index array _ env =
    case Array.get index array of
        Just v ->
            Eval.succeed v

        Nothing ->
            Eval.fail <| Eval.typeError env "Out of bounds access"
