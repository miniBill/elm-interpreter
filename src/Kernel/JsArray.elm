module Kernel.JsArray exposing (appendN, foldr, initialize, initializeFromList)

import Array exposing (Array)
import Eval.Types exposing (Eval)
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
    List.range offset (offset + len - 1)
        |> List.foldr
            (\e ( racc, callTrees ) ->
                case racc of
                    Err _ ->
                        ( racc, callTrees )

                    Ok acc ->
                        case f e cfg env of
                            ( Err err, fCallTree ) ->
                                ( Err err, fCallTree ++ callTrees )

                            ( Ok g, fCallTree ) ->
                                ( Ok (g :: acc), fCallTree ++ callTrees )
            )
            ( Ok [], [] )
        |> Tuple.mapFirst (Result.map Array.fromList)


foldr : (Value -> Eval (Value -> Eval Value)) -> Value -> Array Value -> Eval Value
foldr f init arr cfg env =
    Array.foldr
        (\e ( racc, callTrees ) ->
            case racc of
                Err _ ->
                    ( racc, callTrees )

                Ok acc ->
                    case f e cfg env of
                        ( Ok g, fCallTree ) ->
                            case g acc cfg env of
                                ( Err err, gCallTree ) ->
                                    ( Err err, gCallTree ++ fCallTree ++ callTrees )

                                ( Ok h, gCallTree ) ->
                                    ( Ok h, gCallTree ++ fCallTree ++ callTrees )

                        ( Err err, fCallTree ) ->
                            ( Err err, fCallTree ++ callTrees )
        )
        ( Ok init, [] )
        arr
