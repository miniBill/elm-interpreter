module EvalResult exposing (andThen, appendRopes, combine, fail, fromResult, map, map2, onValue, succeed, toResult)

import Rope exposing (Rope)
import Types exposing (CallTree, EvalErrorData, EvalResult)


{-| Append two ropes, short-circuiting when either is empty.
This avoids building deep trees of empty Node wrappers when trace is off.
-}
appendRopes : Rope a -> Rope a -> Rope a
appendRopes a b =
    if Rope.isEmpty a then
        b

    else if Rope.isEmpty b then
        a

    else
        Rope.appendTo a b


succeed : a -> EvalResult a
succeed x =
    ( Ok x, Rope.empty, Rope.empty )


fail : EvalErrorData -> EvalResult a
fail e =
    ( Err e, Rope.empty, Rope.empty )


fromResult : Result EvalErrorData a -> EvalResult a
fromResult x =
    ( x, Rope.empty, Rope.empty )


toResult : EvalResult out -> Result EvalErrorData out
toResult ( res, _, _ ) =
    res


map : (a -> out) -> EvalResult a -> EvalResult out
map f ( x, callTrees, logs ) =
    ( Result.map f x
    , callTrees
    , logs
    )


andThen : (a -> EvalResult b) -> EvalResult a -> EvalResult b
andThen f ( v, callTrees, logs ) =
    case v of
        Err e ->
            ( Err e, callTrees, logs )

        Ok w ->
            let
                ( y, fxCallTrees, fxLogs ) =
                    f w
            in
            ( y
            , appendRopes callTrees fxCallTrees
            , appendRopes logs fxLogs
            )


map2 : (a -> b -> out) -> EvalResult a -> EvalResult b -> EvalResult out
map2 f ( lv, lc, ll ) ( rv, rc, rl ) =
    ( Result.map2 f lv rv
    , appendRopes lc rc
    , appendRopes ll rl
    )


onValue : (a -> Result EvalErrorData out) -> EvalResult a -> EvalResult out
onValue f ( x, callTrees, logs ) =
    ( Result.andThen f x
    , callTrees
    , logs
    )


combine : List (EvalResult t) -> EvalResult (List t)
combine ls =
    let
        go : List (EvalResult t) -> ( List t, Rope CallTree, Rope String ) -> EvalResult (List t)
        go queue ( vacc, tacc, lacc ) =
            case queue of
                [] ->
                    ( Ok <| List.reverse vacc, tacc, lacc )

                ( Err e, trees, logs ) :: _ ->
                    ( Err e, appendRopes tacc trees, appendRopes lacc logs )

                ( Ok v, trees, logs ) :: tail ->
                    go tail ( v :: vacc, appendRopes tacc trees, appendRopes lacc logs )
    in
    go ls ( [], Rope.empty, Rope.empty )
