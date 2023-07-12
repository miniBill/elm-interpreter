module EvalResult exposing (andThen, combine, fail, fromResult, map, map2, onValue, succeed, toResult)

import Rope exposing (Rope)
import Types exposing (CallTree, EvalErrorData, EvalResult)


succeed : a -> EvalResult a
succeed x =
    fromResult <| Ok x


fail : EvalErrorData -> EvalResult a
fail e =
    fromResult <| Err e


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
            , Rope.appendTo callTrees fxCallTrees
            , Rope.appendTo logs fxLogs
            )


map2 : (a -> b -> out) -> EvalResult a -> EvalResult b -> EvalResult out
map2 f ( lv, lc, ll ) ( rv, rc, rl ) =
    ( Result.map2 f lv rv
    , Rope.appendTo lc rc
    , Rope.appendTo ll rl
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
                    ( Err e, Rope.appendTo tacc trees, Rope.appendTo lacc logs )

                ( Ok v, trees, logs ) :: tail ->
                    go tail ( v :: vacc, Rope.appendTo tacc trees, Rope.appendTo lacc logs )
    in
    go ls ( [], Rope.empty, Rope.empty )
