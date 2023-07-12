module EvalResult exposing (andThen, map, map2, onValue)

import Rope
import Types exposing (EvalErrorData, EvalResult)


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
