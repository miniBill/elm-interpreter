module Kernel.Utils exposing (append, compare, comparison)

import Eval.Types exposing (Eval)
import Value exposing (Env, EvalResult, Value(..), typeError)


append : Value -> Value -> Eval Value
append l r _ env =
    case ( l, r ) of
        ( String ls, String rs ) ->
            ( Ok <| String (ls ++ rs), [] )

        ( List ll, List rl ) ->
            ( Ok <| List (ll ++ rl), [] )

        _ ->
            ( Err <| typeError env <| "Cannot append " ++ Value.toString l ++ " and " ++ Value.toString r
            , []
            )


compare : Value -> Value -> Env -> EvalResult Order
compare l r env =
    let
        inner : comparable -> comparable -> EvalResult Order
        inner lv rv =
            Ok <| Basics.compare lv rv
    in
    case ( l, r ) of
        -- TODO: Implement all cases
        ( Int lv, Int rv ) ->
            inner lv rv

        ( Float lv, Float rv ) ->
            inner lv rv

        ( Int lv, Float rv ) ->
            inner (toFloat lv) rv

        ( Float lv, Int rv ) ->
            inner lv (toFloat rv)

        ( String lv, String rv ) ->
            inner lv rv

        ( Char lv, Char rv ) ->
            inner lv rv

        ( Tuple la lb, Tuple ra rb ) ->
            compare la ra env
                |> Result.andThen
                    (\a ->
                        if a /= EQ then
                            Ok a

                        else
                            compare lb rb env
                    )

        ( Triple la lb lc, Triple ra rb rc ) ->
            compare la ra env
                |> Result.andThen
                    (\a ->
                        if a /= EQ then
                            Ok a

                        else
                            compare lb rb env
                                |> Result.andThen
                                    (\b ->
                                        if b /= EQ then
                                            Ok b

                                        else
                                            compare lc rc env
                                    )
                    )

        ( List [], List (_ :: _) ) ->
            Ok LT

        ( List (_ :: _), List [] ) ->
            Ok GT

        ( List [], List [] ) ->
            Ok EQ

        ( List (lh :: lt), List (rh :: rt) ) ->
            compare lh rh env
                |> Result.andThen
                    (\h ->
                        if h /= EQ then
                            Ok h

                        else
                            compare (List lt) (List rt) env
                    )

        _ ->
            case ( Value.toArray l, Value.toArray r ) of
                ( Just la, Just ra ) ->
                    compare (List la) (List ra) env

                _ ->
                    Err <|
                        typeError env <|
                            "Comparison not yet implemented for "
                                ++ Value.toString l
                                ++ " and "
                                ++ Value.toString r


comparison : List Order -> ( Int, List Value -> Eval Value )
comparison orders =
    ( 2
    , \args _ env ->
        case args of
            [ l, r ] ->
                ( Result.map (\result -> Bool (List.member result orders)) <| compare l r env
                , []
                )

            _ ->
                ( Err <| typeError env "Comparison needs exactly two arguments", [] )
    )
