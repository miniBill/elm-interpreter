module Kernel.Utils exposing (append, compare, comparison)

import Eval.Types as Types exposing (Eval, EvalResult)
import Value exposing (Value(..), typeError)


append : Value -> Value -> Eval Value
append l r _ env =
    case ( l, r ) of
        ( String ls, String rs ) ->
            Types.succeed <| String (ls ++ rs)

        ( List ll, List rl ) ->
            Types.succeed <| List (ll ++ rl)

        _ ->
            Types.fail <| typeError env <| "Cannot append " ++ Value.toString l ++ " and " ++ Value.toString r


compare : Value -> Value -> Eval Order
compare l r cfg env =
    let
        inner : comparable -> comparable -> EvalResult Order
        inner lv rv =
            Types.succeed <| Basics.compare lv rv
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
            compare la ra cfg env
                |> Types.andThen
                    (\a ->
                        if a /= EQ then
                            Types.succeed a

                        else
                            compare lb rb cfg env
                    )

        ( Triple la lb lc, Triple ra rb rc ) ->
            compare la ra cfg env
                |> Types.andThen
                    (\a ->
                        if a /= EQ then
                            Types.succeed a

                        else
                            compare lb rb cfg env
                                |> Types.andThen
                                    (\b ->
                                        if b /= EQ then
                                            Types.succeed b

                                        else
                                            compare lc rc cfg env
                                    )
                    )

        ( List [], List (_ :: _) ) ->
            Types.succeed LT

        ( List (_ :: _), List [] ) ->
            Types.succeed GT

        ( List [], List [] ) ->
            Types.succeed EQ

        ( List (lh :: lt), List (rh :: rt) ) ->
            compare lh rh cfg env
                |> Types.andThen
                    (\h ->
                        if h /= EQ then
                            Types.succeed h

                        else
                            compare (List lt) (List rt) cfg env
                    )

        _ ->
            case ( Value.toArray l, Value.toArray r ) of
                ( Just la, Just ra ) ->
                    compare (List la) (List ra) cfg env

                _ ->
                    Types.fail <|
                        typeError env <|
                            "Comparison not yet implemented for "
                                ++ Value.toString l
                                ++ " and "
                                ++ Value.toString r


comparison : List Order -> ( Int, List Value -> Eval Value )
comparison orders =
    ( 2
    , \args cfg env ->
        case args of
            [ l, r ] ->
                compare l r cfg env
                    |> Types.map
                        (\result ->
                            Bool (List.member result orders)
                        )

            _ ->
                Types.fail <| typeError env "Comparison needs exactly two arguments"
    )
