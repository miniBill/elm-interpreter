module Kernel.Utils exposing (append, compare, comparison, innerCompare)

import Elm.Syntax.ModuleName exposing (ModuleName)
import Eval.Types as Types exposing (Eval)
import Value exposing (EvalError, Value(..), typeError)


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
compare l r _ env =
    Types.fromResult (innerCompare l r env)


innerCompare : Value -> Value -> Value.Env -> Result EvalError Order
innerCompare l r env =
    let
        inner : comparable -> comparable -> Result EvalError Order
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
            innerCompare la ra env
                |> Result.andThen
                    (\a ->
                        if a /= EQ then
                            Ok a

                        else
                            innerCompare lb rb env
                    )

        ( Triple la lb lc, Triple ra rb rc ) ->
            innerCompare la ra env
                |> Result.andThen
                    (\a ->
                        if a /= EQ then
                            Ok a

                        else
                            innerCompare lb rb env
                                |> Result.andThen
                                    (\b ->
                                        if b /= EQ then
                                            Ok b

                                        else
                                            innerCompare lc rc env
                                    )
                    )

        ( List [], List (_ :: _) ) ->
            Ok LT

        ( List (_ :: _), List [] ) ->
            Ok GT

        ( List [], List [] ) ->
            Ok EQ

        ( List (lh :: lt), List (rh :: rt) ) ->
            innerCompare lh rh env
                |> Result.andThen
                    (\h ->
                        if h /= EQ then
                            Ok h

                        else
                            innerCompare (List lt) (List rt) env
                    )

        _ ->
            case ( Value.toArray l, Value.toArray r ) of
                ( Just la, Just ra ) ->
                    innerCompare (List la) (List ra) env

                _ ->
                    Err <|
                        typeError env <|
                            "Comparison not yet implemented for "
                                ++ Value.toString l
                                ++ " and "
                                ++ Value.toString r


comparison : List Order -> ModuleName -> ( Int, List Value -> Eval Value )
comparison orders _ =
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
