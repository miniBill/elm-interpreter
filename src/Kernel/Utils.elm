module Kernel.Utils exposing (append, compare, comparison)

import Array
import Elm.Syntax.ModuleName exposing (ModuleName)
import Eval.Types as Types exposing (Eval)
import FastDict as Dict exposing (Dict)
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

        uncomparable : () -> Result EvalError value
        uncomparable () =
            Err <|
                typeError env
                    ("Cannot compare "
                        ++ Value.toString l
                        ++ " and "
                        ++ Value.toString r
                        ++ " because they have different types"
                    )
    in
    case ( l, r ) of
        ( Int lv, Int rv ) ->
            inner lv rv

        ( Int lv, Float rv ) ->
            inner (toFloat lv) rv

        ( Int _, _ ) ->
            uncomparable ()

        ( Float lv, Float rv ) ->
            inner lv rv

        ( Float lv, Int rv ) ->
            inner lv (toFloat rv)

        ( Float _, _ ) ->
            uncomparable ()

        ( String lv, String rv ) ->
            inner lv rv

        ( String _, _ ) ->
            uncomparable ()

        ( Char lv, Char rv ) ->
            inner lv rv

        ( Char _, _ ) ->
            uncomparable ()

        ( Tuple la lb, Tuple ra rb ) ->
            innerCompare la ra env
                |> Result.andThen
                    (\a ->
                        if a /= EQ then
                            Ok a

                        else
                            innerCompare lb rb env
                    )

        ( Tuple _ _, _ ) ->
            uncomparable ()

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

        ( Triple _ _ _, _ ) ->
            uncomparable ()

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

        ( List _, _ ) ->
            uncomparable ()

        ( Custom lname lvalues, Custom rname rvalues ) ->
            if lname.moduleName /= rname.moduleName then
                inner lname.moduleName rname.moduleName

            else if lname.name /= rname.name then
                inner lname.name rname.name

            else
                case ( Value.toArray l, Value.toArray r ) of
                    ( Just la, Just ra ) ->
                        innerCompare (List la) (List ra) env

                    _ ->
                        innerCompare (List lvalues) (List rvalues) env

        ( Custom _ _, _ ) ->
            uncomparable ()

        ( Record ldict, Record rdict ) ->
            let
                toValue : Dict String Value -> Value
                toValue dict =
                    dict
                        |> Dict.toList
                        |> List.map (\( k, v ) -> Tuple (String k) v)
                        |> List
            in
            innerCompare (toValue ldict) (toValue rdict) env

        ( Record _, _ ) ->
            uncomparable ()

        ( JsArray larr, JsArray rarr ) ->
            innerCompare (List <| Array.toList larr) (List <| Array.toList rarr) env

        ( JsArray _, _ ) ->
            uncomparable ()

        ( Bool lb, Bool rb ) ->
            if lb == rb then
                Ok EQ

            else if lb then
                Ok LT

            else
                Ok GT

        ( Bool _, _ ) ->
            uncomparable ()

        ( Unit, Unit ) ->
            Ok EQ

        ( Unit, _ ) ->
            uncomparable ()

        ( PartiallyApplied _ _ _ _ _, PartiallyApplied _ _ _ _ _ ) ->
            Err <| typeError env "Cannot compare functions"

        ( PartiallyApplied _ _ _ _ _, _ ) ->
            uncomparable ()


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
