module Kernel.List exposing (sortBy, sortWith)

import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Pattern exposing (QualifiedNameRef)
import Eval.PartialResult as PartialResult
import Eval.Types as Types exposing (Eval, Eval2)
import Kernel.Utils
import Value exposing (Value)


sortBy : Eval2 (Eval Value Value) (List Value) (List Value)
sortBy cfg env toComparable list =
    list
        |> PartialResult.mapCombine cfg
            env
            (\icfg ienv value ->
                Types.map
                    (Tuple.pair value)
                    (toComparable icfg ienv value)
            )
        |> Tuple.mapFirst
            (Result.map
                (List.sortWith
                    (\( _, lc ) ( _, rc ) ->
                        case Kernel.Utils.compare env lc rc of
                            Err e ->
                                handleErr e

                            Ok res ->
                                res
                    )
                    >> List.map Tuple.first
                )
            )


sortWith : Eval2 (Eval Value (Eval Value Order)) (List Value) (List Value)
sortWith cfg env compare list =
    ( list
        |> List.sortWith
            (\lv rv ->
                case compare cfg env lv of
                    ( Err e, _ ) ->
                        handleErr e

                    ( Ok k, _ ) ->
                        case k cfg env rv of
                            ( Err e, _ ) ->
                                handleErr e

                            ( Ok res, _ ) ->
                                res
            )
        |> Ok
    , let
        _ =
            Debug.todo
      in
      []
    )


handleErr : { currentModule : ModuleName, callStack : List QualifiedNameRef, error : Value.EvalErrorKind } -> Order
handleErr err =
    let
        _ =
            Debug.log "handleErr" err
    in
    -- TODO: find out how to deal with errors
    Debug.todo "handleErr"
