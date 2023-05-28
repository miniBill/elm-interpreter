module Kernel.List exposing (sortBy, sortWith)

import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Pattern exposing (QualifiedNameRef)
import Eval.Types as Types exposing (Eval)
import Kernel.Utils
import Value exposing (Value)


sortBy : (Value -> Eval Value) -> List Value -> Eval (List Value)
sortBy toComparable list cfg env =
    Types.combineMap
        (\value icfg ienv ->
            Types.map
                (Tuple.pair value)
                (toComparable value icfg ienv)
        )
        list
        cfg
        env
        |> Tuple.mapFirst
            (Result.map
                (List.sortWith
                    (\( _, lc ) ( _, rc ) ->
                        case Kernel.Utils.compare lc rc env of
                            Err e ->
                                handleErr e

                            Ok res ->
                                res
                    )
                    >> List.map Tuple.first
                )
            )


sortWith : (Value -> Eval (Value -> Eval Order)) -> List Value -> Eval (List Value)
sortWith compare list cfg env =
    ( list
        |> List.sortWith
            (\lv rv ->
                case compare lv cfg env of
                    ( Err e, _ ) ->
                        handleErr e

                    ( Ok k, _ ) ->
                        case k rv cfg env of
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
