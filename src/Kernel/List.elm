module Kernel.List exposing (sortBy, sortWith)

import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Pattern exposing (QualifiedNameRef)
import Kernel.Utils
import Result.Extra
import Value exposing (Env, EvalResult, Value)


sortBy : Env -> (Value -> EvalResult Value) -> List Value -> EvalResult (List Value)
sortBy env toComparable list =
    list
        |> Result.Extra.combineMap
            (\value -> Result.map (Tuple.pair value) (toComparable value))
        |> Result.map
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


sortWith : Env -> (Value -> EvalResult (Value -> EvalResult Order)) -> List Value -> EvalResult (List Value)
sortWith _ compare list =
    list
        |> List.sortWith
            (\lv rv ->
                case compare lv of
                    Err e ->
                        handleErr e

                    Ok k ->
                        case k rv of
                            Err e ->
                                handleErr e

                            Ok res ->
                                res
            )
        |> Ok


handleErr : { currentModule : ModuleName, callStack : List QualifiedNameRef, error : Value.EvalError } -> Order
handleErr err =
    let
        _ =
            Debug.log "handleErr" err
    in
    -- TODO: find out how to deal with errors
    Debug.todo "handleErr"
