module Kernel.List exposing (foldl, foldr, sortBy, sortWith)

import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Pattern exposing (QualifiedNameRef)
import Eval.Types as Types exposing (Eval)
import Kernel.Utils
import Rope
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
        |> Types.map
            (List.sortWith
                (\( _, lc ) ( _, rc ) ->
                    case Kernel.Utils.innerCompare lc rc env of
                        Err e ->
                            handleErr e

                        Ok res ->
                            res
                )
                >> List.map Tuple.first
            )


sortWith : (Value -> Eval (Value -> Eval Order)) -> List Value -> Eval (List Value)
sortWith compare list cfg env =
    ( list
        |> List.sortWith
            (\lv rv ->
                case Types.toResult (compare lv cfg env) of
                    Err e ->
                        handleErr e

                    Ok k ->
                        case Types.toResult (k rv cfg env) of
                            Err e ->
                                handleErr e

                            Ok res ->
                                res
            )
        |> Ok
    , let
        _ =
            Debug.todo
      in
      Rope.empty
    , let
        _ =
            Debug.todo
      in
      Rope.empty
    )


handleErr : { currentModule : ModuleName, callStack : List QualifiedNameRef, error : Value.EvalErrorKind } -> Order
handleErr err =
    let
        _ =
            Debug.log "handleErr" err
    in
    -- TODO: find out how to deal with errors
    Debug.todo "handleErr"


foldr : (Value -> Eval (Value -> Eval Value)) -> Value -> List Value -> Eval Value
foldr f i xs cfg env =
    Types.foldr (\el acc c e -> Types.andThen (\fe -> fe acc c e) (f el c e)) i xs cfg env


foldl : (Value -> Eval (Value -> Eval Value)) -> Value -> List Value -> Eval Value
foldl f i xs cfg env =
    Types.foldl (\el acc c e -> Types.andThen (\fe -> fe acc c e) (f el c e)) i xs cfg env
