module Eval.Types exposing (CallTree(..), CallTreeContinuation, Config, Error(..), Eval, PartialEval, PartialResult(..), combineMap, map)

import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.Node exposing (Node)
import Elm.Syntax.Pattern exposing (QualifiedNameRef)
import Parser exposing (DeadEnd)
import Value exposing (Env, EvalError, EvalResult, Value)


type alias PartialEval =
    Config -> Env -> PartialResult


type alias Eval out =
    Config -> Env -> ( EvalResult out, List CallTree )


map : (a -> b) -> ( EvalResult a, List CallTree ) -> ( EvalResult b, List CallTree )
map f ( x, y ) =
    ( Result.map f x, y )


combineMap : (a -> Eval b) -> List a -> Eval (List b)
combineMap f xs cfg env =
    List.foldr
        (\el ( lacc, callTrees ) ->
            case lacc of
                Err _ ->
                    ( lacc, callTrees )

                Ok acc ->
                    let
                        ( g, callTree ) =
                            f el cfg env
                    in
                    ( Result.map (\h -> h :: acc) g
                    , callTree ++ callTrees
                    )
        )
        ( Ok [], [] )
        xs


type alias Config =
    { trace : Bool
    , callTreeContinuation : CallTreeContinuation
    }


type alias CallTreeContinuation =
    List CallTree -> EvalResult Value -> List CallTree


type CallTree
    = CallNode
        String
        QualifiedNameRef
        { args : List Value
        , result : EvalResult Value
        , children : List CallTree
        }


type Error
    = ParsingError (List DeadEnd)
    | EvalError EvalError


{-| Represent the result of a computation inside one of the branches of `evalExpression`.

This is needed because to get TCO we need to return an expression, rather than calling `evalExpression` recursively.

-}
type PartialResult
    = PartialExpression Env (Node Expression) CallTreeContinuation
    | PartialValue (List CallTree) Value
    | PartialErr (List CallTree) EvalError
