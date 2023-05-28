module Eval.Types exposing (CallTree(..), CallTreeContinuation, Config, Error(..), Eval, Eval2, Eval3, Eval4, PartialEval, PartialEval2, PartialEval3, PartialResult(..), map)

import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.Node exposing (Node)
import Elm.Syntax.Pattern exposing (QualifiedNameRef)
import Parser exposing (DeadEnd)
import Value exposing (Env, EvalError, EvalResult, Value)


type alias PartialEval a =
    Config -> Env -> a -> PartialResult


type alias PartialEval2 a b =
    Config -> Env -> a -> b -> PartialResult


type alias PartialEval3 a b c =
    Config -> Env -> a -> b -> c -> PartialResult


type alias Eval a out =
    Config -> Env -> a -> ( EvalResult out, List CallTree )


type alias Eval2 a b out =
    Config -> Env -> a -> b -> ( EvalResult out, List CallTree )


type alias Eval3 a b c out =
    Config -> Env -> a -> b -> c -> ( EvalResult out, List CallTree )


type alias Eval4 a b c d out =
    Config -> Env -> a -> b -> c -> d -> ( EvalResult out, List CallTree )


map : (a -> b) -> ( EvalResult a, List CallTree ) -> ( EvalResult b, List CallTree )
map f ( x, y ) =
    ( Result.map f x, y )


type alias Config =
    { trace : Bool
    , callTreeContinuation : CallTreeContinuation
    }


type alias CallTreeContinuation =
    List CallTree -> EvalResult Value -> CallTree


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
