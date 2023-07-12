module Types exposing
    ( BinOp
    , CallTree(..)
    , Env
    , EnvValues
    , Error(..)
    , Eval
    , EvalConfig
    , EvalErrorData
    , EvalErrorKind(..)
    , EvalResult
    , ExprOrValue(..)
    , ModuleName
    , PartialEval
    , PartialResult
    , Value(..)
    )

import Array exposing (Array)
import Elm.CodeGen
import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.Node exposing (Node)
import Elm.Syntax.Pattern exposing (Pattern, QualifiedNameRef)
import FastDict exposing (Dict)
import Parser exposing (DeadEnd)
import Recursion exposing (Rec)
import Rope exposing (Rope)


type alias BinOp =
    Elm.CodeGen.BinOp


type alias ModuleName =
    List String


type ExprOrValue
    = Expr Env (Node Expression)
    | Value Value


type Value
    = String String
    | Int Int
    | Float Float
    | Char Char
    | Bool Bool
    | Unit
    | Lambda Env Pattern (Node Expression)
      -- Containers
    | Tuple (List Value)
    | Record (Dict String Value)
    | Custom QualifiedNameRef (List Value)
    | List (List Value)
    | JsArray (Array Value)


type alias Env =
    { callStack : List QualifiedNameRef
    , values : EnvValues
    }


type alias EnvValues =
    Dict QualifiedNameRef ExprOrValue


type alias Eval out =
    EvalConfig -> Env -> EvalResult out


type alias PartialEval out =
    EvalConfig -> Env -> PartialResult out


type alias PartialResult out =
    Rec
        ( Node Expression, EvalConfig, Env )
        (EvalResult out)
        (EvalResult out)


type alias EvalResult out =
    ( Result EvalErrorData out
    , Rope CallTree
    , Rope String
    )


type CallTree
    = CallNode
        { expression : Expression
        , result : Result EvalErrorData Value
        , children : Rope CallTree
        }


type alias EvalConfig =
    { trace : Bool
    }


type Error
    = ParsingError (List DeadEnd)
    | EvalError EvalErrorData


type alias EvalErrorData =
    { callStack : List QualifiedNameRef
    , error : EvalErrorKind
    }


type EvalErrorKind
    = TypeError String
    | Unsupported String
    | NameError String
    | Todo String
