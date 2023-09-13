module Types exposing (CallTree(..), Config, Env, EnvValues, Error(..), Eval, EvalErrorData, EvalErrorKind(..), EvalResult, PartialEval, PartialResult, Value(..))

import Array exposing (Array)
import Elm.Syntax.Expression exposing (Expression, FunctionImplementation)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node exposing (Node)
import Elm.Syntax.Pattern exposing (Pattern, QualifiedNameRef)
import FastDict exposing (Dict)
import Parser exposing (DeadEnd)
import Recursion exposing (Rec)
import Rope exposing (Rope)


type alias PartialEval out =
    Config -> Env -> PartialResult out


type alias PartialResult out =
    Rec
        ( Node Expression, Config, Env )
        (EvalResult out)
        (EvalResult out)


type alias Eval out =
    Config -> Env -> EvalResult out


type alias EvalResult out =
    ( Result EvalErrorData out
    , Rope CallTree
    , Rope String
    )


type alias Config =
    { trace : Bool
    }


type CallTree
    = CallNode
        { expression : Node Expression
        , result : Result EvalErrorData Value
        , children : Rope CallTree
        , env : Env
        }


type Error
    = ParsingError (List DeadEnd)
    | EvalError EvalErrorData


type Value
    = String String
    | Int Int
    | Float Float
    | Char Char
    | Bool Bool
    | Unit
    | Tuple Value Value
    | Triple Value Value Value
    | Record (Dict String Value)
    | Custom QualifiedNameRef (List Value)
    | PartiallyApplied Env (List Value) (List (Node Pattern)) (Maybe QualifiedNameRef) (Node Expression)
    | JsArray (Array Value)
    | List (List Value)


type alias Env =
    { currentModule : ModuleName
    , functions : Dict ModuleName (Dict String FunctionImplementation)
    , values : EnvValues
    , callStack : List QualifiedNameRef
    }


type alias EnvValues =
    Dict String Value


type alias EvalErrorData =
    { currentModule : ModuleName
    , callStack : List QualifiedNameRef
    , error : EvalErrorKind
    }


type EvalErrorKind
    = TypeError String
    | Unsupported String
    | NameError String
    | Todo String
