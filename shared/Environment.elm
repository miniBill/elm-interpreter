module Environment exposing (call, empty, with, withExpr, withValue)

import Elm.Syntax.Pattern exposing (QualifiedNameRef)
import FastDict as Dict exposing (Dict)
import Types exposing (Env, Expr, ExprOrValue(..), Value)


withExpr : String -> Expr -> Env -> Env
withExpr name value env =
    { env
        | values = Dict.insert name (Expr value) env.values
    }


withValue : String -> Value -> Env -> Env
withValue name value env =
    { env
        | values = Dict.insert name (Value value) env.values
    }


with : Dict String ExprOrValue -> Env -> Env
with newValues old =
    { old | values = Dict.union newValues old.values }


empty : Env
empty =
    { callStack = []
    , values = Dict.empty
    }


call : QualifiedNameRef -> Env -> Env
call fqName env =
    { env | callStack = fqName :: env.callStack }
