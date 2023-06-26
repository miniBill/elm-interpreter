module Environment exposing (call, empty, with, withValue)

import Elm.Syntax.Pattern exposing (QualifiedNameRef)
import FastDict as Dict exposing (Dict)
import Types exposing (Env, Expr)


withValue : String -> Expr -> Env -> Env
withValue name value env =
    { env
        | values = Dict.insert name value env.values
    }


with : Dict String Expr -> Env -> Env
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
