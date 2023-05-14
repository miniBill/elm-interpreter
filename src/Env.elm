module Env exposing (Env, addFunction, addValue, empty, with)

import Elm.Syntax.Expression exposing (FunctionImplementation)
import Elm.Syntax.Node as Node
import FastDict as Dict
import Value exposing (Value)


type alias Env =
    Value.Env


addValue : String -> Value -> Env -> Env
addValue name value env =
    { functions = env.functions
    , values = Dict.insert name value env.values
    }


addFunction : FunctionImplementation -> Env -> Env
addFunction function env =
    { functions = Dict.insert (Node.value function.name) function env.functions
    , values = env.values
    }


with : Env -> Env -> Env
with new old =
    { functions = Dict.union new.functions old.functions
    , values = Dict.union new.values old.values
    }


empty : Env
empty =
    { functions = Dict.empty
    , values = Dict.empty
    }
