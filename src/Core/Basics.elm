module Core.Basics exposing (env)

import Core.List
import Elm.Syntax.Expression exposing (FunctionImplementation)
import Elm.Syntax.Node as Node
import Env exposing (Env)
import FastDict as Dict


env : Env
env =
    { values = Dict.empty
    , functions =
        functions
            |> List.map (\fun -> ( Node.value fun.name, fun ))
            |> Dict.fromList
    }


functions : List FunctionImplementation
functions =
    basics
        ++ Core.List.functions
        ++ maybe
        ++ result
        ++ string
        ++ char
        ++ tuple
        ++ debug


basics : List FunctionImplementation
basics =
    []


maybe : List FunctionImplementation
maybe =
    []


result : List FunctionImplementation
result =
    []


string : List FunctionImplementation
string =
    []


char : List FunctionImplementation
char =
    []


tuple : List FunctionImplementation
tuple =
    []


debug : List FunctionImplementation
debug =
    []
