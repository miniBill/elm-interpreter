module Environment exposing (addFunction, addValue, call, empty, with)

import Elm.Syntax.Expression exposing (FunctionImplementation)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node
import FastDict as Dict
import Types exposing (Env, EnvValues, ExprOrValue(..), Value)


addValue : String -> Value -> Env -> Env
addValue name value env =
    { env
        | values = Dict.insert name (Value value) env.values
    }


addFunction : ModuleName -> FunctionImplementation -> Env -> Env
addFunction moduleName function env =
    { env
        | values =
            Dict.insert
                { moduleName = moduleName, name = Node.value function.name }
                (functionToExpression function)
                env.values
    }


functionToExpression : FunctionImplementation -> a
functionToExpression arg1 =
    Debug.todo "TODO"


with : EnvValues -> Env -> Env
with newValues old =
    { old | values = Dict.union newValues old.values }


empty : Env
empty =
    { callStack = []
    , values = Dict.empty
    }


call : ModuleName -> String -> Env -> Env
call moduleName name env =
    { env
        | callStack =
            { moduleName = moduleName, name = name }
                :: env.callStack
    }
