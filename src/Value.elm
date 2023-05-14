module Value exposing (EvalError(..), Value(..))

import Dict exposing (Dict)


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
    | Custom String (List Value)
    | Lambda (Value -> Result EvalError Value)


type EvalError
    = TypeError String
    | Unsupported String
    | NameError String
