module Value exposing (Value(..))

import Dict exposing (Dict)


type Value
    = String String
    | Int Int
    | Float Float
    | Unit
    | Tuple Value Value
    | Triple Value Value Value
    | Record (Dict String Value)
    | Custom String (List Value)
