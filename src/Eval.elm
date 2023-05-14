module Eval exposing (Error, eval)

import Elm.Parser
import Parser exposing (DeadEnd)
import Value exposing (Value)


type Error
    = ParsingError (List DeadEnd)


eval : String -> Result Error Value
eval expression =
    Debug.todo "Eval.eval"
