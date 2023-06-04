module Eval.Log exposing (Continuation(..), Line)

import Elm.Syntax.Pattern exposing (QualifiedNameRef)
import FastDict exposing (Dict)
import Rope exposing (Rope)
import Value exposing (Value)


type Continuation
    = Done
    | AppendTo (Rope Line) Continuation
    | Prepend Line Continuation
    | Append Line Continuation


type alias Line =
    { stack : List QualifiedNameRef
    , message : String
    , env : Dict String Value
    }
