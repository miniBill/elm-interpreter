module Syntax exposing (fakeNode, qualifiedNameToString)

import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (QualifiedNameRef)
import Elm.Syntax.Range exposing (Location, Range)


fakeNode : a -> Node a
fakeNode value =
    Node fakeRange value


fakeRange : Range
fakeRange =
    { start = fakeLocation, end = fakeLocation }


fakeLocation : Location
fakeLocation =
    { row = -1
    , column = -1
    }


qualifiedNameToString : QualifiedNameRef -> String
qualifiedNameToString { moduleName, name } =
    (moduleName ++ [ name ])
        |> String.join "."
