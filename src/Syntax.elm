module Syntax exposing (fakeNode)

import Elm.Syntax.Node exposing (Node(..))
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
