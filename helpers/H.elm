module H exposing (node, node1, val)

import Elm.Syntax.Expression
import Elm.Syntax.Node exposing (Node(..))


node : Int -> Int -> Int -> Int -> a -> Node a
node fr fc tr tc value =
    Node
        { start = { row = fr, column = fc }
        , end = { row = tr, column = tc }
        }
        value


node1 : Int -> Int -> Int -> a -> Node a
node1 fr fc tc value =
    Node
        { start = { row = fr, column = fc }
        , end = { row = fr, column = tc }
        }
        value


val : String -> Elm.Syntax.Expression.Expression
val name =
    Elm.Syntax.Expression.FunctionOrValue [] name
