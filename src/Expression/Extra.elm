module Expression.Extra exposing (toString)

import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.Node exposing (Node)
import Elm.Writer


toString : Node Expression -> String
toString expression =
    expression
        |> Elm.Writer.writeExpression
        |> Elm.Writer.write
