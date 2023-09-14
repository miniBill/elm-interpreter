module Expression.Extra exposing (toString)

import Elm.Pretty
import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.Node exposing (Node(..))
import Pretty


toString : Node Expression -> String
toString (Node _ expression) =
    expression
        |> Elm.Pretty.prettyExpression
        |> Pretty.pretty 120
