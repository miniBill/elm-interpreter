module Core.List exposing (functions)

import Elm.Syntax.Expression exposing (Expression(..), FunctionImplementation)
import Elm.Syntax.Pattern exposing (Pattern(..))
import Syntax exposing (fakeNode)


functions : List FunctionImplementation
functions =
    [ { name = fakeNode "List.isEmpty"
      , arguments = [ fakeNode (VarPattern "list") ]
      , expression =
            fakeNode
                (CaseExpression
                    { expression = fakeNode (FunctionOrValue [] "list")
                    , cases =
                        [ ( fakeNode (ListPattern []), fakeNode (FunctionOrValue [] "True") )
                        , ( fakeNode AllPattern, fakeNode (FunctionOrValue [] "False") )
                        ]
                    }
                )
      }
    ]
