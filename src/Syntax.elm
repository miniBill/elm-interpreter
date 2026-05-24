module Syntax exposing (qualifiedNameToString)

import Elm.Syntax.Pattern exposing (QualifiedNameRef)


qualifiedNameToString : QualifiedNameRef -> String
qualifiedNameToString { moduleName, name } =
    (moduleName ++ [ name ])
        |> String.join "."
