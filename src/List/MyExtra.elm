module List.MyExtra exposing (groupBy)

import List.Extra


groupBy : (a -> b) -> List a -> List ( b, List a )
groupBy f list =
    list
        |> List.Extra.groupWhile (\l r -> f l == f r)
        |> List.map (\( head, tail ) -> ( f head, head :: tail ))
