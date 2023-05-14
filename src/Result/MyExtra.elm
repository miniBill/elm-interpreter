module Result.MyExtra exposing (combineFoldl)


combineFoldl : (a -> b -> Result error b) -> b -> List a -> Result error b
combineFoldl f init list =
    List.foldl
        (\e acc ->
            case acc of
                Err _ ->
                    acc

                Ok a ->
                    f e a
        )
        (Ok init)
        list
