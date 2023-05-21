module Result.MyExtra exposing (combineFoldl)


combineFoldl : (a -> b -> Result error b) -> Result error b -> List a -> Result error b
combineFoldl f init list =
    case init of
        Err e ->
            Err e

        Ok i ->
            let
                combineFoldlHelper : b -> List a -> Result error b
                combineFoldlHelper acc tail =
                    case tail of
                        [] ->
                            Ok acc

                        x :: xs ->
                            case f x acc of
                                Err e ->
                                    Err e

                                Ok y ->
                                    combineFoldlHelper y xs
            in
            combineFoldlHelper i list
