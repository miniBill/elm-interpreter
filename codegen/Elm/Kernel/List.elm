module Elm.Kernel.List exposing (map2, map3)


map2 : (a -> b -> c) -> List a -> List b -> List c
map2 f ls rs =
    let
        go lo ro acc =
            case lo of
                [] ->
                    List.reverse acc

                lh :: lt ->
                    case ro of
                        [] ->
                            List.reverse acc

                        rh :: rt ->
                            go lt rt (f lh rh :: acc)
    in
    go ls rs []


map3 : (a -> b -> c -> d) -> List a -> List b -> List c -> List d
map3 f ls ms rs =
    let
        go lo mo ro acc =
            case lo of
                [] ->
                    List.reverse acc

                lh :: lt ->
                    case mo of
                        [] ->
                            List.reverse acc

                        mh :: mt ->
                            case ro of
                                [] ->
                                    List.reverse acc

                                rh :: rt ->
                                    go lt mt rt (f lh mh rh :: acc)
    in
    go ls ms rs []
