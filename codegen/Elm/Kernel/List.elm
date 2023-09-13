module Elm.Kernel.List exposing (map2, map3, map4, map5, sortBy)


map2 : (a -> b -> c) -> List a -> List b -> List c
map2 f xas xbs =
    let
        go : List a -> List b -> List c -> List c
        go ao bo acc =
            case ao of
                [] ->
                    List.reverse acc

                ah :: at ->
                    case bo of
                        [] ->
                            List.reverse acc

                        bh :: bt ->
                            go at bt (f ah bh :: acc)
    in
    go xas xbs []


map3 : (a -> b -> c -> d) -> List a -> List b -> List c -> List d
map3 f xas xbs xcs =
    let
        go : List a -> List b -> List c -> List d -> List d
        go ao bo co acc =
            case ao of
                [] ->
                    List.reverse acc

                ah :: at ->
                    case bo of
                        [] ->
                            List.reverse acc

                        bh :: bt ->
                            case co of
                                [] ->
                                    List.reverse acc

                                ch :: ct ->
                                    go at bt ct (f ah bh ch :: acc)
    in
    go xas xbs xcs []


map4 : (a -> b -> c -> d -> e) -> List a -> List b -> List c -> List d -> List e
map4 f xas xbs xcs xds =
    let
        go : List a -> List b -> List c -> List d -> List e -> List e
        go ao bo co do acc =
            case ao of
                [] ->
                    List.reverse acc

                ah :: at ->
                    case bo of
                        [] ->
                            List.reverse acc

                        bh :: bt ->
                            case co of
                                [] ->
                                    List.reverse acc

                                ch :: ct ->
                                    case do of
                                        [] ->
                                            List.reverse acc

                                        dh :: dt ->
                                            go at bt ct dt (f ah bh ch dh :: acc)
    in
    go xas xbs xcs xds []


map5 : (a -> b -> c -> d -> e -> f) -> List a -> List b -> List c -> List d -> List e -> List f
map5 f xas xbs xcs xds xes =
    let
        go : List a -> List b -> List c -> List d -> List e -> List f -> List f
        go ao bo co do eo acc =
            case ao of
                [] ->
                    List.reverse acc

                ah :: at ->
                    case bo of
                        [] ->
                            List.reverse acc

                        bh :: bt ->
                            case co of
                                [] ->
                                    List.reverse acc

                                ch :: ct ->
                                    case do of
                                        [] ->
                                            List.reverse acc

                                        dh :: dt ->
                                            case eo of
                                                [] ->
                                                    List.reverse acc

                                                eh :: et ->
                                                    go at bt ct dt et (f ah bh ch dh eh :: acc)
    in
    go xas xbs xcs xds xes []


sortBy : (a -> comparable) -> List a -> List a
sortBy f xs =
    sortWith (\l r -> compare (f l) (f r)) xs


sortWith : (a -> a -> Order) -> List a -> List a
sortWith f xs =
    case xs of
        [] ->
            xs

        [ _ ] ->
            xs

        _ ->
            let
                ( left, right ) =
                    split xs
            in
            mergeWith f (sortWith f left) (sortWith f right)


split : List a -> ( List a, List a )
split xs =
    let
        goLeft : List a -> List a -> List a -> ( List a, List a )
        goLeft l lacc racc =
            case l of
                [] ->
                    ( lacc, racc )

                lh :: lt ->
                    goRight lt (lh :: lacc) racc

        goRight : List a -> List a -> List a -> ( List a, List a )
        goRight l lacc racc =
            case l of
                [] ->
                    ( lacc, racc )

                lh :: lt ->
                    goLeft lt lacc (lh :: racc)
    in
    goLeft xs [] []


mergeWith : (a -> a -> Order) -> List a -> List a -> List a
mergeWith f ls rs =
    case ls of
        [] ->
            rs

        lh :: lt ->
            case rs of
                [] ->
                    ls

                rh :: rt ->
                    case f lh rh of
                        LT ->
                            lh :: mergeWith f lt rs

                        GT ->
                            rh :: mergeWith f ls rt

                        EQ ->
                            lh :: rh :: mergeWith f lt rt
