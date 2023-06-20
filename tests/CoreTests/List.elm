module CoreTests.List exposing (suite)

import Test exposing (Test, describe)
import TestUtils exposing (evalTest, list, maybe, tuple)
import Value exposing (Value(..))


suite : Test
suite =
    describe "List Tests"
        [ testListOfN 0
        , testListOfN 1
        , testListOfN 2
        , testListOfN 5000
        ]


evalTestN : Int -> String -> String -> (e -> Value) -> e -> Test
evalTestN n description code toValue value =
    evalTest description
        ("""
let
    n : Int
    n =
        """ ++ String.fromInt n ++ """

    xs : List Int
    xs =
        List.range 1 n

    xsOpp : List Int
    xsOpp =
        List.range -n -1

    xsNeg : List Int
    xsNeg =
        List.foldl (::) [] xsOpp

    -- assume foldl and (::) work
    zs : List Int
    zs =
        List.range 0 n

    mid : Int
    mid =
        n // 2
in
""" ++ code)
        toValue
        value


testListOfN : Int -> Test
testListOfN n =
    let
        xs : List Int
        xs =
            List.range 1 n

        xsOpp : List Int
        xsOpp =
            List.range -n -1

        xsNeg : List Int
        xsNeg =
            List.foldl (::) [] xsOpp

        -- assume foldl and (::) work
        zs : List Int
        zs =
            List.range 0 n

        mid : Int
        mid =
            n // 2
    in
    describe (String.fromInt n ++ " elements")
        [ describe "foldl"
            [ evalTestN n
                "order"
                "List.foldl (\\x _ -> x) 0 xs"
                Int
                (List.foldl (\x _ -> x) 0 xs)
            , evalTestN n
                "total"
                "List.foldl (+) 0 xs"
                Int
                (List.foldl (+) 0 xs)
            ]
        , describe "foldr"
            [ evalTestN n
                "order"
                "List.foldr (\\x _ -> x) 0 xs"
                Int
                (List.foldr (\x _ -> x) 0 xs)
            , evalTestN n
                "total"
                "List.foldl (+) 0 xs"
                Int
                (List.foldl (+) 0 xs)
            ]
        , describe "map"
            [ evalTestN n
                "identity"
                "List.map identity xs"
                (list Int)
                (List.map identity xs)
            , evalTestN n
                "linear"
                "List.map ((+) 1) xs"
                (list Int)
                (List.map ((+) 1) xs)
            ]
        , evalTestN n
            "isEmpty"
            "List.isEmpty xs"
            Bool
            (List.isEmpty xs)
        , evalTestN n
            "length"
            "List.length xs"
            Int
            (List.length xs)
        , evalTestN n
            "reverse"
            "List.reverse xsNeg"
            (list Int)
            (List.reverse xsNeg)
        , describe "member"
            [ evalTestN n
                "positive"
                "List.member n zs"
                Bool
                (List.member n zs)
            , evalTestN n
                "negative"
                "List.member (n + 1) xs"
                Bool
                (List.member (n + 1) xs)
            ]
        , evalTestN n
            "head"
            "List.head xs"
            (maybe Int)
            (List.head xs)
        , describe "List.filter"
            [ evalTestN n
                "none"
                "List.filter (\\x -> x > n) xs"
                (list Int)
                (List.filter (\x -> x > n) xs)
            , evalTestN n
                "one"
                "List.filter (\\z -> z == n) zs"
                (list Int)
                (List.filter (\z -> z == n) zs)
            , evalTestN n
                "all"
                "List.filter (\\x -> x <= n) xs"
                (list Int)
                (List.filter (\x -> x <= n) xs)
            ]
        , describe "take"
            [ evalTestN n
                "none"
                "List.take 0 xs"
                (list Int)
                (List.take 0 xs)
            , evalTestN n
                "some"
                "List.take n zs"
                (list Int)
                (List.take n zs)
            , evalTestN n
                "all"
                "List.take n xs"
                (list Int)
                (List.take n xs)
            , evalTestN n
                "all+"
                "List.take (n + 1) xs"
                (list Int)
                (List.take (n + 1) xs)
            ]
        , describe "drop"
            [ evalTestN n
                "none"
                "List.drop 0 xs"
                (list Int)
                (List.drop 0 xs)
            , evalTestN n
                "some"
                "List.drop n zs"
                (list Int)
                (List.drop n zs)
            , evalTestN n
                "all"
                "List.drop n xs"
                (list Int)
                (List.drop n xs)
            , evalTestN n
                "all+"
                "List.drop (n + 1) xs"
                (list Int)
                (List.drop (n + 1) xs)
            ]
        , evalTestN n
            "repeat"
            "List.repeat n -1"
            (list Int)
            (List.repeat n -1)
        , evalTestN n
            "append"
            "List.append xs xs |> List.foldl (+) 0"
            Int
            (List.append xs xs |> List.foldl (+) 0)
        , evalTestN n
            "(::)"
            "-1 :: xs"
            (list Int)
            (-1 :: xs)
        , evalTestN n
            "List.concat"
            "List.concat [ xs, zs, xs ]"
            (list Int)
            (List.concat [ xs, zs, xs ])
        , evalTestN n
            "intersperse"
            "List.intersperse -1 xs |> List.foldl (\\x ( c1, c2 ) -> ( c2, c1 + x )) ( 0, 0 )"
            (tuple Int Int)
            (List.intersperse -1 xs |> List.foldl (\x ( c1, c2 ) -> ( c2, c1 + x )) ( 0, 0 ))
        , describe "partition"
            [ evalTestN n
                "left"
                "List.partition (\\x -> x > 0) xs"
                (tuple (list Int) (list Int))
                (List.partition (\x -> x > 0) xs)
            , evalTestN n
                "right"
                "List.partition (\\x -> x < 0) xs"
                (tuple (list Int) (list Int))
                (List.partition (\x -> x < 0) xs)
            , evalTestN n
                "split"
                "List.partition (\\x -> x > mid) xs"
                (tuple (list Int) (list Int))
                (List.partition (\x -> x > mid) xs)
            ]
        , describe "map2"
            [ evalTestN n
                "same length"
                "List.map2 (+) xs xs"
                (list Int)
                (List.map2 (+) xs xs)
            , evalTestN n
                "long first"
                "List.map2 (+) zs xs"
                (list Int)
                (List.map2 (+) zs xs)
            , evalTestN n
                "short first"
                "List.map2 (+) xs zs"
                (list Int)
                (List.map2 (+) xs zs)
            ]
        , evalTestN n
            "unzip"
            "List.map (\\x -> ( -x, x )) xs |> List.unzip"
            (tuple (list Int) (list Int))
            (List.map (\x -> ( -x, x )) xs |> List.unzip)
        , describe "filterMap"
            [ evalTestN n
                "none"
                "List.filterMap (\\_ -> Nothing) xs"
                (list Int)
                (List.filterMap (\_ -> Nothing) xs)
            , evalTestN n
                "all"
                "List.filterMap (\\x -> Just -x) xs"
                (list Int)
                (List.filterMap (\x -> Just -x) xs)
            , let
                halve : Int -> Maybe Int
                halve x =
                    if modBy 2 x == 0 then
                        Just (x // 2)

                    else
                        Nothing
              in
              evalTestN n
                "some"
                "let halve x = if modBy 2 x == 0 then Just (x // 2) else Nothing in List.filterMap halve xs"
                (list Int)
                (List.filterMap halve xs)
            ]
        , describe "concatMap"
            [ evalTestN n
                "none"
                "List.concatMap (\\_ -> []) xs"
                (list Int)
                (List.concatMap (\_ -> []) xs)
            , evalTestN n
                "all"
                "List.concatMap (\\x -> [ -x ]) xs"
                (list Int)
                (List.concatMap (\x -> [ -x ]) xs)
            ]
        , evalTestN n
            "indexedMap"
            "List.indexedMap (\\i x -> ( i, -x )) xs"
            (list (tuple Int Int))
            (List.indexedMap (\i x -> ( i, -x )) xs)
        , evalTestN n
            "sum"
            "List.sum xs"
            Int
            (List.sum xs)
        , evalTestN n
            "product"
            "List.product zs"
            Int
            (List.product zs)
        , evalTestN n
            "maximum"
            "List.maximum xs"
            (maybe Int)
            (List.maximum xs)
        , evalTestN n
            "minimum"
            "List.minimum xs"
            (maybe Int)
            (List.minimum xs)
        , describe "all"
            [ evalTestN n
                "false"
                "List.all (\\z -> z < n) zs"
                Bool
                (List.all (\z -> z < n) zs)
            , evalTestN n
                "true"
                "List.all (\\x -> x <= n) xs"
                Bool
                (List.all (\x -> x <= n) xs)
            ]
        , describe "any"
            [ evalTestN n
                "false"
                "List.any (\\x -> x > n) xs"
                Bool
                (List.any (\x -> x > n) xs)
            , evalTestN n
                "true"
                "List.any (\\z -> z >= n) zs"
                Bool
                (List.any (\z -> z >= n) zs)
            ]
        , describe "sort"
            [ evalTestN n
                "sorted"
                "List.sort xs"
                (list Int)
                (List.sort xs)
            , evalTestN n
                "unsorted"
                "List.sort xsNeg"
                (list Int)
                (List.sort xsNeg)
            ]
        , describe "sortBy"
            [ evalTestN n
                "sorted"
                "List.sortBy negate xsNeg"
                (list Int)
                (List.sortBy negate xsNeg)
            , evalTestN n
                "unsorted"
                "List.sortBy negate xsOpp"
                (list Int)
                (List.sortBy negate xsOpp)
            ]
        , describe "sortWith"
            [ evalTestN n
                "sorted"
                "List.sortWith (\\x -> \\y -> compare y x) xsNeg"
                (list Int)
                (List.sortWith (\x -> \y -> compare y x) xsNeg)
            , evalTestN n
                "unsorted"
                "List.sortWith (\\x -> \\y -> compare y x) xsOpp"
                (list Int)
                (List.sortWith (\x -> \y -> compare y x) xsOpp)
            ]
        ]
