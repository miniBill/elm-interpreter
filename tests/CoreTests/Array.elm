module CoreTests.Array exposing (suite)

import Array exposing (Array)
import Fuzz exposing (Fuzzer, intRange)
import Test exposing (Test, describe, fuzz)
import TestUtils exposing (evalExpect, evalTest, list, maybe, withInt)
import Value exposing (Value(..))


suite : Test
suite =
    Test.skip <|
        describe "Array"
            [ initTests
            , isEmptyTests
            , lengthTests
            , getSetTests
            , conversionTests
            , transformTests
            , sliceTests
            , runtimeCrashTests
            ]


{-|

> 33000 elements requires 3 levels in the tree

-}
defaultSizeRange : Fuzzer Int
defaultSizeRange =
    intRange 1 33000


fuzzEvalTest : String -> String -> (a -> Value) -> (Int -> a) -> Test
fuzzEvalTest name source kind value =
    fuzz defaultSizeRange name <|
        \size ->
            evalExpect (withInt "size" size source)
                kind
                (value size)


initTests : Test
initTests =
    describe "Initialization"
        [ fuzzEvalTest "initialize"
            "Array.toList <| Array.initialize size identity"
            List
            (\size -> Array.toList <| Array.initialize size Int)
        , Test.skip <|
            fuzzEvalTest "push"
                "Array.toList <| List.foldl Array.push Array.empty (List.range 0 (size - 1))"
                List
                (\size -> Array.toList <| List.foldl Array.push Array.empty (List.map Int <| List.range 0 (size - 1)))
        , evalTest "initialize non-identity"
            "Array.toList (Array.initialize 4 (\\n -> n * n))"
            (list Int)
            (Array.toList (Array.initialize 4 (\n -> n * n)))
        , evalTest "initialize empty"
            "Array.toList (Array.initialize 0 identity)"
            (list Int)
            (Array.toList (Array.initialize 0 identity))
        , evalTest "initialize negative"
            "Array.toList (Array.initialize -2 identity)"
            (list Int)
            (Array.toList (Array.initialize -2 identity))
        ]


isEmptyTests : Test
isEmptyTests =
    describe "isEmpty"
        [ evalTest "all empty arrays are equal"
            "(Array.empty == Array.fromList [])"
            Bool
            (Array.empty == Array.fromList [])
        , evalTest "empty array"
            "Array.isEmpty Array.empty"
            Bool
            (Array.isEmpty Array.empty)
        , evalTest "empty converted array"
            "Array.isEmpty (Array.fromList [])"
            Bool
            (Array.isEmpty (Array.fromList []))
        , evalTest "non-empty array"
            "Array.isEmpty (Array.fromList [ 1 ])"
            Bool
            (Array.isEmpty (Array.fromList [ 1 ]))
        ]


lengthTests : Test
lengthTests =
    describe "Length"
        [ evalTest "empty array"
            "Array.length Array.empty"
            Int
            (Array.length Array.empty)
        , fuzzEvalTest "non-empty array"
            "Array.length (Array.initialize size identity)"
            Int
            (\size -> Array.length (Array.initialize size identity))

        -- , fuzz defaultSizeRange "push" <|
        --     \size ->
        --         length (push size (initialize size identity))
        --             |> Expect.equal (size + 1)
        -- , fuzz defaultSizeRange "append" <|
        --     \size ->
        --         length (append (initialize size identity) (initialize (size // 2) identity))
        --             |> Expect.equal (size + (size // 2))
        -- , fuzz defaultSizeRange "set does not increase" <|
        --     \size ->
        --         length (set (size // 2) 1 (initialize size identity))
        --             |> Expect.equal size
        -- , fuzz (intRange 100 10000) "big slice" <|
        --     \size ->
        --         length (slice 35 -35 (initialize size identity))
        --             |> Expect.equal (size - 70)
        -- , fuzz2 (intRange -32 -1) (intRange 100 10000) "small slice end" <|
        --     \n size ->
        --         length (slice 0 n (initialize size identity))
        --             |> Expect.equal (size + n)
        ]


getSetTests : Test
getSetTests =
    describe "Get and set"
        [ --     fuzz2 defaultSizeRange defaultSizeRange "can retrieve element" <|
          --     \x y ->
          --         let
          --             n =
          --                 min x y
          --             size =
          --                 max x y
          --         in
          --         get n (initialize (size + 1) identity)
          --             |> Expect.equal (Just n)
          -- , fuzz2 (intRange 1 50) (intRange 100 33000) "out of bounds retrieval returns nothing" <|
          --     \n size ->
          --         let
          --             arr =
          --                 initialize size identity
          --         in
          --         ( get (negate n) arr
          --         , get (size + n) arr
          --         )
          --             |> Expect.equal ( Nothing, Nothing )
          -- , fuzz2 defaultSizeRange defaultSizeRange "set replaces value" <|
          --     \x y ->
          --         let
          --             n =
          --                 min x y
          --             size =
          --                 max x y
          --         in
          --         get n (set n 5 (initialize (size + 1) identity))
          --             |> Expect.equal (Just 5)
          -- , fuzz2 (intRange 1 50) defaultSizeRange "set out of bounds returns original array" <|
          --     \n size ->
          --         let
          --             arr =
          --                 initialize size identity
          --         in
          --         set (negate n) 5 arr
          --             |> set (size + n) 5
          --             |> Expect.equal arr,
          Test.skip <|
            evalTest "Retrieval works from tail"
                "Array.get 1030 (Array.set 1030 5 (Array.initialize 1035 identity))"
                (maybe Int)
                (Array.get 1030 (Array.set 1030 5 (Array.initialize 1035 identity)))
        ]


conversionTests : Test
conversionTests =
    describe "Conversion"
        [ --     fuzz defaultSizeRange "back and forth" <|
          --     \size ->
          --         let
          --             ls =
          --                 List.range 0 (size - 1)
          --         in
          --         toList (fromList ls)
          --             |> Expect.equal ls,
          --   Test.skip <|
          Test.skip <|
            fuzzEvalTest "indexed"
                "Array.toIndexedList (Array.initialize size ((+) 1)) == Array.toList (Array.initialize size (\\idx -> ( idx, idx + 1 )))"
                Bool
                (\size -> Array.toIndexedList (Array.initialize size ((+) 1)) == Array.toList (Array.initialize size (\idx -> ( idx, idx + 1 ))))
        ]


transformTests : Test
transformTests =
    describe "Transform"
        [ --      fuzz defaultSizeRange "foldl" <|
          --     \size ->
          --         foldl (::) [] (initialize size identity)
          --             |> Expect.equal (List.reverse (List.range 0 (size - 1)))
          -- , fuzz defaultSizeRange "foldr" <|
          --     \size ->
          --         foldr (\n acc -> n :: acc) [] (initialize size identity)
          --             |> Expect.equal (List.range 0 (size - 1))
          -- , fuzz defaultSizeRange "filter" <|
          --     \size ->
          --         toList (filter (\a -> modBy 2 a == 0) (initialize size identity))
          --             |> Expect.equal (List.filter (\a -> modBy 2 a == 0) (List.range 0 (size - 1))),
          Test.skip <|
            fuzzEvalTest "map"
                "Array.map ((+) 1) (Array.initialize size identity) == Array.initialize size ((+) 1)"
                Bool
                (\size -> Array.map ((+) 1) (Array.initialize size identity) == Array.initialize size ((+) 1))

        -- , fuzz defaultSizeRange "indexedMap" <|
        --     \size ->
        --         indexedMap (*) (repeat size 5)
        --             |> Expect.equal (initialize size ((*) 5))
        -- , fuzz defaultSizeRange "push appends one element" <|
        --     \size ->
        --         push size (initialize size identity)
        --             |> Expect.equal (initialize (size + 1) identity)
        -- , fuzz (intRange 1 1050) "append" <|
        --     \size ->
        --         append (initialize size identity) (initialize size ((+) size))
        --             |> Expect.equal (initialize (size * 2) identity)
        -- , fuzz2 defaultSizeRange (intRange 1 32) "small appends" <|
        --     \s1 s2 ->
        --         append (initialize s1 identity) (initialize s2 ((+) s1))
        --             |> Expect.equal (initialize (s1 + s2) identity)
        ]


sliceTests : Test
sliceTests =
    let
        smallSample : Array Int
        smallSample =
            Array.fromList (List.range 1 8)
    in
    describe "Slice"
        [ --     fuzz2 (intRange -50 -1) (intRange 100 33000) "both" <|
          --     \n size ->
          --         slice (abs n) n (initialize size identity)
          --             |> Expect.equal (initialize (size + n + n) (\idx -> idx - n))
          -- , fuzz2 (intRange -50 -1) (intRange 100 33000) "left" <|
          --     \n size ->
          --         let
          --             arr =
          --                 initialize size identity
          --         in
          --         slice (abs n) (length arr) arr
          --             |> Expect.equal (initialize (size + n) (\idx -> idx - n))
          -- , fuzz2 (intRange -50 -1) (intRange 100 33000) "right" <|
          --     \n size ->
          --         slice 0 n (initialize size identity)
          --             |> Expect.equal (initialize (size + n) identity)
          -- , fuzz defaultSizeRange "slicing all but the last item" <|
          --     \size ->
          --         initialize size identity
          --             |> slice -1 size
          --             |> toList
          --             |> Expect.equal [ size - 1 ],
          Test.skip <|
            evalTest "both small"
                "let smallSample = Array.fromList (List.range 1 8) in Array.toList (Array.slice 2 5 smallSample)"
                (list Int)
                (Array.toList (Array.slice 2 5 smallSample))

        -- , test "start small" <|
        --     \() ->
        --         toList (slice 2 (length smallSample) smallSample)
        --             |> Expect.equal (List.range 3 8)
        -- , test "negative" <|
        --     \() ->
        --         toList (slice -5 -2 smallSample)
        --             |> Expect.equal (List.range 4 6)
        -- , test "impossible" <|
        --     \() ->
        --         toList (slice -1 -2 smallSample)
        --             |> Expect.equal []
        -- , test "crash" <|
        --     \() ->
        --         Array.repeat (33 * 32) 1
        --             |> Array.slice 0 1
        --             |> Expect.equal (Array.repeat 1 1)
        ]


runtimeCrashTests : Test
runtimeCrashTests =
    describe "Runtime crashes in core"
        [ Test.skip <|
            evalTest "magic slice"
                """Array.initialize 40 identity
                    |> Array.slice 10 40
                    |> Array.slice 10 30
                    |> Array.slice 10 20
                    |> Array.slice 10 10
                    |> (\\a -> a == a)"""
                Bool
                (Array.initialize 40 identity
                    |> Array.slice 10 40
                    |> Array.slice 10 30
                    |> Array.slice 10 20
                    |> Array.slice 10 10
                    |> (\a -> a == a)
                )

        -- , test "magic slice 2" <|
        --     \() ->
        --         let
        --             ary =
        --                 fromList <| List.range 0 32
        --             res =
        --                 append (slice 1 32 ary) (slice (32 + 1) -1 ary)
        --         in
        --         Expect.equal res res
        -- , test "magic append" <|
        --     \() ->
        --         let
        --             res =
        --                 append (initialize 1 (always 1))
        --                     (initialize (32 ^ 2 - 1 * 32 + 1) (\i -> i))
        --         in
        --         Expect.equal res res
        ]
