module CoreTests.Bitwise exposing (suite)

import Bitwise
import Test exposing (Test, describe)
import TestUtils exposing (evalTest)
import Types exposing (Value(..))


suite : Test
suite =
    describe "Bitwise"
        [ describe "and"
            [ evalTest "and with 32 bit integers"
                "Bitwise.and 5 3"
                Int
                (Bitwise.and 5 3)
            , evalTest "and with 0 as first argument"
                "Bitwise.and 0 1450"
                Int
                (Bitwise.and 0 1450)
            , evalTest "and with 0 as second argument"
                "Bitwise.and 274 0"
                Int
                (Bitwise.and 274 0)
            , evalTest "and with -1 as first argument"
                "Bitwise.and -1 2671"
                Int
                (Bitwise.and -1 2671)
            , evalTest "and with -1 as second argument"
                "Bitwise.and 96 -1"
                Int
                (Bitwise.and 96 -1)
            ]
        , describe "or"
            [ evalTest "or with 32 bit integers"
                "Bitwise.or 9 14"
                Int
                (Bitwise.or 9 14)
            , evalTest "or with 0 as first argument"
                "Bitwise.or 0 843"
                Int
                (Bitwise.or 0 843)
            , evalTest "or with 0 as second argument"
                "Bitwise.or 19 0"
                Int
                (Bitwise.or 19 0)
            , evalTest "or with -1 as first argument"
                "Bitwise.or -1 2360"
                Int
                (Bitwise.or -1 2360)
            , evalTest "or with -1 as second argument"
                "Bitwise.or 3 -1"
                Int
                (Bitwise.or 3 -1)
            ]
        , describe "xor"
            [ evalTest "xor with 32 bit integers"
                "Bitwise.xor 580 24"
                Int
                (Bitwise.xor 580 24)
            , evalTest "xor with 0 as first argument"
                "Bitwise.xor 0 56"
                Int
                (Bitwise.xor 0 56)
            , evalTest "xor with 0 as second argument"
                "Bitwise.xor -268 0"
                Int
                (Bitwise.xor -268 0)
            , evalTest "xor with -1 as first argument"
                "Bitwise.xor -1 24"
                Int
                (Bitwise.xor -1 24)
            , evalTest "xor with -1 as second argument"
                "Bitwise.xor -25602 -1"
                Int
                (Bitwise.xor -25602 -1)
            ]
        , describe "complement"
            [ evalTest "complement a positive"
                "Bitwise.complement 8"
                Int
                (Bitwise.complement 8)
            , evalTest "complement a negative"
                "Bitwise.complement -279"
                Int
                (Bitwise.complement -279)
            ]
        , describe "shiftLeftBy"
            [ evalTest "8 |> shiftLeftBy 1 == 16"
                "8 |> Bitwise.shiftLeftBy 1"
                Int
                (8 |> Bitwise.shiftLeftBy 1)
            , evalTest "8 |> shiftLeftby 2 == 32"
                "8 |> Bitwise.shiftLeftBy 2"
                Int
                (8 |> Bitwise.shiftLeftBy 2)
            ]
        , describe "shiftRightBy"
            [ evalTest "32 |> shiftRight 1 == 16"
                "32 |> Bitwise.shiftRightBy 1"
                Int
                (32 |> Bitwise.shiftRightBy 1)
            , evalTest "32 |> shiftRight 2 == 8"
                "32 |> Bitwise.shiftRightBy 2"
                Int
                (32 |> Bitwise.shiftRightBy 2)
            , evalTest "-32 |> shiftRight 1 == -16"
                "-32 |> Bitwise.shiftRightBy 1"
                Int
                (-32 |> Bitwise.shiftRightBy 1)
            ]
        , describe "shiftRightZfBy"
            [ evalTest "32 |> shiftRightZfBy 1 == 16"
                "32 |> Bitwise.shiftRightZfBy 1"
                Int
                (32 |> Bitwise.shiftRightZfBy 1)
            , evalTest "32 |> shiftRightZfBy 2 == 8"
                "32 |> Bitwise.shiftRightZfBy 2"
                Int
                (32 |> Bitwise.shiftRightZfBy 2)
            , evalTest "-32 |> shiftRightZfBy 1 == 2147483632"
                "-32 |> Bitwise.shiftRightZfBy 1"
                Int
                (-32 |> Bitwise.shiftRightZfBy 1)
            ]
        ]
