module CoreTests.Tuple exposing (suite)

import Test exposing (Test, describe)
import TestUtils exposing (evalTest, tuple)
import Types exposing (Value(..))


suite : Test
suite =
    describe "Tuple Tests"
        [ describe "first"
            [ evalTest "extracts first element"
                "Tuple.first ( 1, 2 )"
                Int
                (Tuple.first ( 1, 2 ))
            ]
        , describe "second"
            [ evalTest "extracts second element"
                "Tuple.second ( 1, 2 )"
                Int
                (Tuple.second ( 1, 2 ))
            ]
        , describe "mapFirst"
            [ evalTest "applies function to first element"
                "Tuple.mapFirst ((*) 5) ( 1, 1 )"
                (tuple Int Int)
                (Tuple.mapFirst ((*) 5) ( 1, 1 ))
            ]
        , describe "mapSecond"
            [ evalTest "applies function to second element"
                "Tuple.mapSecond ((*) 5) ( 1, 1 )"
                (tuple Int Int)
                (Tuple.mapSecond ((*) 5) ( 1, 1 ))
            ]
        ]
