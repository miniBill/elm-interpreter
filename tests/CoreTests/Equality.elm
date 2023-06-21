module CoreTests.Equality exposing (suite)

import Fuzz
import Test exposing (Test, describe)
import TestUtils exposing (evalExpect, evalTest, withInt)
import Value exposing (Value(..))


type Different
    = A String
    | B (List Int)


suite : Test
suite =
    describe "Equality Tests"
        [ diffTests
        , recordTests
        , listTests
        ]


listTests : Test
listTests =
    describe "List equality"
        [ Test.fuzz2
            (Fuzz.intRange 100 10000)
            (Fuzz.intRange 100 10000)
            "Simple comparison"
          <|
            \size1 size2 ->
                evalExpect
                    (withInt "size1" size1 <|
                        withInt "size2" size2 <|
                            "List.range 0 size1 == List.range 0 size2"
                    )
                    Bool
                    (List.range 0 size1 == List.range 0 size2)
        ]


diffTests : Test
diffTests =
    describe "ADT equality"
        [ evalTest "As eq"
            """(A "a" == A "a")"""
            Bool
            (A "a" == A "a")
        , evalTest "Bs eq"
            """(B [ 1 ] == B [ 1 ])"""
            Bool
            (B [ 1 ] == B [ 1 ])
        , evalTest "A left neq"
            """(A "a" /= B [ 1 ])"""
            Bool
            (A "a" /= B [ 1 ])
        , evalTest "A right neq"
            """(B [ 1 ] /= A "a")"""
            Bool
            (B [ 1 ] /= A "a")
        ]


recordTests : Test
recordTests =
    describe "Record equality"
        [ evalTest "empty same"
            """({} == {})"""
            Bool
            ({} == {})
        , evalTest "ctor same"
            """({ field = Just 3 } == { field = Just 3 })"""
            Bool
            ({ field = Just 3 } == { field = Just 3 })
        , evalTest "ctor same, special case"
            """({ ctor = Just 3 } == { ctor = Just 3 })"""
            Bool
            ({ ctor = Just 3 } == { ctor = Just 3 })
        , evalTest "ctor diff"
            """({ field = Just 3 } /= { field = Nothing })"""
            Bool
            ({ field = Just 3 } /= { field = Nothing })
        , evalTest "ctor diff, special case"
            """({ ctor = Just 3 } /= { ctor = Nothing })"""
            Bool
            ({ ctor = Just 3 } /= { ctor = Nothing })
        ]
