module CoreTests.Basics exposing (suite)

import Array
import Dict
import Set
import Test exposing (Test, describe)
import TestUtils exposing (evalTest, evalTest_, list)
import Value exposing (Value(..))


suite : Test
suite =
    let
        comparison : Test
        comparison =
            describe "Comparison"
                [ evalTest "max" "max 32 42" Int <| max 32 42
                , evalTest "min" "min 91 42" Int <| min 91 42
                , evalTest "clamp low" "clamp 10 20 5" Int <| clamp 10 20 5
                , evalTest "clamp mid" "clamp 10 20 15" Int <| clamp 10 20 15
                , evalTest "clamp high" "clamp 10 20 25" Int <| clamp 10 20 25
                , evalTest_ "5 < 6" Bool <| 5 < 6
                , evalTest_ "6 < 5" Bool <| 6 < 5
                , evalTest_ "6 < 6" Bool <| 6 < 6
                , evalTest_ "5 > 6" Bool <| 5 > 6
                , evalTest_ "6 > 5" Bool <| 6 > 5
                , evalTest_ "6 > 6" Bool <| 6 > 6
                , evalTest_ "5 <= 6" Bool <| 5 <= 6
                , evalTest_ "6 <= 5" Bool <| 6 <= 5
                , evalTest_ "6 <= 6" Bool <| 6 <= 6
                , evalTest_ "compare \"A\" \"B\"" identity <| Value.fromOrder (compare "A" "B")
                , evalTest_ "compare 'f' 'f'" identity <| Value.fromOrder (compare 'f' 'f')
                , evalTest_ "compare (1, 2, 3) (0, 1, 2)" identity <| Value.fromOrder (compare ( 1, 2, 3 ) ( 0, 1, 2 ))
                , evalTest_ "compare ['a'] ['b']" identity <| Value.fromOrder (compare [ 'a' ] [ 'b' ])
                , evalTest "array equality" "Array.fromList [ 1, 1, 1, 1 ] == Array.repeat 4 1" Bool <| Array.fromList [ 1, 1, 1, 1 ] == Array.repeat 4 1
                , Test.skip <| evalTest "set equality" "Set.fromList [ 1, 2 ] == Set.fromList [ 2, 1 ]" Bool <| Set.fromList [ 1, 2 ] == Set.fromList [ 2, 1 ]
                , Test.skip <| evalTest "dict equality" "Dict.fromList [ ( 1, 1 ), ( 2, 2 ) ] == Dict.fromList [ ( 2, 2 ), ( 1, 1 ) ]" Bool <| Dict.fromList [ ( 1, 1 ), ( 2, 2 ) ] == Dict.fromList [ ( 2, 2 ), ( 1, 1 ) ]
                , evalTest "char equality" "'0' == '饑'" Bool <| '0' == '饑'
                ]

        toStringTests : Test
        toStringTests =
            describe "Debug.toString Tests"
                [ evalTest "toString Int" "Debug.toString 42" String <| Debug.toString 42
                , evalTest "toString Float" "Debug.toString 42.52" String <| Debug.toString 42.52
                , evalTest "toString Char" "Debug.toString 'c'" String <| Debug.toString 'c'
                , evalTest "toString Char single quote" "Debug.toString '\\''" String <| Debug.toString '\''
                , evalTest "toString Char double quote" "Debug.toString '\"'" String <| Debug.toString '"'
                , evalTest "toString String single quote" """Debug.toString "not 'escaped'" """ String <| Debug.toString "not 'escaped'"
                , Test.skip <| evalTest "toString record" "Debug.toString { field = [ 0 ] }" String <| Debug.toString { field = [ 0 ] }
                ]

        trigTests : Test
        trigTests =
            describe "Trigonometry Tests"
                [ evalTest_ "radians 0" Float <| radians 0
                , evalTest_ "radians 5" Float <| radians 5
                , evalTest_ "radians -5" Float <| radians -5
                , evalTest_ "degrees 0" Float <| degrees 0
                , evalTest_ "degrees 90" Float <| degrees 90
                , evalTest_ "degrees -145" Float <| degrees -145
                , evalTest_ "turns 0" Float <| turns 0
                , evalTest_ "turns 8" Float <| turns 8
                , evalTest_ "turns -133" Float <| turns -133
                , evalTest_ "fromPolar (0, 0)" floatTuple <| fromPolar ( 0, 0 )
                , evalTest_ "fromPolar (1, 0)" floatTuple <| fromPolar ( 1, 0 )
                , evalTest_ "fromPolar (0, 1)" floatTuple <| fromPolar ( 0, 1 )
                , evalTest_ "fromPolar (1, 1)" floatTuple <| fromPolar ( 1, 1 )
                , evalTest_ "toPolar (0, 0)" floatTuple <| toPolar ( 0, 0 )
                , evalTest_ "toPolar (1, 0)" floatTuple <| toPolar ( 1, 0 )
                , evalTest_ "toPolar (0, 1)" floatTuple <| toPolar ( 0, 1 )
                , evalTest_ "toPolar (1, 1)" floatTuple <| toPolar ( 1, 1 )
                , evalTest_ "cos 0" Float <| cos 0
                , evalTest_ "sin 0" Float <| sin 0
                , evalTest_ "tan 17.2" Float <| tan 17.2
                , evalTest_ "acos -1" Float <| acos -1
                , evalTest_ "asin 0.3" Float <| asin 0.3
                , evalTest_ "atan 4567.8" Float <| atan 4567.8
                , evalTest_ "atan2 36 0.65" Float <| atan2 36 0.65
                , evalTest_ "pi" Float pi
                ]

        basicMathTests : Test
        basicMathTests =
            describe "Basic Math Tests"
                [ evalTest "add float" "155.6 + 3.4" Float <| 155.6 + 3.4
                , evalTest "add int" "round 10 + round 7" Int <| (round 10 + round 7)
                , evalTest "subtract float" "1 - 7.3" Float <| 1 - 7.3
                , evalTest "subtract int" "round 9432 - round 8302" Int <| round 9432 - round 8302
                , evalTest "multiply float" "96 * 4.5" Float <| 96 * 4.5
                , evalTest "multiply int" "round 10 * round 9" Int <| round 10 * round 9
                , evalTest "divide float" "527 / 40" Float <| 527 / 40
                , evalTest "divide int" "70 // 3" Int <| 70 // 3
                , evalTest_ "7 |> remainderBy 2" Int <| (7 |> remainderBy 2)
                , evalTest_ "-1 |> remainderBy 4" Int <| (-1 |> remainderBy 4)
                , evalTest_ "modBy 2 7" Int <| modBy 2 7
                , evalTest_ "modBy 4 -1" Int <| modBy 4 -1
                , evalTest_ "3 ^ 2" Float <| 3 ^ 2
                , evalTest_ "sqrt 81" Float <| sqrt 81
                , evalTest_ "negate 42" Float <| negate 42
                , evalTest_ "negate -42" Float <| negate -42
                , evalTest_ "negate 0" Float <| negate 0
                , evalTest_ "abs -25" Float <| abs -25
                , evalTest_ "abs 76" Float <| abs 76
                , evalTest_ "logBase 10 100" Float <| logBase 10 100
                , evalTest_ "logBase 2 256" Float <| logBase 2 256
                , evalTest_ "e" Float e
                ]

        booleanTests : Test
        booleanTests =
            describe "Boolean Tests"
                [ evalTest_ "False && False" Bool <| False && False
                , evalTest_ "False && True" Bool <| False && True
                , evalTest_ "True && False" Bool <| True && False
                , evalTest_ "True && True" Bool <| True && True
                , evalTest_ "False || False" Bool <| False || False
                , evalTest_ "False || True" Bool <| False || True
                , evalTest_ "True || False" Bool <| True || False
                , evalTest_ "True || True" Bool <| True || True
                , evalTest_ "xor False False" Bool <| xor False False
                , evalTest_ "xor False True" Bool <| xor False True
                , evalTest_ "xor True False" Bool <| xor True False
                , evalTest_ "xor True True" Bool <| xor True True
                , evalTest_ "not True" Bool <| not True
                , evalTest_ "not False" Bool <| not False
                ]

        conversionTests : Test
        conversionTests =
            describe "Conversion Tests"
                [ evalTest_ "round 0.6" Int <| round 0.6
                , evalTest_ "round 0.4" Int <| round 0.4
                , evalTest_ "round 0.5" Int <| round 0.5
                , evalTest_ "truncate -2367.9267" Int <| truncate -2367.9267
                , evalTest_ "floor -2367.9267" Int <| floor -2367.9267
                , evalTest_ "ceiling 37.2" Int <| ceiling 37.2
                , evalTest_ "toFloat 25" Float <| toFloat 25
                ]

        miscTests : Test
        miscTests =
            describe "Miscellaneous Tests"
                [ evalTest_ "isNaN (0/0)" Bool <| isNaN (0 / 0)
                , evalTest_ "isNaN (sqrt -1)" Bool <| isNaN (sqrt -1)
                , evalTest_ "isNaN (1/0)" Bool <| isNaN (1 / 0)
                , evalTest_ "isNaN 1" Bool <| isNaN 1
                , evalTest_ "isInfinite (0/0)" Bool <| isInfinite (0 / 0)
                , evalTest_ "isInfinite (sqrt -1)" Bool <| isInfinite (sqrt -1)
                , evalTest_ "isInfinite (1/0)" Bool <| isInfinite (1 / 0)
                , evalTest_ "isInfinite 1" Bool <| isInfinite 1
                , evalTest_ "\"hello\" ++ \"world\"" String <| "hello" ++ "world"
                , evalTest_ "[1, 1, 2] ++ [3, 5, 8]" (list Int) <| [ 1, 1, 2 ] ++ [ 3, 5, 8 ]
                , evalTest_ "Tuple.first (1, 2)" Int <| Tuple.first ( 1, 2 )
                , evalTest_ "Tuple.second (1, 2)" Int <| Tuple.second ( 1, 2 )
                ]

        higherOrderTests : Test
        higherOrderTests =
            describe "Higher Order Helpers"
                [ evalTest_ "identity 'c'" Char <| identity 'c'
                , evalTest_ "always 42 ()" Int <| always 42 ()
                , evalTest "<|" " identity <| 3 + 6" Int <| (identity <| 3 + 6)
                , evalTest "|>" " 3 + 6 |> identity" Int <| (3 + 6 |> identity)
                , Test.skip <| evalTest "<<" " not << xor True <| True" Bool <| (not << xor True <| True)
                , describe ">>"
                    [ Test.skip <|
                        evalTest "with xor"
                            "True |> xor True >> not"
                            Bool
                        <|
                            (True |> xor True >> not)
                    , evalTest "with a record accessor"
                        """
                            [ { foo = "NaS", bar = "baz" } ]
                                |> List.map (.foo >> String.reverse)
                        """
                        (list String)
                      <|
                        ([ { foo = "NaS", bar = "baz" } ]
                            |> List.map (.foo >> String.reverse)
                        )
                    ]
                ]
    in
    describe "Basics"
        [ comparison
        , toStringTests
        , trigTests
        , basicMathTests
        , booleanTests
        , conversionTests
        , miscTests
        , higherOrderTests
        ]


floatTuple : ( Float, Float ) -> Value
floatTuple ( l, r ) =
    Tuple (Float l) (Float r)
