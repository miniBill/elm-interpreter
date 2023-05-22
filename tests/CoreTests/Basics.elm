module CoreTests.Basics exposing (suite)

import Test exposing (Test, describe)
import Utils exposing (evalTest, evalTest_)
import Value exposing (Value(..))


suite : Test
suite =
    let
        comparison : Test
        comparison =
            describe "Comparison"
                [ evalTest "max" "max 32 42" <| Int <| max 32 42
                , evalTest "min" "min 91 42" <| Int <| min 91 42
                , evalTest "clamp low" "clamp 10 20 5" <| Int <| clamp 10 20 5
                , evalTest "clamp mid" "clamp 10 20 15" <| Int <| clamp 10 20 15
                , evalTest "clamp high" "clamp 10 20 25" <| Int <| clamp 10 20 25
                , evalTest_ "5 < 6" <| Bool <| 5 < 6
                , evalTest_ "6 < 5" <| Bool <| 6 < 5
                , evalTest_ "6 < 6" <| Bool <| 6 < 6
                , evalTest_ "5 > 6" <| Bool <| 5 > 6
                , evalTest_ "6 > 5" <| Bool <| 6 > 5
                , evalTest_ "6 > 6" <| Bool <| 6 > 6
                , evalTest_ "5 <= 6" <| Bool <| 5 <= 6
                , evalTest_ "6 <= 5" <| Bool <| 6 <= 5
                , evalTest_ "6 <= 6" <| Bool <| 6 <= 6
                , evalTest_ "compare \"A\" \"B\"" <| Value.fromOrder (compare "A" "B")
                , evalTest_ "compare 'f' 'f'" <| Value.fromOrder (compare 'f' 'f')
                , evalTest_ "compare (1, 2, 3) (0, 1, 2)" <| Value.fromOrder (compare ( 1, 2, 3 ) ( 0, 1, 2 ))

                -- , evalTest "compare ['a'] ['b']" <| \() -> Expect.equal LT (compare [ 'a' ] [ 'b' ])
                -- , evalTest "array equality" <| \() -> Expect.equal (Array.fromList [ 1, 1, 1, 1 ]) (Array.repeat 4 1)
                -- , evalTest "set equality" <| \() -> Expect.equal (Set.fromList [ 1, 2 ]) (Set.fromList [ 2, 1 ])
                -- , evalTest "dict equality" <| \() -> Expect.equal (Dict.fromList [ ( 1, 1 ), ( 2, 2 ) ]) (Dict.fromList [ ( 2, 2 ), ( 1, 1 ) ])
                -- , evalTest "char equality" <| \() -> Expect.notEqual '0' '饑'
                ]

        toStringTests : Test
        toStringTests =
            describe "toString Tests"
                [ evalTest "toString Int" "Debug.toString 42" <| String <| Debug.toString 42
                , evalTest "toString Float" "Debug.toString 42.52" <| String <| Debug.toString 42.52
                , evalTest "toString Char" "Debug.toString 'c'" <| String <| Debug.toString 'c'

                -- , evalTest "toString Char single quote" <| \() -> Expect.equal "'\\''" (toString '\'')
                -- , evalTest "toString Char double quote" <| \() -> Expect.equal "'\"'" (toString '"')
                -- , evalTest "toString String single quote" <| \() -> Expect.equal "\"not 'escaped'\"" (toString "not 'escaped'")
                -- , evalTest "toString String double quote" <| \() -> Expect.equal "\"are \\\"escaped\\\"\"" (toString "are \"escaped\"")
                -- , evalTest "toString record" <| \() -> Expect.equal "{ field = [0] }" (toString { field = [ 0 ] })
                ]

        trigTests : Test
        trigTests =
            describe "Trigonometry Tests"
                [ evalTest_ "radians 0" (Float <| radians 0)
                , evalTest "radians positive" "radians 5" (Float <| radians 5)
                , evalTest "radians negative" "radians -5" (Float <| radians -5)

                -- , evalTest "degrees 0" <| \() -> Expect.equal 0 (degrees 0)
                -- , evalTest "degrees 90" <| \() -> Expect.lessThan 0.01 (abs (1.57 - degrees 90))
                -- -- This should test to enough precision to know if anything's breaking
                -- , evalTest "degrees -145" <| \() -> Expect.lessThan 0.01 (abs (-2.53 - degrees -145))
                -- -- This should test to enough precision to know if anything's breaking
                -- , evalTest "turns 0" <| \() -> Expect.equal 0 (turns 0)
                -- , evalTest "turns 8" <| \() -> Expect.lessThan 0.01 (abs (50.26 - turns 8))
                -- -- This should test to enough precision to know if anything's breaking
                -- , evalTest "turns -133" <| \() -> Expect.lessThan 0.01 (abs (-835.66 - turns -133))
                -- -- This should test to enough precision to know if anything's breaking
                -- , evalTest "fromPolar (0, 0)" <| \() -> Expect.equal ( 0, 0 ) (fromPolar ( 0, 0 ))
                -- , evalTest "fromPolar (1, 0)" <| \() -> Expect.equal ( 1, 0 ) (fromPolar ( 1, 0 ))
                -- , evalTest "fromPolar (0, 1)" <| \() -> Expect.equal ( 0, 0 ) (fromPolar ( 0, 1 ))
                -- , evalTest "fromPolar (1, 1)" <|
                --     \() ->
                --         Expect.equal True
                --             (let
                --                 ( x, y ) =
                --                     fromPolar ( 1, 1 )
                --              in
                --              0.54 - x < 0.01 && 0.84 - y < 0.01
                --             )
                -- , evalTest "toPolar (0, 0)" <| \() -> Expect.equal ( 0, 0 ) (toPolar ( 0, 0 ))
                -- , evalTest "toPolar (1, 0)" <| \() -> Expect.equal ( 1, 0 ) (toPolar ( 1, 0 ))
                -- , evalTest "toPolar (0, 1)" <|
                --     \() ->
                --         Expect.equal True
                --             (let
                --                 ( r, theta ) =
                --                     toPolar ( 0, 1 )
                --              in
                --              r == 1 && abs (1.57 - theta) < 0.01
                --             )
                -- , evalTest "toPolar (1, 1)" <|
                --     \() ->
                --         Expect.equal True
                --             (let
                --                 ( r, theta ) =
                --                     toPolar ( 1, 1 )
                --              in
                --              abs (1.41 - r) < 0.01 && abs (0.78 - theta) < 0.01
                --             )
                -- , evalTest "cos" <| \() -> Expect.equal 1 (cos 0)
                -- , evalTest "sin" <| \() -> Expect.equal 0 (sin 0)
                -- , evalTest "tan" <| \() -> Expect.lessThan 0.01 (abs (12.67 - tan 17.2))
                -- , evalTest "acos" <| \() -> Expect.lessThan 0.01 (abs (3.14 - acos -1))
                -- , evalTest "asin" <| \() -> Expect.lessThan 0.01 (abs (0.3 - asin 0.3))
                -- , evalTest "atan" <| \() -> Expect.lessThan 0.01 (abs (1.57 - atan 4567.8))
                -- , evalTest "atan2" <| \() -> Expect.lessThan 0.01 (abs (1.55 - atan2 36 0.65))
                -- , evalTest "pi" <| \() -> Expect.lessThan 0.01 (abs (3.14 - pi))
                ]

        basicMathTests : Test
        basicMathTests =
            describe "Basic Math Tests"
                [ evalTest "add float" "155.6 + 3.4" <| Float <| 155.6 + 3.4

                -- , evalTest "add int" <| \() -> Expect.equal 17 (round 10 + round 7)
                -- , evalTest "subtract float" <| \() -> Expect.within (Absolute 0.00000001) -6.3 (1 - 7.3)
                -- , evalTest "subtract int" <| \() -> Expect.equal 1130 (round 9432 - round 8302)
                -- , evalTest "multiply float" <| \() -> Expect.within (Relative 0.00000001) 432 (96 * 4.5)
                -- , evalTest "multiply int" <| \() -> Expect.equal 90 (round 10 * round 9)
                -- , evalTest "divide float" <| \() -> Expect.within (Relative 0.00000001) 13.175 (527 / 40)
                -- , evalTest "divide int" <| \() -> Expect.equal 23 (70 // 3)
                -- , evalTest "7 |> remainderBy 2" <| \() -> Expect.equal 1 (7 |> remainderBy 2)
                -- , evalTest "-1 |> remainderBy 4" <| \() -> Expect.equal -1 (-1 |> remainderBy 4)
                -- , evalTest "modBy 2 7" <| \() -> Expect.equal 1 (modBy 2 7)
                -- , evalTest "modBy 4 -1" <| \() -> Expect.equal 3 (modBy 4 -1)
                -- , evalTest "3^2" <| \() -> Expect.equal 9 (3 ^ 2)
                -- , evalTest "sqrt" <| \() -> Expect.equal 9 (sqrt 81)
                -- , evalTest "negate 42" <| \() -> Expect.equal -42 (negate 42)
                -- , evalTest "negate -42" <| \() -> Expect.equal 42 (negate -42)
                -- , evalTest "negate 0" <| \() -> Expect.equal 0 (negate 0)
                -- , evalTest "abs -25" <| \() -> Expect.equal 25 (abs -25)
                -- , evalTest "abs 76" <| \() -> Expect.equal 76 (abs 76)
                -- , evalTest "logBase 10 100" <| \() -> Expect.equal 2 (logBase 10 100)
                -- , evalTest "logBase 2 256" <| \() -> Expect.equal 8 (logBase 2 256)
                -- , evalTest "e" <| \() -> Expect.lessThan 0.01 (abs (2.72 - e))
                ]

        booleanTests : Test
        booleanTests =
            describe "Boolean Tests"
                [ evalTest_ "False && False" <| Bool <| False && False
                , evalTest_ "False && True" <| Bool <| False && True
                , evalTest_ "True && False" <| Bool <| True && False
                , evalTest_ "True && True" <| Bool <| True && True
                , evalTest_ "False || False" <| Bool <| False || False
                , evalTest_ "False || True" <| Bool <| False || True
                , evalTest_ "True || False" <| Bool <| True || False
                , evalTest_ "True || True" <| Bool <| True || True
                , evalTest_ "xor False False" <| Bool <| xor False False
                , evalTest_ "xor False True" <| Bool <| xor False True
                , evalTest_ "xor True False" <| Bool <| xor True False
                , evalTest_ "xor True True" <| Bool <| xor True True
                , evalTest_ "not True" <| Bool <| not True
                , evalTest_ "not False" <| Bool <| not False
                ]

        conversionTests : Test
        conversionTests =
            describe "Conversion Tests"
                [ evalTest_ "round 0.6" (Int <| round 0.6)
                , evalTest_ "round 0.4" (Int <| round 0.4)
                , evalTest_ "round 0.5" (Int <| round 0.5)
                , evalTest_ "truncate -2367.9267" (Int <| truncate -2367.9267)
                , evalTest_ "floor -2367.9267" (Int <| floor -2367.9267)
                , evalTest_ "ceiling 37.2" (Int <| ceiling 37.2)
                , evalTest_ "toFloat 25" (Float <| toFloat 25)
                ]

        miscTests : Test
        miscTests =
            describe "Miscellaneous Tests"
                [ evalTest_ "isNaN (0/0)" <| Bool <| isNaN (0 / 0)
                , evalTest_ "isNaN (sqrt -1)" <| Bool <| isNaN (sqrt -1)
                , evalTest_ "isNaN (1/0)" <| Bool <| isNaN (1 / 0)
                , evalTest_ "isNaN 1" <| Bool <| isNaN 1
                , evalTest_ "isInfinite (0/0)" <| Bool <| isInfinite (0 / 0)
                , evalTest_ "isInfinite (sqrt -1)" <| Bool <| isInfinite (sqrt -1)
                , evalTest_ "isInfinite (1/0)" <| Bool <| isInfinite (1 / 0)
                , evalTest_ "isInfinite 1" <| Bool <| isInfinite 1
                , evalTest_ "\"hello\" ++ \"world\"" <| String <| "hello" ++ "world"
                , evalTest_ "[1, 1, 2] ++ [3, 5, 8]" <| List <| List.map Int <| [ 1, 1, 2 ] ++ [ 3, 5, 8 ]
                , evalTest_ "Tuple.first (1, 2)" <| Int <| Tuple.first ( 1, 2 )
                , evalTest_ "Tuple.second (1, 2)" <| Int <| Tuple.second ( 1, 2 )
                ]

        higherOrderTests : Test
        higherOrderTests =
            describe "Higher Order Helpers"
                [ evalTest_ "identity 'c'" <| Char <| identity 'c'
                , evalTest_ "always 42 ()" <| Int <| always 42 ()
                , evalTest "<|" " identity <| 3 + 6" <| Int <| (identity <| 3 + 6)
                , evalTest "|>" " 3 + 6 |> identity" <| Int <| (3 + 6 |> identity)

                -- TODO: fix
                , Test.skip <| evalTest "<<" " not << xor True <| True" <| Bool <| (not << xor True <| True)
                , describe ">>"
                    [ -- TODO: fix
                      Test.skip <|
                        evalTest "with xor"
                            "True |> xor True >> not"
                        <|
                            Bool <|
                                (True |> xor True >> not)
                    , evalTest "with a record accessor"
                        """
                            [ { foo = "NaS", bar = "baz" } ]
                                |> List.map (.foo >> String.reverse)
                        """
                        (([ { foo = "NaS", bar = "baz" } ]
                            |> List.map (.foo >> String.reverse)
                         )
                            |> List.map String
                            |> List
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
