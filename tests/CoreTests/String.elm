module CoreTests.String exposing (suite)

import Expect
import Test exposing (Test, describe, test)
import TestUtils exposing (evalTest_, list)
import Types exposing (Value(..))


suite : Test
suite =
    describe "String"
        [ simpleTests
        , combiningTests
        , intTests
        , floatTests
        , encodingTests
        ]


simpleTests : Test
simpleTests =
    describe "Simple Stuff"
        [ evalTest_ "String.isEmpty \"\"" Bool <| String.isEmpty ""
        , evalTest_ "String.isEmpty \"the world\"" Bool <| String.isEmpty "the world"
        , evalTest_ "String.length \"innumerable\"" Int <| String.length "innumerable"
        , evalTest_ "String.endsWith \"ship\" \"spaceship\"" Bool <| String.endsWith "ship" "spaceship"
        , evalTest_ "String.reverse \"stressed\"" String <| String.reverse "stressed"
        , evalTest_ "String.repeat 3 \"ha\"" String <| String.repeat 3 "ha"
        , evalTest_ "String.indexes \"a\" \"aha\"" (list Int) <| String.indexes "a" "aha"
        , evalTest_ "String.indexes \"\" \"aha\"" (list Int) <| String.indexes "" "aha"
        ]


combiningTests : Test
combiningTests =
    describe "Combining Strings"
        [ test "uncons non-empty" <| \() -> Expect.equal (Just ( 'a', "bc" )) (String.uncons "abc")
        , test "uncons empty" <| \() -> Expect.equal Nothing (String.uncons "")
        , test "append 1" <| \() -> Expect.equal "butterfly" (String.append "butter" "fly")
        , test "append 2" <| \() -> Expect.equal "butter" (String.append "butter" "")
        , test "append 3" <| \() -> Expect.equal "butter" (String.append "" "butter")
        , test "concat" <| \() -> Expect.equal "nevertheless" (String.concat [ "never", "the", "less" ])
        , test "split commas" <| \() -> Expect.equal [ "cat", "dog", "cow" ] (String.split "," "cat,dog,cow")
        , test "split slashes" <| \() -> Expect.equal [ "home", "steve", "Desktop", "" ] (String.split "/" "home/steve/Desktop/")
        , test "join spaces" <| \() -> Expect.equal "cat dog cow" (String.join " " [ "cat", "dog", "cow" ])
        , test "join slashes" <| \() -> Expect.equal "home/steve/Desktop" (String.join "/" [ "home", "steve", "Desktop" ])
        , test "slice 1" <| \() -> Expect.equal "c" (String.slice 2 3 "abcd")
        , test "slice 2" <| \() -> Expect.equal "abc" (String.slice 0 3 "abcd")
        , test "slice 3" <| \() -> Expect.equal "abc" (String.slice 0 -1 "abcd")
        , test "slice 4" <| \() -> Expect.equal "cd" (String.slice -2 4 "abcd")
        ]


intTests : Test
intTests =
    describe "String.toInt"
        [ goodInt "1234" 1234
        , goodInt "+1234" 1234
        , goodInt "-1234" -1234
        , badInt "1.34"
        , badInt "1e31"
        , badInt "123a"
        , goodInt "0123" 123
        , badInt "0x001A"
        , badInt "0x001a"
        , badInt "0xBEEF"
        , badInt "0x12.0"
        , badInt "0x12an"
        ]


floatTests : Test
floatTests =
    describe "String.toFloat"
        [ goodFloat "123" 123
        , goodFloat "3.14" 3.14
        , goodFloat "+3.14" 3.14
        , goodFloat "-3.14" -3.14
        , goodFloat "0.12" 0.12
        , goodFloat ".12" 0.12
        , goodFloat "1e-42" 1.0e-42
        , goodFloat "6.022e23" 6.022e23
        , goodFloat "6.022E23" 6.022e23
        , goodFloat "6.022e+23" 6.022e23
        , badFloat "6.022e"
        , badFloat "6.022n"
        , badFloat "6.022.31"
        ]


encodingTests : Test
encodingTests =
    describe "UTF-16 Encoding"
        [ test "reverse 1" <| \() -> Expect.equal "𝌆c𝌆b𝌆a𝌆" (String.reverse "𝌆a𝌆b𝌆c𝌆")
        , test "reverse 2" <| \() -> Expect.equal "nàm" (String.reverse "màn")
        , test "reverse 3" <| \() -> Expect.equal "😣ba" (String.reverse "ab😣")
        , test "filter" <| \() -> Expect.equal "mànabc" (String.filter (\c -> c /= '😣') "màn😣abc")
        , test "toList" <| \() -> Expect.equal [ '𝌆', 'a', '𝌆', 'b', '𝌆' ] (String.toList "𝌆a𝌆b𝌆")
        , test "uncons" <| \() -> Expect.equal (Just ( '😃', "bc" )) (String.uncons "😃bc")
        , test "map 1" <| \() -> Expect.equal "aaa" (String.map (\_ -> 'a') "😃😃😃")
        , test "map 2" <| \() -> Expect.equal "😃😃😃" (String.map (\_ -> '😃') "aaa")
        , test "foldl" <| \() -> Expect.equal 3 (String.foldl (\_ c -> c + 1) 0 "😃😃😃")
        , test "foldr" <| \() -> Expect.equal 3 (String.foldr (\_ c -> c + 1) 0 "😃😃😃")
        , test "all" <| \() -> Expect.equal True (String.all ((==) '😃') "😃😃😃")
        , test "any" <| \() -> Expect.equal True (String.any ((==) '😃') "abc😃123")
        ]



-- NUMBER HELPERS


goodInt : String -> Int -> Test
goodInt str int =
    test str <|
        \_ ->
            Expect.equal (Just int) (String.toInt str)


badInt : String -> Test
badInt str =
    test str <|
        \_ ->
            Expect.equal
                Nothing
                (String.toInt str)


goodFloat : String -> Float -> Test
goodFloat str float =
    test str <|
        \_ ->
            Expect.equal (Just float) (String.toFloat str)


badFloat : String -> Test
badFloat str =
    test str <|
        \_ ->
            Expect.equal
                Nothing
                (String.toFloat str)
