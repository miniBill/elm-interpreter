module TopologicalSortTests exposing (suite)

import Expect
import FastDict as Dict exposing (Dict)
import Set
import Test exposing (Test, describe, test)
import TopologicalSort


suite : Test
suite =
    describe "Tests for TopologicalSort"
        [ acyclicGraphTest
        , cyclicGraphTest
        ]


acyclicGraphTest : Test
acyclicGraphTest =
    describe "Acyclic graph"
        [ sortTest "Linear graph"
            [ ( "a", "b" ), ( "b", "c" ), ( "c", "" ) ]
            (Ok [ "a", "b", "c" ])
        , sortTest "Unconnected nodes"
            [ ( "a", "b" ), ( "b", "c" ), ( "c", "" ), ( "d", "a" ), ( "e", "c" ), ( "f", "" ) ]
            (Ok [ "d", "a", "b", "e", "c", "f" ])
        , sortTest "No shortcuts"
            [ ( "a", "bcd" ), ( "b", "cd" ), ( "c", "d" ), ( "d", "" ) ]
            (Ok [ "a", "b", "c", "d" ])
        , sortTest "No doubles"
            [ ( "a", "bc" ), ( "b", "d" ), ( "cd", "e" ), ( "e", "" ) ]
            (Ok [ "a", "b", "cd", "e" ])
        , sortTest "With functions"
            [ ( "a", "b" ), ( "b()", "c" ), ( "c()", "d" ), ( "d", "" ) ]
            (Ok [ "a", "b()", "c()", "d" ])
        ]


cyclicGraphTest : Test
cyclicGraphTest =
    describe "Cyclic graph"
        [ sortTest "Mutually recursive"
            [ ( "a()", "b" ), ( "b()", "a" ) ]
            (Ok [ "b()", "a()" ])
        , sortTest "Larger cycle"
            [ ( "a()", "b" ), ( "b()", "c" ), ( "c()", "d" ), ( "d()", "e" ), ( "e()", "a" ) ]
            (Ok [ "e()", "a()", "b()", "c()", "d()" ])
        , sortTest "With dependencies"
            [ ( "a", "b" ), ( "b()", "ce" ), ( "c()", "df" ), ( "d()", "be" ), ( "e", "" ), ( "f", "" ) ]
            (Ok [ "a", "d()", "b()", "c()", "e", "f" ])
        , sortTest "Line of cycles"
            [ ( "a", "b" ), ( "b()", "cd" ), ( "c()", "b" ), ( "d", "e" ), ( "e()", "f" ), ( "f()", "eg" ), ( "g", "" ) ]
            (Ok [ "a", "c()", "b()", "d", "f()", "e()", "g" ])
        , sortTest "Connected cycles"
            (let
                -- to avoid warnings about concatenating list literals
                emptyList : List a
                emptyList =
                    []
             in
             [ ( "a", "b" ), ( "b()", "ce" ), ( "c()", "bd" ), ( "d", "" ) ]
                ++ emptyList
                ++ [ ( "e()", "f" ), ( "f()", "gi" ), ( "g()", "hj" ), ( "h()", "b" ), ( "i", "" ) ]
                ++ emptyList
                ++ [ ( "j()", "gk" ), ( "k", "" ) ]
            )
            (Ok [ "a", "j()", "g()", "h()", "b()", "e()", "f()", "i", "c()", "d", "k" ])
        , sortTest "Illegal cycle"
            [ ( "a()", "b" ), ( "b", "c" ), ( "c()", "a" ) ]
            (Err TopologicalSort.IllegalCycle)
        ]


sortTest : String -> List ( String, String ) -> Result TopologicalSort.SortError (List String) -> Test
sortTest name graph result =
    test name <|
        \_ ->
            let
                nodes : Dict String ( Int, String )
                nodes =
                    graph
                        |> List.indexedMap (\i ( n, r ) -> ( n, ( i + 1, r ) ))
                        |> Dict.fromList
            in
            TopologicalSort.sort
                { id = \n -> Dict.get n nodes |> Maybe.map Tuple.first |> Maybe.withDefault -1
                , defVars = String.toList >> Set.fromList >> Set.remove '(' >> Set.remove ')'
                , refVars = \n -> Dict.get n nodes |> Maybe.map (Tuple.second >> String.toList) |> Maybe.withDefault [] |> Set.fromList
                , cycleAllowed = String.endsWith "()"
                }
                (Dict.keys nodes)
                |> Expect.equal result
