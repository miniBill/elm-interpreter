module CoreTests.Maybe exposing (suite)

import Test exposing (Test, describe)
import TestUtils exposing (evalTest, maybe)
import Types exposing (Value(..))


suite : Test
suite =
    describe "Maybe Tests"
        [ describe "Common Helpers Tests"
            [ describe "withDefault Tests"
                [ evalTest "no default used"
                    "Maybe.withDefault 5 (Just 0)"
                    Int
                    (Maybe.withDefault 5 (Just 0))
                , evalTest "default used"
                    "Maybe.withDefault 5 Nothing"
                    Int
                    (Maybe.withDefault 5 Nothing)
                ]
            , describe "map Tests"
                (let
                    f : number -> number
                    f =
                        \n -> n + 1
                 in
                 [ evalTest "on Just"
                    "let f n = n + 1 in Maybe.map f (Just 0)"
                    (maybe Int)
                    (Maybe.map f (Just 0))
                 , evalTest "on Nothing"
                    "let f n = n + 1 in Maybe.map f Nothing"
                    (maybe Int)
                    (Maybe.map f Nothing)
                 ]
                )
            , describe "map2 Tests"
                (let
                    f : number -> number -> number
                    f =
                        (+)
                 in
                 [ evalTest "on (Just, Just)"
                    "let f = (+) in Maybe.map2 f (Just 0) (Just 1)"
                    (maybe Int)
                    (Maybe.map2 f (Just 0) (Just 1))
                 , evalTest "on (Just, Nothing)"
                    "let f = (+) in Maybe.map2 f (Just 0) Nothing"
                    (maybe Int)
                    (Maybe.map2 f (Just 0) Nothing)
                 , evalTest "on (Nothing, Just)"
                    "let f = (+) in Maybe.map2 f Nothing (Just 0)"
                    (maybe Int)
                    (Maybe.map2 f Nothing (Just 0))
                 ]
                )
            , describe "map3 Tests"
                (let
                    f : number -> number -> number -> number
                    f =
                        \a b c -> a + b + c
                 in
                 [ evalTest "on (Just, Just, Just)"
                    "let f a b c = a + b + c in Maybe.map3 f (Just 1) (Just 1) (Just 1)"
                    (maybe Int)
                    (Maybe.map3 f (Just 1) (Just 1) (Just 1))
                 , evalTest "on (Just, Just, Nothing)"
                    "let f a b c = a + b + c in Maybe.map3 f (Just 1) (Just 1) Nothing"
                    (maybe Int)
                    (Maybe.map3 f (Just 1) (Just 1) Nothing)
                 , evalTest "on (Just, Nothing, Just)"
                    "let f a b c = a + b + c in Maybe.map3 f (Just 1) Nothing (Just 1)"
                    (maybe Int)
                    (Maybe.map3 f (Just 1) Nothing (Just 1))
                 , evalTest "on (Nothing, Just, Just)"
                    "let f a b c = a + b + c in Maybe.map3 f Nothing (Just 1) (Just 1)"
                    (maybe Int)
                    (Maybe.map3 f Nothing (Just 1) (Just 1))
                 ]
                )
            , describe "map4 Tests"
                (let
                    f : number -> number -> number -> number -> number
                    f =
                        \a b c d -> a + b + c + d
                 in
                 [ evalTest "on (Just, Just, Just, Just)"
                    "let f a b c d = a + b + c + d in Maybe.map4 f (Just 1) (Just 1) (Just 1) (Just 1)"
                    (maybe Int)
                    (Maybe.map4 f (Just 1) (Just 1) (Just 1) (Just 1))
                 , evalTest "on (Just, Just, Just, Nothing)"
                    "let f a b c d = a + b + c + d in Maybe.map4 f (Just 1) (Just 1) (Just 1) Nothing"
                    (maybe Int)
                    (Maybe.map4 f (Just 1) (Just 1) (Just 1) Nothing)
                 , evalTest "on (Just, Just, Nothing, Just)"
                    "let f a b c d = a + b + c + d in Maybe.map4 f (Just 1) (Just 1) Nothing (Just 1)"
                    (maybe Int)
                    (Maybe.map4 f (Just 1) (Just 1) Nothing (Just 1))
                 , evalTest "on (Just, Nothing, Just, Just)"
                    "let f a b c d = a + b + c + d in Maybe.map4 f (Just 1) Nothing (Just 1) (Just 1)"
                    (maybe Int)
                    (Maybe.map4 f (Just 1) Nothing (Just 1) (Just 1))
                 , evalTest "on (Nothing, Just, Just, Just)"
                    "let f a b c d = a + b + c + d in Maybe.map4 f Nothing (Just 1) (Just 1) (Just 1)"
                    (maybe Int)
                    (Maybe.map4 f Nothing (Just 1) (Just 1) (Just 1))
                 ]
                )
            , describe "map5 Tests"
                (let
                    f : number -> number -> number -> number -> number -> number
                    f =
                        \a b c d e -> a + b + c + d + e
                 in
                 [ evalTest "on (Just, Just, Just, Just, Just)"
                    "let f a b c d e = a + b + c + d + e in Maybe.map5 f (Just 1) (Just 1) (Just 1) (Just 1) (Just 1)"
                    (maybe Int)
                    (Maybe.map5 f (Just 1) (Just 1) (Just 1) (Just 1) (Just 1))
                 , evalTest "on (Just, Just, Just, Just, Nothing)"
                    "let f a b c d e = a + b + c + d + e in Maybe.map5 f (Just 1) (Just 1) (Just 1) (Just 1) Nothing"
                    (maybe Int)
                    (Maybe.map5 f (Just 1) (Just 1) (Just 1) (Just 1) Nothing)
                 , evalTest "on (Just, Just, Just, Nothing, Just)"
                    "let f a b c d e = a + b + c + d + e in Maybe.map5 f (Just 1) (Just 1) (Just 1) Nothing (Just 1)"
                    (maybe Int)
                    (Maybe.map5 f (Just 1) (Just 1) (Just 1) Nothing (Just 1))
                 , evalTest "on (Just, Just, Nothing, Just, Just)"
                    "let f a b c d e = a + b + c + d + e in Maybe.map5 f (Just 1) (Just 1) Nothing (Just 1) (Just 1)"
                    (maybe Int)
                    (Maybe.map5 f (Just 1) (Just 1) Nothing (Just 1) (Just 1))
                 , evalTest "on (Just, Nothing, Just, Just, Just)"
                    "let f a b c d e = a + b + c + d + e in Maybe.map5 f (Just 1) Nothing (Just 1) (Just 1) (Just 1)"
                    (maybe Int)
                    (Maybe.map5 f (Just 1) Nothing (Just 1) (Just 1) (Just 1))
                 , evalTest "on (Nothing, Just, Just, Just, Just)"
                    "let f a b c d e = a + b + c + d + e in Maybe.map5 f Nothing (Just 1) (Just 1) (Just 1) (Just 1)"
                    (maybe Int)
                    (Maybe.map5 f Nothing (Just 1) (Just 1) (Just 1) (Just 1))
                 ]
                )
            ]
        , describe "Chaining Maybes Tests"
            [ describe "andThen Tests"
                [ evalTest "succeeding chain"
                    "Maybe.andThen (\\a -> Just a) (Just 1)"
                    (maybe Int)
                    (Maybe.andThen (\a -> Just a) (Just 1))
                , evalTest "failing chain (original Maybe failed)"
                    "Maybe.andThen (\\a -> Just a) Nothing"
                    (maybe Int)
                    (Maybe.andThen (\a -> Just a) Nothing)
                , evalTest "failing chain (chained function failed)"
                    "Maybe.andThen (\\a -> Nothing) (Just 1)"
                    (maybe Int)
                    (Maybe.andThen (\_ -> Nothing) (Just 1))
                ]
            ]
        ]
