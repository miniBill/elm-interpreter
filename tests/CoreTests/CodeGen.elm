module CoreTests.CodeGen exposing (suite)

import Test exposing (Test, describe)
import TestUtils exposing (evalTest, evalTest_)
import Value exposing (Value(..))


type Wrapper a
    = Wrapper a


caseUnderscore : Maybe number -> number
caseUnderscore m_ =
    case m_ of
        Just x ->
            x

        Nothing ->
            0


patternUnderscore : number
patternUnderscore =
    case Just 42 of
        Just x_ ->
            x_

        Nothing ->
            0


letQualified : number
letQualified =
    let
        (Wrapper x) =
            Wrapper 42
    in
    x


caseQualified : number
caseQualified =
    case Just 42 of
        Maybe.Just x ->
            x

        Nothing ->
            0


suite : Test
suite =
    describe "CodeGen"
        [ underscores
        , qualifiedPatterns
        , hex
        ]


underscores : Test
underscores =
    describe "Underscores"
        [ evalTest "case"
            """
let
    caseUnderscore : Maybe number -> number
    caseUnderscore m_ =
        case m_ of
            Just x ->
                x

            Nothing ->
                0
in caseUnderscore (Just 42)"""
            Int
            (caseUnderscore (Just 42))
        , evalTest "pattern"
            """
let
    patternUnderscore : number
    patternUnderscore =
        case Just 42 of
            Just x_ ->
                x_

            Nothing ->
                0
in
patternUnderscore"""
            Int
            patternUnderscore
        ]


qualifiedPatterns : Test
qualifiedPatterns =
    describe "Qualified Patterns"
        [ evalTest "let"
            """
let
    letQualified : number
    letQualified =
        let
            (Wrapper x) =
                Wrapper 42
        in
        x
in
letQualified"""
            Int
            letQualified
        , evalTest "case"
            """
let
    caseQualified : number
    caseQualified =
        case Just 42 of
            Maybe.Just x ->
                x

            Nothing ->
                0
in
caseQualified"""
            Int
            caseQualified
        ]


hex : Test
hex =
    describe "Hex"
        [ evalTest_ "0xFFFFFFFF" Int 0xFFFFFFFF
        , evalTest_ "0xD066F00D" Int 0xD066F00D
        , evalTest_ "0x00" Int 0x00
        ]
