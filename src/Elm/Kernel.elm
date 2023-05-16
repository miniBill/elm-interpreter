module Elm.Kernel exposing (functions)

import Bitwise
import FastDict as Dict exposing (Dict)
import Value exposing (EvalError(..), Value(..))


functions : Dict String ( Int, List Value -> Result EvalError Value )
functions =
    [ -- Elm.Kernel.Basics
      ( "Elm.Kernel.Basics.acos", oneFloat Float acos )
    , ( "Elm.Kernel.Basics.add", twoNumbers Int Float (+) (+) )
    , ( "Elm.Kernel.Basics.and", twoBools Bool (&&) )
    , ( "Elm.Kernel.Basics.asin", oneFloat Float asin )
    , ( "Elm.Kernel.Basics.atan", oneFloat Float atan )
    , ( "Elm.Kernel.Basics.atan2", twoFloats Float atan2 )
    , ( "Elm.Kernel.Basics.ceiling", oneFloat Int ceiling )
    , ( "Elm.Kernel.Basics.cos", oneFloat Float cos )
    , ( "Elm.Kernel.Basics.e", constant Float e )

    -- Elm.Kernel.Bitwise
    , ( "Elm.Kernel.Bitwise.and", twoInts Int Bitwise.and )
    , ( "Elm.Kernel.Bitwise.or", twoInts Int Bitwise.or )
    , ( "Elm.Kernel.Bitwise.xor", twoInts Int Bitwise.xor )

    -- Elm.Kernel.String
    , ( "Elm.Kernel.String.length", oneString Int String.length )
    , ( "Elm.Kernel.String.toFloat", oneString (maybe Float) String.toFloat )
    , ( "Elm.Kernel.String.toInt", oneString (maybe Int) String.toInt )
    , ( "Elm.Kernel.String.toLower", oneString String String.toLower )
    , ( "Elm.Kernel.String.toUpper", oneString String String.toUpper )
    ]
        |> Dict.fromList


maybe : (a -> Value) -> Maybe a -> Value
maybe f maybeValue =
    case maybeValue of
        Nothing ->
            Custom { moduleName = [ "Maybe" ], name = "Nothing" } []

        Just value ->
            Custom { moduleName = [ "Maybe" ], name = "Just" } [ f value ]


constant : (res -> Value) -> res -> ( Int, List Value -> Result EvalError Value )
constant toValue const =
    ( 0
    , \args ->
        case args of
            [] ->
                Ok <| toValue const

            _ ->
                Err <| TypeError <| "Didn't expect any args"
    )


oneString : (res -> Value) -> (String -> res) -> ( Int, List Value -> Result EvalError Value )
oneString toValue f =
    ( 1
    , \args ->
        case args of
            [ String s ] ->
                Ok <| toValue (f s)

            _ ->
                Err <| TypeError "Expected one String"
    )


oneFloat : (res -> Value) -> (Float -> res) -> ( Int, List Value -> Result EvalError Value )
oneFloat toValue f =
    ( 1
    , \args ->
        case args of
            [ Float n ] ->
                Ok <| toValue (f n)

            _ ->
                Err <| TypeError "Expected one Float"
    )


twoInts : (res -> Value) -> (Int -> Int -> res) -> ( Int, List Value -> Result EvalError Value )
twoInts toValue f =
    ( 2
    , \args ->
        case args of
            [ Int li, Int ri ] ->
                Ok <| toValue (f li ri)

            _ ->
                Err <| TypeError "Expected two Ints"
    )


twoFloats : (res -> Value) -> (Float -> Float -> res) -> ( Int, List Value -> Result EvalError Value )
twoFloats toValue f =
    ( 2
    , \args ->
        case args of
            [ Float li, Float ri ] ->
                Ok <| toValue (f li ri)

            _ ->
                Err <| TypeError "Expected two Floats"
    )


twoBools : (res -> Value) -> (Bool -> Bool -> res) -> ( Int, List Value -> Result EvalError Value )
twoBools toValue f =
    ( 2
    , \args ->
        case args of
            [ Bool lb, Bool rb ] ->
                Ok <| toValue (f lb rb)

            _ ->
                Err <| TypeError "Expected two Bools"
    )


twoNumbers :
    (Int -> Value)
    -> (Float -> Value)
    -> (Int -> Int -> Int)
    -> (Float -> Float -> Float)
    -> ( Int, List Value -> Result EvalError Value )
twoNumbers toValueInt toValueFloat fInt fFloat =
    ( 2
    , \args ->
        case args of
            [ Int li, Int ri ] ->
                Ok <| toValueInt (fInt li ri)

            [ Float lf, Float rf ] ->
                Ok <| toValueFloat (fFloat lf rf)

            _ ->
                Err <| TypeError "Expected two numbers"
    )
