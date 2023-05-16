module Elm.Kernel exposing (functions)

import Bitwise
import FastDict as Dict exposing (Dict)
import Value exposing (EvalError(..), Value(..))


functions : Dict String ( Int, List Value -> Result EvalError Value )
functions =
    [ -- Elm.Kernel.Basics
      ( "Elm.Kernel.Basics.acos", one float to float acos )
    , ( "Elm.Kernel.Basics.add", twoNumbers (+) (+) )
    , ( "Elm.Kernel.Basics.and", two bool bool to bool (&&) )
    , ( "Elm.Kernel.Basics.asin", one float to float asin )
    , ( "Elm.Kernel.Basics.atan", one float to float atan )
    , ( "Elm.Kernel.Basics.atan2", two float float to float atan2 )
    , ( "Elm.Kernel.Basics.ceiling", one float to int ceiling )
    , ( "Elm.Kernel.Basics.cos", one float to float cos )
    , ( "Elm.Kernel.Basics.e", constant float e )
    , ( "Elm.Kernel.Basics.fdiv", two float float to float (/) )
    , ( "Elm.Kernel.Basics.floor", one float to int floor )
    , ( "Elm.Kernel.Basics.idiv", two int int to int (//) )
    , ( "Elm.Kernel.Basics.isInfinite", one float to bool isInfinite )
    , ( "Elm.Kernel.Basics.isNaN", one float to bool isNaN )
    , ( "Elm.Kernel.Basics.log", one float to float (logBase e) )
    , ( "Elm.Kernel.Basics.modBy", two int int to int modBy )
    , ( "Elm.Kernel.Basics.mul", twoNumbers (*) (*) )
    , ( "Elm.Kernel.Basics.not", one bool To bool not )
    , ( "Elm.Kernel.Basics.or", two bool bool to bool (||) )
    , ( "Elm.Kernel.Basics.pi", constant float pi )
    , ( "Elm.Kernel.Basics.pow", twoNumbers (^) (^) )
    , ( "Elm.Kernel.Basics.remainderBy", two int int to int remainderBy )
    , ( "Elm.Kernel.Basics.round", one float to int round )
    , ( "Elm.Kernel.Basics.sin", one float to float sin )
    , ( "Elm.Kernel.Basics.sqrt", one float to float sqrt )
    , ( "Elm.Kernel.Basics.sub", twoNumbers (-) (-) )
    , ( "Elm.Kernel.Basics.tan", one float to float tan )
    , ( "Elm.Kernel.Basics.toFloat", one int To float toFloat )
    , ( "Elm.Kernel.Basics.truncate", one float to int truncate )
    , ( "Elm.Kernel.Basics.xor", two bool bool to bool xor )

    -- Elm.Kernel.Bitwise
    , ( "Elm.Kernel.Bitwise.and", two int int to int Bitwise.and )
    , ( "Elm.Kernel.Bitwise.complement", one int to int Bitwise.complement )
    , ( "Elm.Kernel.Bitwise.or", two int int to int Bitwise.or )
    , ( "Elm.Kernel.Bitwise.shiftLeftBy", two int int to int Bitwise.shiftLeftBy )
    , ( "Elm.Kernel.Bitwise.shiftRightBy", two int int to int Bitwise.shiftRightBy )
    , ( "Elm.Kernel.Bitwise.shiftRightZfBy", two int int to int Bitwise.shiftRightZfBy )
    , ( "Elm.Kernel.Bitwise.xor", two int int to int Bitwise.xor )

    -- Elm.Kernel.Char
    , ( "Elm.Kernel.Char.fromCode", one int to char Char.fromCode )
    , ( "Elm.Kernel.Char.toCode", one char to int Char.toCode )

    -- Elm.Kernel.String
    , ( "Elm.Kernel.String.length", one string To int String.length )
    , ( "Elm.Kernel.String.toFloat", one string To (maybe float) String.toFloat )
    , ( "Elm.Kernel.String.toInt", one string To (maybe int) String.toInt )
    , ( "Elm.Kernel.String.toLower", one string to string String.toLower )
    , ( "Elm.Kernel.String.toUpper", one string to string String.toUpper )
    ]
        |> Dict.fromList


type alias Selector a =
    ( Value -> Maybe a
    , a -> Value
    , String
    )


type To
    = To


to : To
to =
    To


string : Selector String
string =
    ( \value ->
        case value of
            String s ->
                Just s

            _ ->
                Nothing
    , String
    , "String"
    )


float : Selector Float
float =
    ( \value ->
        case value of
            Float s ->
                Just s

            _ ->
                Nothing
    , Float
    , "Float"
    )


int : Selector Int
int =
    ( \value ->
        case value of
            Int s ->
                Just s

            _ ->
                Nothing
    , Int
    , "Int"
    )


char : Selector Char
char =
    ( \value ->
        case value of
            Char s ->
                Just s

            _ ->
                Nothing
    , Char
    , "Char"
    )


bool : Selector Bool
bool =
    ( \value ->
        case value of
            Bool s ->
                Just s

            _ ->
                Nothing
    , Bool
    , "Bool"
    )


maybe : Selector a -> Selector (Maybe a)
maybe ( selector, toValue, name ) =
    ( \value ->
        case value of
            Custom ctor args ->
                case ( ctor.moduleName, ctor.name, args ) of
                    ( [ "Maybe" ], "Nothing", [] ) ->
                        Just Nothing

                    ( [ "Maybe" ], "Just", [ arg ] ) ->
                        Maybe.map Just (selector arg)

                    _ ->
                        Nothing

            _ ->
                Nothing
    , \maybeValue ->
        case maybeValue of
            Nothing ->
                Custom { moduleName = [ "Maybe" ], name = "Nothing" } []

            Just value ->
                Custom { moduleName = [ "Maybe" ], name = "Just" } [ toValue value ]
    , "Maybe " ++ name
    )


constant : Selector res -> res -> ( Int, List Value -> Result EvalError Value )
constant ( _, toValue, _ ) const =
    ( 0
    , \args ->
        case args of
            [] ->
                Ok <| toValue const

            _ ->
                Err <| TypeError <| "Didn't expect any args"
    )


one : Selector a -> To -> Selector out -> (a -> out) -> ( Int, List Value -> Result EvalError Value )
one ( firstSelector, _, firstName ) To ( _, output, _ ) f =
    ( 1
    , \args ->
        case args of
            [ arg ] ->
                case firstSelector arg of
                    Just s ->
                        Ok <| output <| f s

                    Nothing ->
                        Err <| TypeError <| "Expected one " ++ firstName

            _ ->
                Err <| TypeError <| "Expected one " ++ firstName
    )


two : Selector a -> Selector b -> To -> Selector out -> (a -> b -> out) -> ( Int, List Value -> Result EvalError Value )
two ( firstSelector, _, firstName ) ( secondSelector, _, secondName ) To ( _, output, _ ) f =
    let
        err : () -> Result EvalError value
        err () =
            if firstName == secondName then
                Err <| TypeError <| "Expected two " ++ firstName ++ "s"

            else
                Err <| TypeError <| "Expected one " ++ firstName ++ " and one " ++ secondName
    in
    ( 2
    , \args ->
        case args of
            [ firstArg, secondArg ] ->
                case ( firstSelector firstArg, secondSelector secondArg ) of
                    ( Just first, Just second ) ->
                        Ok <| output <| f first second

                    _ ->
                        err ()

            _ ->
                err ()
    )


twoNumbers :
    (Int -> Int -> Int)
    -> (Float -> Float -> Float)
    -> ( Int, List Value -> Result EvalError Value )
twoNumbers fInt fFloat =
    ( 2
    , \args ->
        case args of
            [ Int li, Int ri ] ->
                Ok <| Int (fInt li ri)

            [ Float lf, Float rf ] ->
                Ok <| Float (fFloat lf rf)

            _ ->
                Err <| TypeError "Expected two numbers"
    )
