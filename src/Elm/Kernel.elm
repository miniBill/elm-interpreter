module Elm.Kernel exposing (functions)

import Bitwise
import FastDict as Dict exposing (Dict)
import Maybe.Extra
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
    , ( "Elm.Kernel.Char.toLocaleLower", one char to char Char.toLocaleLower )
    , ( "Elm.Kernel.Char.toLocaleUpper", one char to char Char.toLocaleUpper )
    , ( "Elm.Kernel.Char.toLower", one char to char Char.toLower )
    , ( "Elm.Kernel.Char.toUpper", one char to char Char.toUpper )

    -- Elm.Kernel.Debug
    , ( "Elm.Kernel.Debug.log", two string anything to anything Debug.log )
    , ( "Elm.Kernel.Debug.toString", one anything to string Debug.toString )
    , ( "Elm.Kernel.Debug.todo", one string to anything Debug.todo )

    -- Elm.Kernel.List
    , ( "Elm.Kernel.List.cons", two anything (list anything) To (list anything) (::) )
    , ( "Elm.Kernel.List.fromArray", one anything To anything identity )
    , ( "Elm.Kernel.List.toArray", one anything To anything identity )

    -- Elm.Kernel.String
    , ( "Elm.Kernel.String.length", one string To int String.length )
    , ( "Elm.Kernel.String.toFloat", one string To (maybe float) String.toFloat )
    , ( "Elm.Kernel.String.toInt", one string To (maybe int) String.toInt )
    , ( "Elm.Kernel.String.toLower", one string to string String.toLower )
    , ( "Elm.Kernel.String.toUpper", one string to string String.toUpper )

    -- , ( "Elm.Kernel.String.all", one string to string String.all )
    -- , ( "Elm.Kernel.String.any", one string to string String.any )
    , ( "Elm.Kernel.String.append", two string string to string String.append )
    , ( "Elm.Kernel.String.cons", two char string to string String.cons )
    , ( "Elm.Kernel.String.contains", two string string to bool String.contains )
    , ( "Elm.Kernel.String.endsWith", two string string to bool String.endsWith )

    -- , ( "Elm.Kernel.String.filter", one string to string String.filter )
    -- , ( "Elm.Kernel.String.foldl", one string to string String.foldl )
    -- , ( "Elm.Kernel.String.foldr", one string to string String.foldr )
    , ( "Elm.Kernel.String.fromList", one (list char) to string String.fromList )
    , ( "Elm.Kernel.String.fromNumber"
      , one anything to string <|
            \s ->
                case s of
                    Int i ->
                        String.fromInt i

                    Float f ->
                        String.fromFloat f

                    _ ->
                        "TODO"
      )
    , ( "Elm.Kernel.String.indexes", two string string to (list int) String.indexes )
    , ( "Elm.Kernel.String.join", two string (list string) to string String.join )
    , ( "Elm.Kernel.String.lines", one string to (list string) String.lines )

    -- , ( "Elm.Kernel.String.map", one string to string String.map )
    , ( "Elm.Kernel.String.reverse", one string to string String.reverse )
    , ( "Elm.Kernel.String.slice", three int int string to string String.slice )
    , ( "Elm.Kernel.String.split", two string string to (list string) String.split )
    , ( "Elm.Kernel.String.startsWith", two string string to bool String.startsWith )
    , ( "Elm.Kernel.String.trim", one string to string String.trim )
    , ( "Elm.Kernel.String.trimLeft", one string to string String.trimLeft )
    , ( "Elm.Kernel.String.trimRight", one string to string String.trimRight )
    , ( "Elm.Kernel.String.uncons", one string to (maybe (tuple char string)) String.uncons )
    , ( "Elm.Kernel.String.words", one string to (list string) String.words )
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


anything : Selector Value
anything =
    ( Just
    , identity
    , "anything"
    )


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


list : Selector a -> Selector (List a)
list ( selector, toValue, name ) =
    ( \value -> value |> Value.toList |> Maybe.andThen (Maybe.Extra.traverse selector)
    , \value -> value |> List.map toValue |> Value.fromList
    , "List " ++ name
    )


tuple : Selector a -> Selector b -> Selector ( a, b )
tuple ( firstSelector, firstToValue, firstName ) ( secondSelector, secondToValue, secondName ) =
    ( \value ->
        case value of
            Tuple first second ->
                Maybe.map2 Tuple.pair (firstSelector first) (secondSelector second)

            _ ->
                Nothing
    , \( first, second ) ->
        Tuple (firstToValue first) (secondToValue second)
    , "( " ++ firstName ++ ", " ++ secondName ++ ")"
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


one :
    Selector a
    -> To
    -> Selector out
    -> (a -> out)
    -> ( Int, List Value -> Result EvalError Value )
one ( firstSelector, _, firstName ) To ( _, output, _ ) f =
    let
        err : String -> Result EvalError value
        err got =
            Err <| TypeError <| "Expected one " ++ firstName ++ ", got " ++ got
    in
    ( 1
    , \args ->
        case args of
            [ arg ] ->
                case firstSelector arg of
                    Just s ->
                        Ok <| output <| f s

                    Nothing ->
                        err <| Value.toString arg

            [] ->
                err "zero"

            _ ->
                err "more"
    )


two :
    Selector a
    -> Selector b
    -> To
    -> Selector out
    -> (a -> b -> out)
    -> ( Int, List Value -> Result EvalError Value )
two ( firstSelector, _, firstName ) ( secondSelector, _, secondName ) To ( _, output, _ ) f =
    let
        err : String -> Result EvalError value
        err got =
            if firstName == secondName then
                Err <| TypeError <| "Expected two " ++ firstName ++ "s, got " ++ got

            else
                Err <| TypeError <| "Expected one " ++ firstName ++ " and one " ++ secondName ++ ", got " ++ got
    in
    ( 2
    , \args ->
        case args of
            [ firstArg, secondArg ] ->
                case firstSelector firstArg of
                    Nothing ->
                        Err <| TypeError <| "Expected the first argument to be " ++ firstName ++ ", got " ++ Value.toString firstArg

                    Just first ->
                        case secondSelector secondArg of
                            Nothing ->
                                Err <| TypeError <| "Expected the second argument to be " ++ secondName ++ ", got " ++ Value.toString secondArg

                            Just second ->
                                Ok <| output <| f first second

            [] ->
                err "zero"

            _ ->
                err "more"
    )


three :
    Selector a
    -> Selector b
    -> Selector c
    -> To
    -> Selector out
    -> (a -> b -> c -> out)
    -> ( Int, List Value -> Result EvalError Value )
three ( firstSelector, _, firstName ) ( secondSelector, _, secondName ) ( thirdSelector, _, thirdName ) To ( _, output, _ ) f =
    let
        err : String -> Result EvalError value
        err got =
            if firstName == secondName && secondName == thirdName then
                Err <| TypeError <| "Expected three " ++ firstName ++ "s, got " ++ got

            else
                Err <| TypeError <| "Expected one " ++ firstName ++ ", one " ++ secondName ++ " and one " ++ thirdName ++ ", got " ++ got
    in
    ( 2
    , \args ->
        case args of
            [ firstArg, secondArg, thirdArg ] ->
                case ( firstSelector firstArg, secondSelector secondArg, thirdSelector thirdArg ) of
                    ( Just first, Just second, Just third ) ->
                        Ok <| output <| f first second third

                    _ ->
                        err (String.join ", " (List.map Value.toString args))

            [] ->
                err "zero"

            _ ->
                err (String.join ", " (List.map Value.toString args))
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
