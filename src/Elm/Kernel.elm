module Elm.Kernel exposing (functions)

import Array exposing (Array)
import Bitwise
import Elm.Syntax.ModuleName exposing (ModuleName)
import FastDict as Dict exposing (Dict)
import List.Extra
import Maybe.Extra
import Value exposing (EvalError(..), Value(..))


functions : Dict ModuleName (Dict String ( Int, List Value -> Result EvalError Value ))
functions =
    [ -- Elm.Kernel.Basics
      ( [ "Elm", "Kernel", "Basics" ]
      , [ ( "acos", one float to float acos )
        , ( "add", twoNumbers (+) (+) )
        , ( "and", two bool bool to bool (&&) )
        , ( "asin", one float to float asin )
        , ( "atan", one float to float atan )
        , ( "atan2", two float float to float atan2 )
        , ( "ceiling", one float to int ceiling )
        , ( "cos", one float to float cos )
        , ( "e", constant float e )
        , ( "fdiv", two float float to float (/) )
        , ( "floor", one float to int floor )
        , ( "idiv", two int int to int (//) )
        , ( "isInfinite", one float to bool isInfinite )
        , ( "isNaN", one float to bool isNaN )
        , ( "log", one float to float (logBase e) )
        , ( "modBy", two int int to int modBy )
        , ( "mul", twoNumbers (*) (*) )
        , ( "not", one bool to bool not )
        , ( "or", two bool bool to bool (||) )
        , ( "pi", constant float pi )
        , ( "pow", twoNumbers (^) (^) )
        , ( "remainderBy", two int int to int remainderBy )
        , ( "round", one float to int round )
        , ( "sin", one float to float sin )
        , ( "sqrt", one float to float sqrt )
        , ( "sub", twoNumbers (-) (-) )
        , ( "tan", one float to float tan )
        , ( "toFloat", one int to float toFloat )
        , ( "truncate", one float to int truncate )
        , ( "xor", two bool bool to bool xor )
        ]
      )

    -- Elm.Kernel.Bitwise
    , ( [ "Elm", "Kernel", "Bitwise" ]
      , [ ( "and", two int int to int Bitwise.and )
        , ( "complement", one int to int Bitwise.complement )
        , ( "or", two int int to int Bitwise.or )
        , ( "shiftLeftBy", two int int to int Bitwise.shiftLeftBy )
        , ( "shiftRightBy", two int int to int Bitwise.shiftRightBy )
        , ( "shiftRightZfBy", two int int to int Bitwise.shiftRightZfBy )
        , ( "xor", two int int to int Bitwise.xor )
        ]
      )

    -- Elm.Kernel.Char
    , ( [ "Elm", "Kernel", "Char" ]
      , [ ( "fromCode", one int to char Char.fromCode )
        , ( "toCode", one char to int Char.toCode )
        , ( "toLocaleLower", one char to char Char.toLocaleLower )
        , ( "toLocaleUpper", one char to char Char.toLocaleUpper )
        , ( "toLower", one char to char Char.toLower )
        , ( "toUpper", one char to char Char.toUpper )
        ]
      )

    -- Elm.Kernel.Debug
    , ( [ "Elm", "Kernel", "Debug" ]
      , [ ( "log", two string anything to anything Debug.log )
        , ( "toString", one anything to string Value.toString )
        , ( "todo", one string to anything Debug.todo )
        ]
      )

    -- Elm.Kernel.JsArray
    , ( [ "Elm", "Kernel", "JsArray" ]
      , [ ( "appendN", three int (array anything) (array anything) to (array anything) appendN )
        , ( "empty", zero to (array anything) Array.empty )
        , ( "initializeFromList", two int (list anything) to (tuple (array anything) (list anything)) initializeFromList )
        , ( "length", one (array anything) to int Array.length )
        ]
      )

    -- Elm.Kernel.List
    , ( [ "Elm", "Kernel", "List" ]
      , [ ( "cons", two anything (list anything) to (list anything) (::) )
        , ( "fromArray", one anything to anything identity )
        , ( "toArray", one anything to anything identity )
        ]
      )

    -- Elm.Kernel.String
    , ( [ "Elm", "Kernel", "String" ]
      , [ ( "length", one string to int String.length )
        , ( "toFloat", one string to (maybe float) String.toFloat )
        , ( "toInt", one string to (maybe int) String.toInt )
        , ( "toLower", one string to string String.toLower )
        , ( "toUpper", one string to string String.toUpper )

        -- , ( "all", one string to string String.all )
        -- , ( "any", one string to string String.any )
        , ( "append", two string string to string String.append )
        , ( "cons", two char string to string String.cons )
        , ( "contains", two string string to bool String.contains )
        , ( "endsWith", two string string to bool String.endsWith )

        -- , ( "filter", one string to string String.filter )
        -- , ( "foldl", one string to string String.foldl )
        -- , ( "foldr", one string to string String.foldr )
        , ( "fromList", one (list char) to string String.fromList )
        , ( "fromNumber", oneWithError anything to string fromNumber )
        , ( "indexes", two string string to (list int) String.indexes )
        , ( "join", two string (list string) to string String.join )
        , ( "lines", one string to (list string) String.lines )

        -- , ( "map", one string to string String.map )
        , ( "reverse", one string to string String.reverse )
        , ( "slice", three int int string to string String.slice )
        , ( "split", two string string to (list string) String.split )
        , ( "startsWith", two string string to bool String.startsWith )
        , ( "trim", one string to string String.trim )
        , ( "trimLeft", one string to string String.trimLeft )
        , ( "trimRight", one string to string String.trimRight )
        , ( "uncons", one string to (maybe (tuple char string)) String.uncons )
        , ( "words", one string to (list string) String.words )
        ]
      )
    , -- Elm.Kernel.Utils
      ( [ "Elm", "Kernel", "Utils" ]
      , [ ( "append", twoWithError anything anything to anything append )
        , ( "ge", comparison [ GT, EQ ] )
        , ( "gt", comparison [ GT ] )
        , ( "le", comparison [ LT, EQ ] )
        , ( "lt", comparison [ LT ] )
        , ( "equal", comparison [ EQ ] )
        , ( "compare", twoWithError anything anything to order compare )
        ]
      )
    ]
        |> List.map (\( moduleName, moduleFunctions ) -> ( moduleName, Dict.fromList moduleFunctions ))
        |> Dict.fromList



-- Selectors


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


order : Selector Order
order =
    ( Value.toOrder
    , Value.fromOrder
    , "Order"
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

            Int i ->
                -- Stuff like "2 / 3" is parsed as (Int 2) / (Int 3)
                Just (toFloat i)

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
    ( \value ->
        case value of
            List l ->
                Maybe.Extra.traverse selector l

            _ ->
                Nothing
    , \value ->
        value
            |> List.map toValue
            |> List
    , "List " ++ name
    )


array : Selector a -> Selector (Array a)
array ( selector, toValue, name ) =
    ( \value ->
        case value of
            Array l ->
                l
                    |> Array.toList
                    |> Maybe.Extra.traverse selector
                    |> Maybe.map Array.fromList

            _ ->
                Nothing
    , \value ->
        value
            |> Array.map toValue
            |> Array
    , "Array " ++ name
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


zero :
    To
    -> Selector out
    -> out
    -> ( Int, List Value -> Result EvalError Value )
zero To output f =
    zeroWithError To output (Ok f)


zeroWithError :
    To
    -> Selector out
    -> Result EvalError out
    -> ( Int, List Value -> Result EvalError Value )
zeroWithError To ( _, output, _ ) f =
    let
        err : String -> Result EvalError value
        err got =
            Err <| TypeError <| "Expected zero args, got " ++ got
    in
    ( 0
    , \args ->
        case args of
            [] ->
                Result.map output f

            _ ->
                err "more"
    )


one :
    Selector a
    -> To
    -> Selector out
    -> (a -> out)
    -> ( Int, List Value -> Result EvalError Value )
one firstSelector To output f =
    oneWithError firstSelector To output (\v -> Ok (f v))


oneWithError :
    Selector a
    -> To
    -> Selector out
    -> (a -> Result EvalError out)
    -> ( Int, List Value -> Result EvalError Value )
oneWithError ( firstSelector, _, firstName ) To ( _, output, _ ) f =
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
                        Result.map output <| f s

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
two firstSelector secondSelector To output f =
    twoWithError firstSelector secondSelector To output (\l r -> Ok (f l r))


twoWithError :
    Selector a
    -> Selector b
    -> To
    -> Selector out
    -> (a -> b -> Result EvalError out)
    -> ( Int, List Value -> Result EvalError Value )
twoWithError ( firstSelector, _, firstName ) ( secondSelector, _, secondName ) To ( _, output, _ ) f =
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
                                Result.map output <| f first second

            [] ->
                err "zero"

            _ ->
                err (String.join ", " <| List.map Value.toString args)
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

            [ Int li, Float rf ] ->
                Ok <| Float (fFloat (toFloat li) rf)

            [ Float lf, Int ri ] ->
                Ok <| Float (fFloat lf (toFloat ri))

            [ Float lf, Float rf ] ->
                Ok <| Float (fFloat lf rf)

            _ ->
                Err <| TypeError "Expected two numbers"
    )


comparison : List Order -> ( Int, List Value -> Result EvalError Value )
comparison orders =
    ( 2
    , \args ->
        case args of
            [ l, r ] ->
                Result.map (\result -> Bool (List.member result orders)) <| compare l r

            _ ->
                Err <| TypeError "Comparison needs exactly two arguments"
    )


compare : Value -> Value -> Result EvalError Order
compare l r =
    let
        inner : comparable -> comparable -> Result EvalError Order
        inner lv rv =
            Ok <| Basics.compare lv rv
    in
    case ( l, r ) of
        -- TODO: Implement all cases
        ( Int lv, Int rv ) ->
            inner lv rv

        ( Float lv, Float rv ) ->
            inner lv rv

        ( Int lv, Float rv ) ->
            inner (toFloat lv) rv

        ( Float lv, Int rv ) ->
            inner lv (toFloat rv)

        ( String lv, String rv ) ->
            inner lv rv

        ( Char lv, Char rv ) ->
            inner lv rv

        ( Tuple la lb, Tuple ra rb ) ->
            compare la ra
                |> Result.andThen
                    (\a ->
                        if a /= EQ then
                            Ok a

                        else
                            compare lb rb
                    )

        ( Triple la lb lc, Triple ra rb rc ) ->
            compare la ra
                |> Result.andThen
                    (\a ->
                        if a /= EQ then
                            Ok a

                        else
                            compare lb rb
                                |> Result.andThen
                                    (\b ->
                                        if b /= EQ then
                                            Ok b

                                        else
                                            compare lc rc
                                    )
                    )

        ( List [], List (_ :: _) ) ->
            Ok LT

        ( List (_ :: _), List [] ) ->
            Ok GT

        ( List [], List [] ) ->
            Ok EQ

        ( List (lh :: lt), List (rh :: rt) ) ->
            compare lh rh
                |> Result.andThen
                    (\h ->
                        if h /= EQ then
                            Ok h

                        else
                            compare (List lt) (List rt)
                    )

        _ ->
            Err <| TypeError <| "Comparison not yet implemented for " ++ Value.toString l ++ " and " ++ Value.toString r



--


append : Value -> Value -> Result EvalError Value
append l r =
    case ( l, r ) of
        ( String ls, String rs ) ->
            Ok <| String (ls ++ rs)

        ( List ll, List rl ) ->
            Ok <| List (ll ++ rl)

        _ ->
            Err <| TypeError <| "Cannot append " ++ Value.toString l ++ " and " ++ Value.toString r


fromNumber : Value -> Result EvalError String
fromNumber s =
    case s of
        Int i ->
            Ok <| String.fromInt i

        Float f ->
            Ok <| String.fromFloat f

        _ ->
            Err <| TypeError <| "Cannot convert " ++ Value.toString s ++ " to a number"


appendN : Int -> Array Value -> Array Value -> Array Value
appendN n dest source =
    let
        itemsToCopy : Int
        itemsToCopy =
            min (Array.length source) (n - Array.length dest)
    in
    Array.append
        dest
        (Array.slice 0 itemsToCopy source)


initializeFromList : Int -> List Value -> ( Array Value, List Value )
initializeFromList n values =
    let
        ( before, after ) =
            List.Extra.splitAt n values
    in
    ( Array.fromList before, after )
