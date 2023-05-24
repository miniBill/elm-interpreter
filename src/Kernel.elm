module Kernel exposing (functions)

import Array exposing (Array)
import Bitwise
import Elm.Syntax.ModuleName exposing (ModuleName)
import FastDict as Dict exposing (Dict)
import List.Extra
import Maybe.Extra
import Value exposing (Env, EvalResult, Value(..), typeError)


functions : Dict ModuleName (Dict String ( Int, Env -> List Value -> EvalResult Value ))
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
      , [ ( "appendN", three int (jsArray anything) (jsArray anything) to (jsArray anything) appendN )
        , ( "empty", zero to (jsArray anything) Array.empty )
        , ( "initializeFromList", two int (list anything) to (tuple (jsArray anything) (list anything)) initializeFromList )
        , ( "length", one (jsArray anything) to int Array.length )
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


jsArray : Selector a -> Selector (Array a)
jsArray ( selector, toValue, name ) =
    ( \value ->
        case value of
            JSArray jsa ->
                jsa
                    |> Array.toList
                    |> Maybe.Extra.traverse selector
                    |> Maybe.map Array.fromList

            _ ->
                Nothing
    , \array ->
        array
            |> Array.map toValue
            |> JSArray
    , "JSArray " ++ name
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


constant : Selector res -> res -> ( Int, Env -> List Value -> EvalResult Value )
constant ( _, toValue, _ ) const =
    ( 0
    , \env args ->
        case args of
            [] ->
                Ok <| toValue const

            _ ->
                typeError env <| "Didn't expect any args"
    )


zero :
    To
    -> Selector out
    -> out
    -> ( Int, Env -> List Value -> EvalResult Value )
zero _ output f =
    zeroWithError To output (Ok f)


zeroWithError :
    To
    -> Selector out
    -> EvalResult out
    -> ( Int, Env -> List Value -> EvalResult Value )
zeroWithError _ ( _, output, _ ) f =
    let
        err : Env -> String -> EvalResult value
        err env got =
            typeError env <| "Expected zero args, got " ++ got
    in
    ( 0
    , \env args ->
        case args of
            [] ->
                Result.map output f

            _ ->
                err env "more"
    )


one :
    Selector a
    -> To
    -> Selector out
    -> (a -> out)
    -> ( Int, Env -> List Value -> EvalResult Value )
one firstSelector _ output f =
    oneWithError firstSelector To output (\_ v -> Ok (f v))


oneWithError :
    Selector a
    -> To
    -> Selector out
    -> (Env -> a -> EvalResult out)
    -> ( Int, Env -> List Value -> EvalResult Value )
oneWithError ( firstSelector, _, firstName ) _ ( _, output, _ ) f =
    let
        err : Env -> String -> EvalResult value
        err env got =
            typeError env <| "Expected one " ++ firstName ++ ", got " ++ got
    in
    ( 1
    , \env args ->
        case args of
            [ arg ] ->
                case firstSelector arg of
                    Just s ->
                        Result.map output <| f env s

                    Nothing ->
                        err env <| Value.toString arg

            [] ->
                err env "zero"

            _ ->
                err env "more"
    )


two :
    Selector a
    -> Selector b
    -> To
    -> Selector out
    -> (a -> b -> out)
    -> ( Int, Env -> List Value -> EvalResult Value )
two firstSelector secondSelector _ output f =
    twoWithError firstSelector secondSelector To output (\_ l r -> Ok (f l r))


twoWithError :
    Selector a
    -> Selector b
    -> To
    -> Selector out
    -> (Env -> a -> b -> EvalResult out)
    -> ( Int, Env -> List Value -> EvalResult Value )
twoWithError ( firstSelector, _, firstName ) ( secondSelector, _, secondName ) _ ( _, output, _ ) f =
    let
        err : Env -> String -> EvalResult value
        err env got =
            if firstName == secondName then
                typeError env <| "Expected two " ++ firstName ++ "s, got " ++ got

            else
                typeError env <| "Expected one " ++ firstName ++ " and one " ++ secondName ++ ", got " ++ got
    in
    ( 2
    , \env args ->
        case args of
            [ firstArg, secondArg ] ->
                case firstSelector firstArg of
                    Nothing ->
                        typeError env <| "Expected the first argument to be " ++ firstName ++ ", got " ++ Value.toString firstArg

                    Just first ->
                        case secondSelector secondArg of
                            Nothing ->
                                typeError env <| "Expected the second argument to be " ++ secondName ++ ", got " ++ Value.toString secondArg

                            Just second ->
                                Result.map output <| f env first second

            [] ->
                err env "zero"

            _ ->
                err env <| String.join ", " <| List.map Value.toString args
    )


three :
    Selector a
    -> Selector b
    -> Selector c
    -> To
    -> Selector out
    -> (a -> b -> c -> out)
    -> ( Int, Env -> List Value -> EvalResult Value )
three firstSelector secondSelector thirdSelector _ output f =
    threeWithError firstSelector secondSelector thirdSelector To output (\_ l m r -> Ok (f l m r))


threeWithError :
    Selector a
    -> Selector b
    -> Selector c
    -> To
    -> Selector out
    -> (Env -> a -> b -> c -> EvalResult out)
    -> ( Int, Env -> List Value -> EvalResult Value )
threeWithError ( firstSelector, _, firstName ) ( secondSelector, _, secondName ) ( thirdSelector, _, thirdName ) _ ( _, output, _ ) f =
    let
        err : Env -> String -> EvalResult value
        err env got =
            if firstName == secondName && secondName == thirdName then
                typeError env <| "Expected three " ++ firstName ++ "s, got " ++ got

            else
                typeError env <| "Expected one " ++ firstName ++ ", one " ++ secondName ++ " and one " ++ thirdName ++ ", got " ++ got
    in
    ( 3
    , \env args ->
        case args of
            [ firstArg, secondArg, thirdArg ] ->
                case ( firstSelector firstArg, secondSelector secondArg, thirdSelector thirdArg ) of
                    ( Just first, Just second, Just third ) ->
                        Result.map output <| f env first second third

                    _ ->
                        err env <| String.join ", " (List.map Value.toString args)

            [] ->
                err env "zero"

            _ ->
                err env <| "[ " ++ String.join ", " (List.map Value.toString args) ++ " ]"
    )


twoNumbers :
    (Int -> Int -> Int)
    -> (Float -> Float -> Float)
    -> ( Int, Env -> List Value -> EvalResult Value )
twoNumbers fInt fFloat =
    ( 2
    , \env args ->
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
                typeError env "Expected two numbers"
    )


comparison : List Order -> ( Int, Env -> List Value -> EvalResult Value )
comparison orders =
    ( 2
    , \env args ->
        case args of
            [ l, r ] ->
                Result.map (\result -> Bool (List.member result orders)) <| compare env l r

            _ ->
                typeError env "Comparison needs exactly two arguments"
    )


compare : Env -> Value -> Value -> EvalResult Order
compare env l r =
    let
        inner : comparable -> comparable -> EvalResult Order
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
            compare env la ra
                |> Result.andThen
                    (\a ->
                        if a /= EQ then
                            Ok a

                        else
                            compare env lb rb
                    )

        ( Triple la lb lc, Triple ra rb rc ) ->
            compare env la ra
                |> Result.andThen
                    (\a ->
                        if a /= EQ then
                            Ok a

                        else
                            compare env lb rb
                                |> Result.andThen
                                    (\b ->
                                        if b /= EQ then
                                            Ok b

                                        else
                                            compare env lc rc
                                    )
                    )

        ( List [], List (_ :: _) ) ->
            Ok LT

        ( List (_ :: _), List [] ) ->
            Ok GT

        ( List [], List [] ) ->
            Ok EQ

        ( List (lh :: lt), List (rh :: rt) ) ->
            compare env lh rh
                |> Result.andThen
                    (\h ->
                        if h /= EQ then
                            Ok h

                        else
                            compare env (List lt) (List rt)
                    )

        _ ->
            typeError env <| "Comparison not yet implemented for " ++ Value.toString l ++ " and " ++ Value.toString r



--


append : Env -> Value -> Value -> EvalResult Value
append env l r =
    case ( l, r ) of
        ( String ls, String rs ) ->
            Ok <| String (ls ++ rs)

        ( List ll, List rl ) ->
            Ok <| List (ll ++ rl)

        _ ->
            typeError env <| "Cannot append " ++ Value.toString l ++ " and " ++ Value.toString r


fromNumber : Env -> Value -> EvalResult String
fromNumber env s =
    case s of
        Int i ->
            Ok <| String.fromInt i

        Float f ->
            Ok <| String.fromFloat f

        _ ->
            typeError env <| "Cannot convert " ++ Value.toString s ++ " to a number"


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
