module Kernel exposing (EvalFunction, functions)

import Array exposing (Array)
import Bitwise
import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node exposing (Node)
import Elm.Syntax.Pattern as Pattern exposing (QualifiedNameRef)
import Eval.Types exposing (Eval)
import FastDict as Dict exposing (Dict)
import Kernel.JsArray
import Kernel.List
import Kernel.String
import Kernel.Utils
import Maybe.Extra
import Value exposing (EvalResult, Value(..), typeError)


type alias EvalFunction =
    List Value
    -> List (Node Pattern.Pattern)
    -> Maybe QualifiedNameRef
    -> Node Expression
    -> Eval Value


functions : EvalFunction -> Dict ModuleName (Dict String ( Int, List Value -> Eval Value ))
functions evalFunction =
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
      , [ ( "appendN", three int (jsArray anything) (jsArray anything) to (jsArray anything) Kernel.JsArray.appendN )
        , ( "empty", zero to (jsArray anything) Array.empty )
        , ( "foldr", threeWithError (function2 evalFunction anything anything to anything) anything (jsArray anything) to anything Kernel.JsArray.foldr )
        , ( "initialize", threeWithError int int (function evalFunction int to anything) to (jsArray anything) Kernel.JsArray.initialize )
        , ( "initializeFromList", two int (list anything) to (tuple (jsArray anything) (list anything)) Kernel.JsArray.initializeFromList )
        , ( "length", one (jsArray anything) to int Array.length )
        ]
      )

    -- Elm.Kernel.List
    , ( [ "Elm", "Kernel", "List" ]
      , [ ( "cons", two anything (list anything) to (list anything) (::) )
        , ( "fromArray", one anything to anything identity )
        , ( "sortBy", twoWithError (function evalFunction anything to anything) (list anything) to (list anything) Kernel.List.sortBy )
        , ( "sortWith", twoWithError (function2 evalFunction anything anything to order) (list anything) to (list anything) Kernel.List.sortWith )
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
        , ( "fromNumber", oneWithError anything to string <| \s _ env -> ( Kernel.String.fromNumber env s, [] ) )
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
      , [ ( "append", twoWithError anything anything to anything Kernel.Utils.append )
        , ( "ge", Kernel.Utils.comparison [ GT, EQ ] )
        , ( "gt", Kernel.Utils.comparison [ GT ] )
        , ( "le", Kernel.Utils.comparison [ LT, EQ ] )
        , ( "lt", Kernel.Utils.comparison [ LT ] )
        , ( "equal", Kernel.Utils.comparison [ EQ ] )
        , ( "compare", twoWithError anything anything to order <| \l r _ env -> ( Kernel.Utils.compare l r env, [] ) )
        ]
      )
    ]
        |> List.map (\( moduleName, moduleFunctions ) -> ( moduleName, Dict.fromList moduleFunctions ))
        |> Dict.fromList



-- Selectors


type alias InSelector a x =
    { x
        | fromValue : Value -> Maybe a
        , name : String
    }


type alias OutSelector a x =
    { x
        | toValue : a -> Value
        , name : String
    }


type alias Selector a =
    InSelector a (OutSelector a {})


type To
    = To


to : To
to =
    To


anything : Selector Value
anything =
    { fromValue = Just
    , toValue = identity
    , name = "anything"
    }


order : Selector Order
order =
    { fromValue = Value.toOrder
    , toValue = Value.fromOrder
    , name = "Order"
    }


string : Selector String
string =
    { fromValue =
        \value ->
            case value of
                String s ->
                    Just s

                _ ->
                    Nothing
    , toValue = String
    , name = "String"
    }


float : Selector Float
float =
    { fromValue =
        \value ->
            case value of
                Float s ->
                    Just s

                Int i ->
                    -- Stuff like "2 / 3" is parsed as (Int 2) / (Int 3)
                    Just (toFloat i)

                _ ->
                    Nothing
    , toValue = Float
    , name = "Float"
    }


int : Selector Int
int =
    { fromValue =
        \value ->
            case value of
                Int s ->
                    Just s

                _ ->
                    Nothing
    , toValue = Int
    , name = "Int"
    }


char : Selector Char
char =
    { fromValue =
        \value ->
            case value of
                Char s ->
                    Just s

                _ ->
                    Nothing
    , toValue = Char
    , name = "Char"
    }


bool : Selector Bool
bool =
    { fromValue =
        \value ->
            case value of
                Bool s ->
                    Just s

                _ ->
                    Nothing
    , toValue = Bool
    , name = "Bool"
    }


maybe : Selector a -> Selector (Maybe a)
maybe selector =
    { fromValue =
        \value ->
            case value of
                Custom ctor args ->
                    case ( ctor.moduleName, ctor.name, args ) of
                        ( [ "Maybe" ], "Nothing", [] ) ->
                            Just Nothing

                        ( [ "Maybe" ], "Just", [ arg ] ) ->
                            Maybe.map Just (selector.fromValue arg)

                        _ ->
                            Nothing

                _ ->
                    Nothing
    , toValue =
        \maybeValue ->
            case maybeValue of
                Nothing ->
                    Custom { moduleName = [ "Maybe" ], name = "Nothing" } []

                Just value ->
                    Custom { moduleName = [ "Maybe" ], name = "Just" } [ selector.toValue value ]
    , name = "Maybe " ++ selector.name
    }


list : Selector a -> Selector (List a)
list selector =
    { fromValue =
        \value ->
            case value of
                List l ->
                    Maybe.Extra.traverse selector.fromValue l

                _ ->
                    Nothing
    , toValue =
        \value ->
            value
                |> List.map selector.toValue
                |> List
    , name = "List " ++ selector.name
    }


jsArray : Selector a -> Selector (Array a)
jsArray selector =
    { fromValue =
        \value ->
            case value of
                JsArray jsa ->
                    jsa
                        |> Array.toList
                        |> Maybe.Extra.traverse selector.fromValue
                        |> Maybe.map Array.fromList

                _ ->
                    Nothing
    , toValue =
        \array ->
            array
                |> Array.map selector.toValue
                |> JsArray
    , name = "JsArray " ++ selector.name
    }


function :
    EvalFunction
    -> OutSelector from xf
    -> To
    -> InSelector to xt
    -> InSelector (from -> Eval to) {}
function evalFunctionWith inSelector _ outSelector =
    let
        fromValue : Value -> Maybe (from -> Eval to)
        fromValue value =
            case value of
                PartiallyApplied localEnv oldArgs patterns maybeName implementation ->
                    Just
                        (\arg cfg _ ->
                            case evalFunctionWith (oldArgs ++ [ inSelector.toValue arg ]) patterns maybeName implementation cfg localEnv of
                                ( Err e, callTree ) ->
                                    ( Err e, callTree )

                                ( Ok out, callTree ) ->
                                    case outSelector.fromValue out of
                                        Just ov ->
                                            ( Ok ov, callTree )

                                        Nothing ->
                                            ( Err <|
                                                typeError localEnv <|
                                                    "Could not convert output from "
                                                        ++ Value.toString out
                                                        ++ " to "
                                                        ++ outSelector.name
                                            , callTree
                                            )
                        )

                _ ->
                    Nothing
    in
    { name = inSelector.name ++ " -> " ++ outSelector.name
    , fromValue = fromValue
    }


function2 :
    EvalFunction
    -> OutSelector a xa
    -> OutSelector b xb
    -> To
    -> InSelector to xt
    -> InSelector (a -> Eval (b -> Eval to)) {}
function2 evalFunction in1Selector in2Selector _ outSelector =
    function evalFunction in1Selector to (function evalFunction in2Selector to outSelector)


tuple : Selector a -> Selector b -> Selector ( a, b )
tuple firstSelector secondSelector =
    { fromValue =
        \value ->
            case value of
                Tuple first second ->
                    Maybe.map2 Tuple.pair (firstSelector.fromValue first) (secondSelector.fromValue second)

                _ ->
                    Nothing
    , toValue =
        \( first, second ) ->
            Tuple (firstSelector.toValue first) (secondSelector.toValue second)
    , name = "( " ++ firstSelector.name ++ ", " ++ secondSelector.name ++ ")"
    }


constant : OutSelector res x -> res -> ( Int, List Value -> Eval Value )
constant selector const =
    ( 0
    , \args _ env ->
        case args of
            [] ->
                ( Ok <| selector.toValue const, [] )

            _ ->
                ( Err <| typeError env <| "Didn't expect any args", [] )
    )


zero :
    To
    -> OutSelector out ox
    -> out
    -> ( Int, List Value -> Eval Value )
zero _ output f =
    zeroWithError To output (Ok f)


zeroWithError :
    To
    -> OutSelector out ox
    -> EvalResult out
    -> ( Int, List Value -> Eval Value )
zeroWithError _ output f =
    ( 0
    , \args _ env ->
        case args of
            [] ->
                ( Result.map output.toValue f, [] )

            _ ->
                ( Err <| typeError env <| "Expected zero args, got more", [] )
    )


one :
    InSelector a ax
    -> To
    -> OutSelector out ox
    -> (a -> out)
    -> ( Int, List Value -> Eval Value )
one firstSelector _ output f =
    oneWithError firstSelector To output (\_ _ v -> ( Ok (f v), [] ))


oneWithError :
    InSelector a xa
    -> To
    -> OutSelector out xo
    -> (a -> Eval out)
    -> ( Int, List Value -> Eval Value )
oneWithError firstSelector _ output f =
    ( 1
    , \args cfg env ->
        let
            err : String -> EvalResult value
            err got =
                Err <| typeError env <| "Expected one " ++ firstSelector.name ++ ", got " ++ got
        in
        case args of
            [ arg ] ->
                case firstSelector.fromValue arg of
                    Just s ->
                        Tuple.mapFirst
                            (Result.map output.toValue)
                            (f s cfg env)

                    Nothing ->
                        ( err (Value.toString arg), [] )

            [] ->
                ( err "zero", [] )

            _ ->
                ( err "more", [] )
    )


two :
    InSelector a xa
    -> InSelector b xb
    -> To
    -> OutSelector out xo
    -> (a -> b -> out)
    -> ( Int, List Value -> Eval Value )
two firstSelector secondSelector _ output f =
    twoWithError firstSelector secondSelector To output (\_ _ l r -> ( Ok (f l r), [] ))


twoWithError :
    InSelector a xa
    -> InSelector b xb
    -> To
    -> OutSelector out xo
    -> (a -> b -> Eval out)
    -> ( Int, List Value -> Eval Value )
twoWithError firstSelector secondSelector _ output f =
    ( 2
    , \args cfg env ->
        let
            typeError_ : String -> EvalResult value
            typeError_ msg =
                Err (typeError env msg)

            err : String -> EvalResult value
            err got =
                if firstSelector.name == secondSelector.name then
                    typeError_ <| "Expected two " ++ firstSelector.name ++ "s, got " ++ got

                else
                    typeError_ <| "Expected one " ++ firstSelector.name ++ " and one " ++ secondSelector.name ++ ", got " ++ got
        in
        case args of
            [ firstArg, secondArg ] ->
                case firstSelector.fromValue firstArg of
                    Nothing ->
                        ( typeError_ <| "Expected the first argument to be " ++ firstSelector.name ++ ", got " ++ Value.toString firstArg, [] )

                    Just first ->
                        case secondSelector.fromValue secondArg of
                            Nothing ->
                                ( typeError_ <| "Expected the second argument to be " ++ secondSelector.name ++ ", got " ++ Value.toString secondArg, [] )

                            Just second ->
                                Tuple.mapFirst
                                    (Result.map output.toValue)
                                    (f cfg env first second)

            [] ->
                ( err "zero", [] )

            _ ->
                ( err (String.join ", " <| List.map Value.toString args), [] )
    )


three :
    InSelector a xa
    -> InSelector b xb
    -> InSelector c xc
    -> To
    -> OutSelector out xo
    -> (a -> b -> c -> out)
    -> ( Int, List Value -> Eval Value )
three firstSelector secondSelector thirdSelector _ output f =
    threeWithError firstSelector secondSelector thirdSelector To output (\_ _ l m r -> ( Ok (f l m r), [] ))


threeWithError :
    InSelector a xa
    -> InSelector b xb
    -> InSelector c xc
    -> To
    -> OutSelector out xo
    -> (a -> b -> c -> Eval out)
    -> ( Int, List Value -> Eval Value )
threeWithError firstSelector secondSelector thirdSelector _ output f =
    ( 3
    , \args cfg env ->
        let
            err : String -> EvalResult value
            err got =
                if firstSelector.name == secondSelector.name && secondSelector.name == thirdSelector.name then
                    Err <| typeError env <| "Expected three " ++ firstSelector.name ++ "s, got " ++ got

                else
                    Err <| typeError env <| "Expected one " ++ firstSelector.name ++ ", one " ++ secondSelector.name ++ " and one " ++ thirdSelector.name ++ ", got " ++ got
        in
        case args of
            [ firstArg, secondArg, thirdArg ] ->
                case ( firstSelector.fromValue firstArg, secondSelector.fromValue secondArg, thirdSelector.fromValue thirdArg ) of
                    ( Just first, Just second, Just third ) ->
                        Tuple.mapFirst
                            (Result.map output.toValue)
                            (f first second third cfg env)

                    _ ->
                        ( err (String.join ", " (List.map Value.toString args)), [] )

            [] ->
                ( err "zero", [] )

            _ ->
                ( err ("[ " ++ String.join ", " (List.map Value.toString args) ++ " ]"), [] )
    )


twoNumbers :
    (Int -> Int -> Int)
    -> (Float -> Float -> Float)
    -> ( Int, List Value -> Eval Value )
twoNumbers fInt fFloat =
    ( 2
    , \args _ env ->
        case args of
            [ Int li, Int ri ] ->
                ( Ok <| Int (fInt li ri), [] )

            [ Int li, Float rf ] ->
                ( Ok <| Float (fFloat (toFloat li) rf), [] )

            [ Float lf, Int ri ] ->
                ( Ok <| Float (fFloat lf (toFloat ri)), [] )

            [ Float lf, Float rf ] ->
                ( Ok <| Float (fFloat lf rf), [] )

            _ ->
                ( Err <| typeError env "Expected two numbers", [] )
    )
