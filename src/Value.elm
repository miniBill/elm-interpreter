module Value exposing (Env, EnvValues, EvalError, EvalErrorKind(..), EvalResult, Value(..), fromOrder, nameError, toArray, toOrder, toString, typeError, unsupported)

import Array exposing (Array)
import Elm.Syntax.Expression as Expression exposing (Expression, FunctionImplementation)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node exposing (Node)
import Elm.Syntax.Pattern exposing (Pattern, QualifiedNameRef)
import Elm.Writer
import FastDict as Dict exposing (Dict)
import Syntax exposing (fakeNode)


type Value
    = String String
    | Int Int
    | Float Float
    | Char Char
    | Bool Bool
    | Unit
    | Tuple Value Value
    | Triple Value Value Value
    | Record (Dict String Value)
    | Custom QualifiedNameRef (List Value)
    | PartiallyApplied Env (List Value) (List (Node Pattern)) (Maybe QualifiedNameRef) (Node Expression)
    | JsArray (Array Value)
    | List (List Value)


type alias Env =
    { currentModule : ModuleName
    , functions : Dict ModuleName (Dict String FunctionImplementation)
    , values : EnvValues
    , callStack : List QualifiedNameRef
    }


type alias EnvValues =
    Dict String Value


type alias EvalResult a =
    Result EvalError a


type alias EvalError =
    { currentModule : ModuleName
    , callStack : List QualifiedNameRef
    , error : EvalErrorKind
    }


type EvalErrorKind
    = TypeError String
    | Unsupported String
    | NameError String


typeError : Env -> String -> EvalError
typeError env msg =
    error env (TypeError msg)


nameError : Env -> String -> EvalError
nameError env msg =
    error env (NameError msg)


unsupported : Env -> String -> EvalError
unsupported env msg =
    error env (Unsupported msg)


error : Env -> EvalErrorKind -> EvalError
error env msg =
    { currentModule = env.currentModule
    , callStack = env.callStack
    , error = msg
    }


toExpression : Value -> Node Expression
toExpression value =
    fakeNode <|
        case value of
            String s ->
                Expression.Literal s

            Int i ->
                Expression.Integer i

            Float f ->
                Expression.Floatable f

            Char c ->
                Expression.CharLiteral c

            Bool b ->
                Expression.FunctionOrValue [] (boolToString b)

            Unit ->
                Expression.UnitExpr

            Tuple l r ->
                Expression.TupledExpression
                    [ toExpression l
                    , toExpression r
                    ]

            Triple l m r ->
                Expression.TupledExpression
                    [ toExpression l
                    , toExpression m
                    , toExpression r
                    ]

            Record fields ->
                fields
                    |> Dict.toList
                    |> List.map
                        (\( fieldName, fieldValue ) ->
                            fakeNode ( fakeNode fieldName, toExpression fieldValue )
                        )
                    |> Expression.RecordExpr

            List list ->
                list
                    |> List.map toExpression
                    |> Expression.ListExpr

            Custom name args ->
                case toArray value of
                    Just array ->
                        arrayToExpression "Array" array

                    Nothing ->
                        (fakeNode (Expression.FunctionOrValue name.moduleName name.name)
                            :: List.map toExpression args
                        )
                            |> Expression.Application

            JsArray array ->
                arrayToExpression "JsArray" (Array.toList array)

            PartiallyApplied _ [] patterns (Just qualifiedName) implementation ->
                Expression.FunctionOrValue qualifiedName.moduleName qualifiedName.name

            PartiallyApplied _ args patterns (Just qualifiedName) implementation ->
                (fakeNode
                    (Expression.FunctionOrValue
                        qualifiedName.moduleName
                        qualifiedName.name
                    )
                    :: List.map toExpression args
                )
                    |> Expression.Application

            PartiallyApplied _ [] patterns Nothing implementation ->
                Expression.LambdaExpression
                    { args = patterns
                    , expression = implementation
                    }

            PartiallyApplied _ args patterns Nothing implementation ->
                (fakeNode
                    (Expression.LambdaExpression
                        { args = patterns
                        , expression = implementation
                        }
                    )
                    :: List.map toExpression args
                )
                    |> Expression.Application


arrayToExpression : String -> List Value -> Expression
arrayToExpression name array =
    Expression.Application
        [ Expression.FunctionOrValue
            [ name ]
            "fromList"
            |> fakeNode
        , array
            |> List
            |> toExpression
        ]


toArray : Value -> Maybe (List Value)
toArray value =
    case value of
        Custom name args ->
            case ( name.moduleName, name.name, args ) of
                ( [ "Array" ], "Array_elm_builtin", [ _, _, tree, tail ] ) ->
                    let
                        treeToArray : Value -> List Value -> Maybe (List Value)
                        treeToArray node acc =
                            case node of
                                JsArray arr ->
                                    Just (Array.toList arr ++ acc)

                                _ ->
                                    Debug.todo ("treeToArray " ++ Debug.toString node)
                    in
                    case tail of
                        JsArray tailArray ->
                            treeToArray tree (Array.toList tailArray)

                        _ ->
                            Nothing

                _ ->
                    Nothing

        _ ->
            Nothing


boolToString : Bool -> String
boolToString b =
    if b then
        "True"

    else
        "False"


toString : Value -> String
toString value =
    -- TODO: This is inefficient and subtly different from Debug.toString
    toExpression value
        |> Elm.Writer.writeExpression
        |> Elm.Writer.write


fromOrder : Order -> Value
fromOrder order =
    case order of
        LT ->
            Custom { moduleName = [ "Basics" ], name = "LT" } []

        EQ ->
            Custom { moduleName = [ "Basics" ], name = "EQ" } []

        GT ->
            Custom { moduleName = [ "Basics" ], name = "GT" } []


toOrder : Value -> Maybe Order
toOrder value =
    case value of
        Custom { moduleName, name } [] ->
            case ( moduleName, name ) of
                ( [ "Basics" ], "LT" ) ->
                    Just LT

                ( [ "Basics" ], "EQ" ) ->
                    Just EQ

                ( [ "Basics" ], "GT" ) ->
                    Just GT

                _ ->
                    Nothing

        _ ->
            Nothing
