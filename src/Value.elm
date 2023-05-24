module Value exposing (Env, EnvValues, EvalError(..), Value(..), fromOrder, toOrder, toString)

import Array exposing (Array)
import Elm.Syntax.Expression as Expression exposing (Expression, FunctionImplementation)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node exposing (Node)
import Elm.Syntax.Pattern exposing (Pattern, QualifiedNameRef)
import Elm.Writer
import FastDict as Dict exposing (Dict)
import Maybe.Extra
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
    | PartiallyApplied (() -> Env) (List Value) (List (Node Pattern)) (Node Expression)
    | Array (Array Value)
    | List (List Value)


type alias Env =
    { currentModule : ModuleName
    , functions : Dict ModuleName (Dict String FunctionImplementation)
    , values : EnvValues
    }


type alias EnvValues =
    Dict String Value


type EvalError
    = TypeError String
    | Unsupported String
    | NameError String


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

            Array array ->
                array
                    |> Array.toList
                    |> List
                    |> toExpression
                    |> (\list ->
                            Expression.Application
                                [ Expression.FunctionOrValue
                                    [ "Array" ]
                                    "fromList"
                                    |> fakeNode
                                , list
                                ]
                       )

            Custom name args ->
                (fakeNode (Expression.FunctionOrValue name.moduleName name.name)
                    :: List.map toExpression args
                )
                    |> Expression.Application

            PartiallyApplied _ [] patterns implementation ->
                Expression.LambdaExpression
                    { args = patterns
                    , expression = implementation
                    }

            PartiallyApplied _ args patterns implementation ->
                (fakeNode
                    (Expression.LambdaExpression
                        { args = patterns
                        , expression = implementation
                        }
                    )
                    :: List.map toExpression args
                )
                    |> Expression.Application


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
