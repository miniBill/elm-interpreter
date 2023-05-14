module Value exposing (Env, EvalError(..), Value(..), toString)

import Elm.Syntax.Expression as Expression exposing (Expression, FunctionImplementation)
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
    | PartiallyApplied Env (List Value) (List (Node Pattern)) Expression


type alias Env =
    { functions : Dict String FunctionImplementation
    , values : Dict String Value
    }


type EvalError
    = TypeError String
    | Unsupported String
    | NameError String


toExpression : Value -> Maybe Expression
toExpression value =
    case value of
        String s ->
            Just (Expression.Literal s)

        Int i ->
            Just (Expression.Integer i)

        Float f ->
            Just (Expression.Floatable f)

        Char c ->
            Just (Expression.CharLiteral c)

        Bool b ->
            Just (Expression.FunctionOrValue [] (boolToString b))

        Unit ->
            Just Expression.UnitExpr

        Tuple l r ->
            Maybe.map2
                (\le re ->
                    Expression.TupledExpression
                        [ fakeNode le
                        , fakeNode re
                        ]
                )
                (toExpression l)
                (toExpression r)

        Triple l m r ->
            Maybe.map3
                (\le me re ->
                    Expression.TupledExpression
                        [ fakeNode le
                        , fakeNode me
                        , fakeNode re
                        ]
                )
                (toExpression l)
                (toExpression m)
                (toExpression r)

        Record fields ->
            fields
                |> Dict.toList
                |> Maybe.Extra.traverse
                    (\( fieldName, fieldValue ) ->
                        Maybe.map
                            (\expr -> fakeNode ( fakeNode fieldName, fakeNode expr ))
                            (toExpression fieldValue)
                    )
                |> Maybe.map Expression.RecordExpr

        Custom name args ->
            (Just (Expression.FunctionOrValue name.moduleName name.name) :: List.map toExpression args)
                |> Maybe.Extra.combine
                |> Maybe.map
                    (List.map fakeNode
                        >> Expression.Application
                    )

        PartiallyApplied _ [] patterns implementation ->
            Just
                (Expression.LambdaExpression
                    { args = patterns
                    , expression = fakeNode implementation
                    }
                )

        PartiallyApplied localEnv args patterns implementation ->
            Maybe.map2
                (\lambda argExprs ->
                    Expression.Application <|
                        List.map fakeNode (lambda :: argExprs)
                )
                (toExpression (PartiallyApplied localEnv [] patterns implementation))
                (Maybe.Extra.traverse toExpression args)


boolToString : Bool -> String
boolToString b =
    if b then
        "True"

    else
        "False"


toString : Value -> String
toString value =
    case toExpression value of
        Just e ->
            e
                |> fakeNode
                |> Elm.Writer.writeExpression
                |> Elm.Writer.write

        Nothing ->
            "Could not convert to string :("
