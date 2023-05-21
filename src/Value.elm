module Value exposing (Env, EnvValues, EvalError(..), Value(..), toString)

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


toExpression : Value -> Maybe (Node Expression)
toExpression value =
    let
        ok : a -> Maybe (Node a)
        ok e =
            Just (fakeNode e)
    in
    case value of
        String s ->
            ok (Expression.Literal s)

        Int i ->
            ok (Expression.Integer i)

        Float f ->
            ok (Expression.Floatable f)

        Char c ->
            ok (Expression.CharLiteral c)

        Bool b ->
            ok (Expression.FunctionOrValue [] (boolToString b))

        Unit ->
            ok Expression.UnitExpr

        Tuple l r ->
            Maybe.map2
                (\le re ->
                    fakeNode <|
                        Expression.TupledExpression
                            [ le
                            , re
                            ]
                )
                (toExpression l)
                (toExpression r)

        Triple l m r ->
            Maybe.map3
                (\le me re ->
                    fakeNode <|
                        Expression.TupledExpression
                            [ le
                            , me
                            , re
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
                            (\expr -> fakeNode ( fakeNode fieldName, expr ))
                            (toExpression fieldValue)
                    )
                |> Maybe.map (Expression.RecordExpr >> fakeNode)

        List list ->
            list
                |> Maybe.Extra.traverse toExpression
                |> Maybe.map (Expression.ListExpr >> fakeNode)

        Array array ->
            array
                |> Array.toList
                |> List
                |> toExpression
                |> Maybe.map
                    (\list ->
                        fakeNode <|
                            Expression.Application
                                [ Expression.FunctionOrValue
                                    [ "Array" ]
                                    "fromList"
                                    |> fakeNode
                                , list
                                ]
                    )

        Custom name args ->
            (ok (Expression.FunctionOrValue name.moduleName name.name)
                :: List.map toExpression args
            )
                |> Maybe.Extra.combine
                |> Maybe.map (Expression.Application >> fakeNode)

        PartiallyApplied _ [] patterns implementation ->
            ok
                (Expression.LambdaExpression
                    { args = patterns
                    , expression = implementation
                    }
                )

        PartiallyApplied localEnv args patterns implementation ->
            Maybe.map2
                (\lambda argExprs ->
                    fakeNode <| Expression.Application (lambda :: argExprs)
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
                |> Elm.Writer.writeExpression
                |> Elm.Writer.write

        Nothing ->
            "Could not convert to string :("
