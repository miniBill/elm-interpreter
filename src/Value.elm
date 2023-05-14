module Value exposing (EvalError(..), Value(..), toString)

import Elm.Syntax.Expression as Expression
import Elm.Syntax.Pattern exposing (QualifiedNameRef)
import Elm.Writer
import FastDict exposing (Dict)
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
    | Lambda (Value -> Result EvalError Value)


type EvalError
    = TypeError String
    | Unsupported String
    | NameError String


toExpression : Value -> Maybe Expression.Expression
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

        Record _ ->
            Debug.todo "branch 'Record _' not implemented"

        Custom name args ->
            (Just (Expression.FunctionOrValue name.moduleName name.name) :: List.map toExpression args)
                |> Maybe.Extra.combine
                |> Maybe.map
                    (List.map fakeNode
                        >> Expression.Application
                    )

        Lambda _ ->
            Nothing


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
