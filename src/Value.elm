module Value exposing (EvalError(..), Value(..), toString)

import Elm.Syntax.Expression as Expression
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (QualifiedNameRef)
import Elm.Syntax.Range exposing (Location, Range)
import Elm.Writer
import FastDict exposing (Dict)


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


fakeNode : a -> Node a
fakeNode value =
    Node fakeRange value


fakeRange : Range
fakeRange =
    { start = fakeLocation, end = fakeLocation }


fakeLocation : Location
fakeLocation =
    { row = -1
    , column = -1
    }


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

        Custom _ _ ->
            Debug.todo "branch 'Custom _ _' not implemented"

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
