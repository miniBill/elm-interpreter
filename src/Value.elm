module Value exposing (EvalError(..), Value(..), toString)

import Elm.Syntax.Expression as Expression
import Elm.Syntax.Node exposing (Node)
import Elm.Syntax.Pattern exposing (QualifiedNameRef)
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
    Debug.todo "fakeNode"


toExpression : Value -> Maybe Expression.Expression
toExpression value =
    case value of
        String s ->
            Just (Expression.Literal s)

        Int _ ->
            Debug.todo "branch 'Int _' not implemented"

        Float _ ->
            Debug.todo "branch 'Float _' not implemented"

        Char _ ->
            Debug.todo "branch 'Char _' not implemented"

        Bool _ ->
            Debug.todo "branch 'Bool _' not implemented"

        Unit ->
            Debug.todo "branch 'Unit' not implemented"

        Tuple _ _ ->
            Debug.todo "branch 'Tuple _ _' not implemented"

        Triple _ _ _ ->
            Debug.todo "branch 'Triple _ _ _' not implemented"

        Record _ ->
            Debug.todo "branch 'Record _' not implemented"

        Custom _ _ ->
            Debug.todo "branch 'Custom _ _' not implemented"

        Lambda _ ->
            Debug.todo "branch 'Lambda _' not implemented"


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
