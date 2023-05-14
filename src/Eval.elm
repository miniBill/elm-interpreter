module Eval exposing (Error, eval, evalModule)

import Dict exposing (Dict)
import Elm.Parser
import Elm.Processing
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression as Expression exposing (Expression, FunctionImplementation)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Infix as Infix
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))
import Parser exposing (DeadEnd)
import Value exposing (EvalError(..), Value)


type Error
    = ParsingError (List DeadEnd)
    | EvalError EvalError


type alias Env =
    { functions : Dict String FunctionImplementation
    , values : Dict String Value
    }


addValueToEnv : String -> Value -> Env -> Env
addValueToEnv name value env =
    { functions = env.functions
    , values = Dict.insert name value env.values
    }


addFunctionToEnv : FunctionImplementation -> Env -> Env
addFunctionToEnv function env =
    { functions = Dict.insert (Node.value function.name) function env.functions
    , values = env.values
    }


eval : String -> Result Error Value
eval expressionSource =
    let
        source : String
        source =
            toModule expressionSource

        expression : Expression
        expression =
            Expression.FunctionOrValue [] "main"
    in
    evalModule source expression


evalModule : String -> Expression -> Result Error Value
evalModule source expression =
    source
        |> Elm.Parser.parse
        |> Result.mapError ParsingError
        |> Result.map
            (\rawFile ->
                let
                    context : Elm.Processing.ProcessContext
                    context =
                        Elm.Processing.init
                in
                Elm.Processing.process context rawFile
            )
        |> Result.andThen buildEnv
        |> Result.andThen
            (\env ->
                Result.mapError EvalError <|
                    evalExpression env expression
            )


buildEnv : File -> Result Error Env
buildEnv file =
    file.declarations
        |> List.foldl
            (\(Node _ decl) ->
                Result.andThen
                    (\env ->
                        case decl of
                            FunctionDeclaration function ->
                                let
                                    (Node _ implementation) =
                                        function.declaration
                                in
                                Ok (addFunctionToEnv implementation env)

                            PortDeclaration _ ->
                                Err (EvalError <| Unsupported "PortDeclaration")

                            InfixDeclaration _ ->
                                Err (EvalError <| Unsupported "InfixDeclaration")

                            Destructuring _ _ ->
                                Err (EvalError <| Unsupported "Destructuring")

                            AliasDeclaration _ ->
                                Ok env

                            CustomTypeDeclaration _ ->
                                Ok env
                    )
            )
            (Ok
                { functions = Dict.empty
                , values = Dict.empty
                }
            )


evalExpression :
    Env
    -> Expression
    -> Result EvalError Value
evalExpression env expression =
    case expression of
        Expression.UnitExpr ->
            Ok Value.Unit

        Expression.OperatorApplication "+" Infix.Left l r ->
            evalNumberOperator env "+" (+) (+) l r

        Expression.OperatorApplication "-" Infix.Left l r ->
            evalNumberOperator env "-" (-) (-) l r

        Expression.OperatorApplication "*" Infix.Left l r ->
            evalNumberOperator env "*" (*) (*) l r

        Expression.OperatorApplication "<=" Infix.Left l r ->
            evalRelationOperator env "<=" (<=) (<=) l r

        Expression.OperatorApplication ">=" Infix.Left l r ->
            evalRelationOperator env ">=" (>=) (>=) l r

        Expression.OperatorApplication opName _ _ _ ->
            Err <| Unsupported <| "branch 'OperatorApplication \"" ++ opName ++ "\" _ _ _' not implemented"

        Expression.Application [] ->
            Err <| TypeError "Empty Application"

        Expression.Application ((Node _ first) :: rest) ->
            List.foldl
                (\(Node _ arg) ->
                    Result.andThen
                        (\acc ->
                            evalApply env acc arg
                        )
                )
                (evalExpression env first)
                rest

        Expression.FunctionOrValue [] name ->
            case Dict.get name env.values of
                Just value ->
                    Ok value

                Nothing ->
                    case Dict.get name env.functions of
                        Just function ->
                            functionToValue env function

                        Nothing ->
                            Err <| NameError <| name ++ " not found"

        Expression.FunctionOrValue _ _ ->
            Err <| Unsupported "branch 'FunctionOrValue _ _' not implemented"

        Expression.IfBlock (Node _ cond) (Node _ true) (Node _ false) ->
            evalExpression env cond
                |> Result.andThen
                    (\condValue ->
                        case condValue of
                            Value.Bool True ->
                                evalExpression env true

                            Value.Bool False ->
                                evalExpression env false

                            _ ->
                                Err <| TypeError "ifThenElse condition was not a boolean"
                    )

        Expression.PrefixOperator _ ->
            Err <| Unsupported "branch 'PrefixOperator _' not implemented"

        Expression.Operator _ ->
            Err <| Unsupported "branch 'Operator _' not implemented"

        Expression.Integer i ->
            Ok (Value.Int i)

        Expression.Hex i ->
            Ok (Value.Int i)

        Expression.Floatable f ->
            Ok (Value.Float f)

        Expression.Negation _ ->
            Err <| Unsupported "branch 'Negation _' not implemented"

        Expression.Literal string ->
            Ok (Value.String string)

        Expression.CharLiteral c ->
            Ok (Value.Char c)

        Expression.TupledExpression [] ->
            Ok Value.Unit

        Expression.TupledExpression [ c ] ->
            evalExpression env (Node.value c)

        Expression.TupledExpression [ l, r ] ->
            Result.map2 Value.Tuple
                (evalExpression env (Node.value l))
                (evalExpression env (Node.value r))

        Expression.TupledExpression [ l, m, r ] ->
            Result.map3 Value.Triple
                (evalExpression env (Node.value l))
                (evalExpression env (Node.value m))
                (evalExpression env (Node.value r))

        Expression.TupledExpression (_ :: _ :: _ :: _ :: _) ->
            Err <| TypeError "Tuples with more than three elements are not supported"

        Expression.ParenthesizedExpression (Node _ child) ->
            evalExpression env child

        Expression.LetExpression letBlock ->
            let
                newEnv : Result EvalError Env
                newEnv =
                    List.foldl
                        (\(Node _ letDeclaration) ->
                            Result.andThen
                                (addDeclaration letDeclaration)
                        )
                        (Ok env)
                        letBlock.declarations

                addDeclaration :
                    Expression.LetDeclaration
                    -> Env
                    -> Result EvalError Env
                addDeclaration letDeclaration acc =
                    case letDeclaration of
                        Expression.LetFunction function ->
                            Ok <|
                                addFunctionToEnv
                                    (Node.value function.declaration)
                                    acc

                        Expression.LetDestructuring _ _ ->
                            Err <| Unsupported "LetDestructuring"
            in
            case newEnv of
                Err e ->
                    Err e

                Ok ne ->
                    evalExpression ne (Node.value letBlock.expression)

        Expression.CaseExpression _ ->
            Err <| Unsupported "branch 'CaseExpression _' not implemented"

        Expression.LambdaExpression _ ->
            Err <| Unsupported "branch 'LambdaExpression _' not implemented"

        Expression.RecordExpr _ ->
            Err <| Unsupported "branch 'RecordExpr _' not implemented"

        Expression.ListExpr _ ->
            Err <| Unsupported "branch 'ListExpr _' not implemented"

        Expression.RecordAccess _ _ ->
            Err <| Unsupported "branch 'RecordAccess _ _' not implemented"

        Expression.RecordAccessFunction _ ->
            Err <| Unsupported "branch 'RecordAccessFunction _' not implemented"

        Expression.RecordUpdateExpression _ _ ->
            Err <| Unsupported "branch 'RecordUpdateExpression _ _' not implemented"

        Expression.GLSLExpression _ ->
            Err <| Unsupported "GLSL not supported"


evalApply : Env -> Value -> Expression -> Result EvalError Value
evalApply env lValue r =
    case lValue of
        Value.Lambda f ->
            evalExpression env r
                |> Result.andThen
                    (\rValue ->
                        f rValue
                    )

        _ ->
            Err <| TypeError "Trying to apply a non-lambda"


functionToValue : Env -> FunctionImplementation -> Result EvalError Value
functionToValue env function =
    let
        go : Env -> List Pattern -> Result EvalError Value
        go newEnv args =
            case args of
                [] ->
                    evalExpression newEnv (Node.value function.expression)

                (VarPattern varName) :: tail ->
                    Ok
                        (Value.Lambda <|
                            \varValue -> go (addValueToEnv varName varValue newEnv) tail
                        )

                _ :: _ ->
                    Err <| Unsupported "functionToValue - branch '_ :: _' not implemented"
    in
    go env (List.map Node.value function.arguments)


evalNumberOperator :
    Env
    -> String
    -> (Int -> Int -> Int)
    -> (Float -> Float -> Float)
    -> Node Expression
    -> Node Expression
    -> Result EvalError Value
evalNumberOperator env opName opInt opFloat l r =
    evalExpression2 env l r <|
        \lvalue rvalue ->
            case ( lvalue, rvalue ) of
                ( Value.Int li, Value.Int ri ) ->
                    Ok (Value.Int (opInt li ri))

                ( Value.Float lf, Value.Float rf ) ->
                    Ok (Value.Float (opFloat lf rf))

                _ ->
                    let
                        message : String
                        message =
                            "Trying to use operator " ++ opName ++ " with wrong arguments"
                    in
                    Err (TypeError message)


evalRelationOperator :
    Env
    -> String
    -> (Int -> Int -> Bool)
    -> (Float -> Float -> Bool)
    -> Node Expression
    -> Node Expression
    -> Result EvalError Value
evalRelationOperator env opName opInt opFloat l r =
    evalExpression2 env l r <|
        \lvalue rvalue ->
            case ( lvalue, rvalue ) of
                ( Value.Int li, Value.Int ri ) ->
                    Ok (Value.Bool (opInt li ri))

                ( Value.Float lf, Value.Float rf ) ->
                    Ok (Value.Bool (opFloat lf rf))

                _ ->
                    let
                        message : String
                        message =
                            "Trying to use operator " ++ opName ++ " with wrong arguments"
                    in
                    Err (TypeError message)


evalExpression2 :
    Env
    -> Node Expression
    -> Node Expression
    -> (Value -> Value -> Result EvalError Value)
    -> Result EvalError Value
evalExpression2 env l r f =
    case evalExpression env (Node.value l) of
        Err e ->
            Err e

        Ok lValue ->
            case evalExpression env (Node.value r) of
                Err e ->
                    Err e

                Ok rValue ->
                    f lValue rValue


toModule : String -> String
toModule expression =
    "module Main exposing (main)\n\nmain =\n"
        ++ indent 4 expression


indent : Int -> String -> String
indent count input =
    let
        prefix : String
        prefix =
            String.repeat count " "
    in
    input
        |> String.split "\n"
        |> List.map
            (\line ->
                if String.isEmpty line then
                    line

                else
                    prefix ++ line
            )
        |> String.join "\n"
