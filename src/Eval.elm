module Eval exposing (Error, eval, evalModule)

import Elm.Parser
import Elm.Processing
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression as Expression exposing (Expression, FunctionImplementation)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Infix as Infix
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..), QualifiedNameRef)
import Elm.Syntax.Type exposing (Type)
import Env exposing (Env)
import FastDict as Dict
import Parser exposing (DeadEnd)
import Result.Extra
import Unicode
import Value exposing (EvalError(..), Value)


{-| Variant names for list
-}
list :
    { cons : QualifiedNameRef
    , nil : QualifiedNameRef
    }
list =
    { cons = { moduleName = [ "List" ], name = "Cons" }
    , nil = { moduleName = [ "List" ], name = "Nil" }
    }


type Error
    = ParsingError (List DeadEnd)
    | EvalError EvalError


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
                                Ok (Env.addFunction implementation env)

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
            (Ok Env.empty)


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

        Expression.FunctionOrValue moduleName name ->
            if isVariant name then
                let
                    qualifiedNameRef : QualifiedNameRef
                    qualifiedNameRef =
                        { moduleName = moduleName, name = name }
                in
                Ok (Value.Custom qualifiedNameRef [])

            else
                let
                    fullName : String
                    fullName =
                        (moduleName ++ [ name ])
                            |> String.join "."
                in
                case Dict.get fullName env.values of
                    Just value ->
                        Ok value

                    Nothing ->
                        case Dict.get fullName env.functions of
                            Just function ->
                                functionToValue env function

                            Nothing ->
                                Err <| NameError <| fullName ++ " not found"

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

        Expression.TupledExpression exprs ->
            evalTupleExpression env exprs

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
                                Env.addFunction
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

        Expression.CaseExpression caseExpr ->
            evalCase env caseExpr

        Expression.LambdaExpression _ ->
            Err <| Unsupported "branch 'LambdaExpression _' not implemented"

        Expression.RecordExpr fields ->
            Err <| Unsupported "branch 'RecordExpr _' not implemented"

        Expression.ListExpr elements ->
            elements
                |> Result.Extra.combineMap
                    (\element -> evalExpression env (Node.value element))
                |> Result.map
                    (\values ->
                        List.foldr
                            (\value acc ->
                                Value.Custom list.cons [ value, acc ]
                            )
                            (Value.Custom list.nil [])
                            values
                    )

        Expression.RecordAccess (Node _ recordExpr) (Node _ field) ->
            evalExpression env recordExpr
                |> Result.andThen
                    (\value ->
                        case value of
                            Value.Record fields ->
                                case Dict.get field fields of
                                    Just fieldValue ->
                                        Ok fieldValue

                                    Nothing ->
                                        Err <| TypeError <| "Field " ++ field ++ " not found"

                            _ ->
                                Err <| TypeError "Trying to access a field on a non-record value"
                    )

        Expression.RecordAccessFunction _ ->
            Err <| Unsupported "branch 'RecordAccessFunction _' not implemented"

        Expression.RecordUpdateExpression _ _ ->
            Err <| Unsupported "branch 'RecordUpdateExpression _ _' not implemented"

        Expression.GLSLExpression _ ->
            Err <| Unsupported "GLSL not supported"


isVariant : String -> Bool
isVariant name =
    case String.uncons name of
        Nothing ->
            False

        Just ( first, _ ) ->
            Unicode.isUpper first


evalTupleExpression : Env -> List (Node Expression) -> Result EvalError Value
evalTupleExpression env args =
    case args of
        [] ->
            Ok Value.Unit

        [ c ] ->
            evalExpression env (Node.value c)

        [ l, r ] ->
            Result.map2 Value.Tuple
                (evalExpression env (Node.value l))
                (evalExpression env (Node.value r))

        [ l, m, r ] ->
            Result.map3 Value.Triple
                (evalExpression env (Node.value l))
                (evalExpression env (Node.value m))
                (evalExpression env (Node.value r))

        _ :: _ :: _ :: _ :: _ ->
            Err <| TypeError "Tuples with more than three elements are not supported"


evalCase : Env -> Expression.CaseBlock -> Result EvalError Value
evalCase env { expression, cases } =
    case evalExpression env (Node.value expression) of
        Err e ->
            Err e

        Ok exprValue ->
            List.foldl
                (\( Node _ pattern, Node _ result ) acc ->
                    case acc of
                        Just _ ->
                            acc

                        Nothing ->
                            match pattern exprValue
                                |> Maybe.map
                                    (\additionalEnv ->
                                        evalExpression (Env.with additionalEnv env) result
                                    )
                )
                Nothing
                cases
                |> Maybe.withDefault (Err <| TypeError "Missing case branch")


match : Pattern -> Value -> Maybe Env
match pattern value =
    case ( pattern, value ) of
        ( UnitPattern, Value.Unit ) ->
            Just Env.empty

        ( UnitPattern, _ ) ->
            Nothing

        ( AllPattern, _ ) ->
            Just Env.empty

        ( ParenthesizedPattern subPattern, _ ) ->
            match (Node.value subPattern) value

        ( CharPattern _, _ ) ->
            Debug.todo "branch '( CharPattern _, _ )' not implemented"

        ( StringPattern _, _ ) ->
            Debug.todo "branch '( StringPattern _, _ )' not implemented"

        ( IntPattern _, _ ) ->
            Debug.todo "branch '( IntPattern _, _ )' not implemented"

        ( HexPattern _, _ ) ->
            Debug.todo "branch '( HexPattern _, _ )' not implemented"

        ( FloatPattern _, _ ) ->
            Debug.todo "branch '( FloatPattern _, _ )' not implemented"

        ( TuplePattern _, _ ) ->
            Debug.todo "branch '( TuplePattern _, _ )' not implemented"

        ( RecordPattern _, _ ) ->
            Debug.todo "branch '( RecordPattern _, _ )' not implemented"

        ( UnConsPattern _ _, _ ) ->
            Debug.todo "branch '( UnConsPattern _ _, _ )' not implemented"

        ( ListPattern _, _ ) ->
            Debug.todo "branch '( ListPattern _, _ )' not implemented"

        ( VarPattern _, _ ) ->
            Debug.todo "branch '( VarPattern _, _ )' not implemented"

        ( NamedPattern _ _, _ ) ->
            Debug.todo "branch '( NamedPattern _ _, _ )' not implemented"

        ( AsPattern _ _, _ ) ->
            Debug.todo "branch '( AsPattern _ _, _ )' not implemented"


evalApply : Env -> Value -> Expression -> Result EvalError Value
evalApply env lValue r =
    case lValue of
        Value.Lambda f ->
            evalExpression env r
                |> Result.andThen
                    (\rValue ->
                        f rValue
                    )

        Value.Custom name args ->
            evalExpression env r
                |> Result.map
                    (\rValue -> Value.Custom name (args ++ [ rValue ]))

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
                            \varValue -> go (Env.addValue varName varValue newEnv) tail
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
                    operatorTypeError opName lvalue rvalue


operatorTypeError : String -> Value -> Value -> Result EvalError v
operatorTypeError opName lvalue rvalue =
    let
        message : String
        message =
            "Cannot apply operator " ++ opName ++ " to values " ++ Value.toString lvalue ++ " and " ++ Value.toString rvalue
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
                    operatorTypeError opName lvalue rvalue


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
