module Eval exposing (Error(..), eval, evalModule)

import Core.Basics
import Elm.Parser
import Elm.Processing
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression as Expression exposing (Expression, FunctionImplementation)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Infix as Infix
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..), QualifiedNameRef)
import Env exposing (Env)
import FastDict as Dict
import Parser exposing (DeadEnd)
import Result.Extra
import Result.MyExtra
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
        |> Result.MyExtra.combineFoldl
            (\(Node _ decl) env ->
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
            (Ok Core.Basics.env)


evalExpression : Env -> Expression -> Result EvalError Value
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

        Expression.Application ((Node _ first) :: rest) ->
            case evalExpression env first of
                Err e ->
                    Err e

                Ok val ->
                    evalApply env val rest

        Expression.Application _ ->
            Err <| TypeError "Application with less than two args"

        Expression.FunctionOrValue moduleName name ->
            if isVariant name then
                case ( moduleName, name ) of
                    ( [], "True" ) ->
                        Ok (Value.Bool True)

                    ( [], "False" ) ->
                        Ok (Value.Bool False)

                    _ ->
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
                    Result.MyExtra.combineFoldl
                        (\(Node _ letDeclaration) ->
                            addDeclaration letDeclaration
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
            fields
                |> Result.Extra.combineMap
                    (\(Node _ ( Node _ fieldName, Node _ fieldExpr )) ->
                        Result.map
                            (Tuple.pair fieldName)
                            (evalExpression env fieldExpr)
                    )
                |> Result.map
                    (\tuples ->
                        tuples
                            |> Dict.fromList
                            |> Value.Record
                    )

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
            cases
                |> Result.MyExtra.combineFoldl
                    (\( Node _ pattern, Node _ result ) acc ->
                        case acc of
                            Just _ ->
                                Ok acc

                            Nothing ->
                                case match pattern exprValue of
                                    Err e ->
                                        Err e

                                    Ok Nothing ->
                                        Ok Nothing

                                    Ok (Just additionalEnv) ->
                                        evalExpression (Env.with additionalEnv env) result
                                            |> Result.map Just
                    )
                    (Ok Nothing)
                |> Result.andThen
                    (\result ->
                        case result of
                            Nothing ->
                                Err <| TypeError "Missing case branch"

                            Just res ->
                                Ok res
                    )


match : Pattern -> Value -> Result EvalError (Maybe Env)
match pattern value =
    let
        ok : a -> Result error (Maybe a)
        ok val =
            Ok (Just val)

        noMatch : Result error (Maybe a)
        noMatch =
            Ok Nothing

        typeError : String -> Result EvalError value
        typeError message =
            Err <| TypeError message

        andThen : (a -> Result error (Maybe a)) -> Result error (Maybe a) -> Result error (Maybe a)
        andThen f v =
            case v of
                Err _ ->
                    v

                Ok Nothing ->
                    v

                Ok (Just w) ->
                    f w
    in
    case ( pattern, value ) of
        ( UnitPattern, Value.Unit ) ->
            ok Env.empty

        ( UnitPattern, _ ) ->
            noMatch

        ( AllPattern, _ ) ->
            ok Env.empty

        ( ParenthesizedPattern subPattern, _ ) ->
            match (Node.value subPattern) value

        ( NamedPattern namePattern argsPatterns, Value.Custom variant args ) ->
            -- Two names from different modules can never have the same type
            -- so if we assume the code typechecks we can skip the module name check
            if namePattern.name == variant.name then
                let
                    go :
                        Env
                        -> ( List (Node Pattern), List Value )
                        -> Result EvalError (Maybe Env)
                    go env queue =
                        case queue of
                            ( [], [] ) ->
                                ok env

                            ( (Node _ patternHead) :: patternTail, argHead :: argTail ) ->
                                match patternHead argHead
                                    |> andThen
                                        (\newEnv ->
                                            go (Env.with newEnv env) ( patternTail, argTail )
                                        )

                            _ ->
                                typeError "Mismatched number of arguments to variant"
                in
                go Env.empty ( argsPatterns, args )

            else
                noMatch

        ( NamedPattern _ _, _ ) ->
            noMatch

        ( ListPattern [], Value.Custom _ [] ) ->
            -- We assume the code typechecks!
            ok Env.empty

        ( ListPattern ((Node _ patternHead) :: patternTail), Value.Custom _ [ listHead, listTail ] ) ->
            match patternHead listHead
                |> andThen
                    (\headEnv ->
                        match (ListPattern patternTail) listTail
                            |> andThen
                                (\tailEnv ->
                                    ok
                                        (Env.with tailEnv headEnv)
                                )
                    )

        ( UnConsPattern (Node _ patternHead) (Node _ patternTail), Value.Custom _ [ listHead, listTail ] ) ->
            match patternHead listHead
                |> andThen
                    (\headEnv ->
                        match patternTail listTail
                            |> andThen
                                (\tailEnv ->
                                    ok
                                        (Env.with tailEnv headEnv)
                                )
                    )

        ( UnConsPattern _ _, _ ) ->
            noMatch

        ( VarPattern name, _ ) ->
            ok <| Env.addValue name value Env.empty

        ( ListPattern _, _ ) ->
            noMatch

        ( CharPattern c, Value.Char d ) ->
            if c == d then
                ok Env.empty

            else
                noMatch

        ( CharPattern _, _ ) ->
            noMatch

        ( StringPattern c, Value.String d ) ->
            if c == d then
                ok Env.empty

            else
                noMatch

        ( StringPattern _, _ ) ->
            noMatch

        ( IntPattern c, Value.Int d ) ->
            if c == d then
                ok Env.empty

            else
                noMatch

        ( IntPattern _, _ ) ->
            noMatch

        ( HexPattern c, Value.Int d ) ->
            if c == d then
                ok Env.empty

            else
                noMatch

        ( HexPattern _, _ ) ->
            noMatch

        ( FloatPattern c, Value.Float d ) ->
            if c == d then
                ok Env.empty

            else
                noMatch

        ( FloatPattern _, _ ) ->
            noMatch

        ( TuplePattern _, _ ) ->
            Debug.todo "branch '( TuplePattern _, _ )' not implemented"

        ( RecordPattern _, _ ) ->
            Debug.todo "branch '( RecordPattern _, _ )' not implemented"

        ( AsPattern _ _, _ ) ->
            Debug.todo "branch '( AsPattern _ _, _ )' not implemented"


evalApply : Env -> Value -> List (Node Expression) -> Result EvalError Value
evalApply env val args =
    case args of
        [] ->
            Ok val

        (Node _ first) :: rest ->
            case val of
                Value.Lambda f ->
                    case evalExpression env first of
                        Err e ->
                            Err e

                        Ok rValue ->
                            case f rValue of
                                Err e ->
                                    Err e

                                Ok newF ->
                                    evalApply env newF rest

                Value.Custom name customArgs ->
                    args
                        |> Result.Extra.combineMap (\(Node _ arg) -> evalExpression env arg)
                        |> Result.map
                            (\added -> Value.Custom name (customArgs ++ added))

                _ ->
                    Err <| TypeError "Trying to apply a non-lambda non-variant"


functionToValue : Env -> FunctionImplementation -> Result EvalError Value
functionToValue env function =
    let
        go : Env -> List Pattern -> Result EvalError Value
        go newEnv args =
            case args of
                [] ->
                    evalExpression newEnv (Node.value function.expression)

                pattern :: tail ->
                    Ok
                        (Value.Lambda <|
                            \varValue ->
                                case match pattern varValue of
                                    Err e ->
                                        Err e

                                    Ok Nothing ->
                                        let
                                            message : String
                                            message =
                                                "Error in pattern matching while calling " ++ Node.value function.name
                                        in
                                        Err <| TypeError message

                                    Ok (Just add) ->
                                        go (newEnv |> Env.with add) tail
                        )
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
