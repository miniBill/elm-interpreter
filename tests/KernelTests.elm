module KernelTests exposing (suite)

import Core
import Elm.Kernel
import Elm.Syntax.Expression exposing (Case, CaseBlock, Expression(..), Function, FunctionImplementation, Lambda, LetBlock, LetDeclaration(..), RecordSetter)
import Elm.Syntax.Node as Node exposing (Node(..))
import Expect
import FastDict as Dict
import Rope exposing (Rope)
import Set
import Test exposing (Test, describe, test)


suite : Test
suite =
    kernelFunctions
        |> List.map testDefined
        |> describe "Check that all Kernel functions have been defined"


kernelFunctions : List String
kernelFunctions =
    Core.functions
        |> Dict.values
        |> Rope.fromList
        |> Rope.concatMap visitFunctionImplementation
        |> Rope.toList
        |> Set.fromList
        |> Set.toList


visitFunctionImplementation : FunctionImplementation -> Rope String
visitFunctionImplementation { expression } =
    visitExpression expression


visitExpression : Node Expression -> Rope String
visitExpression (Node _ expression) =
    case expression of
        Application children ->
            Rope.fromList children
                |> Rope.concatMap visitExpression

        OperatorApplication _ _ l r ->
            Rope.fromList [ l, r ]
                |> Rope.concatMap visitExpression

        FunctionOrValue ("Elm" :: "Kernel" :: "Scheduler" :: _) _ ->
            Rope.empty

        FunctionOrValue ("Elm" :: "Kernel" :: "Platform" :: _) _ ->
            Rope.empty

        FunctionOrValue (("Elm" :: "Kernel" :: _) as moduleName) name ->
            Rope.fromList [ String.join "." (moduleName ++ [ name ]) ]

        IfBlock cond true false ->
            Rope.fromList [ cond, true, false ]
                |> Rope.concatMap visitExpression

        Negation child ->
            visitExpression child

        TupledExpression children ->
            Rope.fromList children
                |> Rope.concatMap visitExpression

        ParenthesizedExpression child ->
            visitExpression child

        LetExpression letBlock ->
            visitLetBlock letBlock

        CaseExpression caseBlock ->
            visitCaseBlock caseBlock

        LambdaExpression lambda ->
            visitLambda lambda

        RecordExpr setters ->
            setters
                |> Rope.fromList
                |> Rope.concatMap visitRecordSetter

        ListExpr children ->
            Rope.fromList children
                |> Rope.concatMap visitExpression

        RecordAccess child _ ->
            visitExpression child

        RecordUpdateExpression _ setters ->
            setters
                |> Rope.fromList
                |> Rope.concatMap visitRecordSetter

        _ ->
            Rope.empty


visitLambda : Lambda -> Rope String
visitLambda { expression } =
    visitExpression expression


visitRecordSetter : Node RecordSetter -> Rope String
visitRecordSetter (Node _ ( _, expression )) =
    visitExpression expression


visitCaseBlock : CaseBlock -> Rope String
visitCaseBlock { expression, cases } =
    cases
        |> Rope.fromList
        |> Rope.concatMap visitCase
        |> Rope.appendTo (visitExpression expression)


visitCase : Case -> Rope String
visitCase ( _, expression ) =
    visitExpression expression


visitLetBlock : LetBlock -> Rope String
visitLetBlock { declarations, expression } =
    declarations
        |> Rope.fromList
        |> Rope.concatMap visitDeclaration
        |> Rope.appendTo (visitExpression expression)


visitDeclaration : Node LetDeclaration -> Rope String
visitDeclaration (Node _ letDeclaration) =
    case letDeclaration of
        LetFunction function ->
            visitFunction function

        LetDestructuring _ child ->
            visitExpression child


visitFunction : Function -> Rope String
visitFunction { declaration } =
    visitFunctionImplementation (Node.value declaration)


testDefined : String -> Test
testDefined name =
    test name <|
        \_ ->
            if Dict.member name Elm.Kernel.functions then
                Expect.pass

            else
                Expect.fail (name ++ " is not defined")
