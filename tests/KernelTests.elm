module KernelTests exposing (suite)

import Core
import Elm.Kernel
import Elm.Syntax.Expression exposing (Case, CaseBlock, Expression(..), Function, FunctionImplementation, Lambda, LetBlock, LetDeclaration(..), RecordSetter)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Expect
import FastDict as Dict
import Rope exposing (Rope)
import Test exposing (Test, describe, test)


suite : Test
suite =
    kernelFunctions
        |> List.map testDefined
        |> describe "Check that all Kernel functions have been defined"


kernelFunctions : List ( ( ModuleName, String ), List String )
kernelFunctions =
    Core.functions
        |> Dict.values
        |> Rope.fromList
        |> Rope.concatMap (\module_ -> Dict.values module_ |> Rope.fromList)
        |> Rope.concatMap
            (\function ->
                let
                    (Node _ name) =
                        function.name
                in
                visitFunctionImplementation function
                    |> Rope.map (\required -> ( required, name ))
            )
        |> Rope.toList
        |> List.foldl
            (\( required, by ) acc ->
                Dict.insert
                    required
                    (by :: Maybe.withDefault [] (Dict.get required acc))
                    acc
            )
            Dict.empty
        |> Dict.toList


visitFunctionImplementation : FunctionImplementation -> Rope ( ModuleName, String )
visitFunctionImplementation { expression } =
    visitExpression expression


visitExpression : Node Expression -> Rope ( ModuleName, String )
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

        FunctionOrValue ("Elm" :: "Kernel" :: "Process" :: _) _ ->
            Rope.empty

        FunctionOrValue (("Elm" :: "Kernel" :: _) as moduleName) name ->
            Rope.singleton ( moduleName, name )

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


visitLambda : Lambda -> Rope ( ModuleName, String )
visitLambda { expression } =
    visitExpression expression


visitRecordSetter : Node RecordSetter -> Rope ( ModuleName, String )
visitRecordSetter (Node _ ( _, expression )) =
    visitExpression expression


visitCaseBlock : CaseBlock -> Rope ( ModuleName, String )
visitCaseBlock { expression, cases } =
    cases
        |> Rope.fromList
        |> Rope.concatMap visitCase
        |> Rope.appendTo (visitExpression expression)


visitCase : Case -> Rope ( ModuleName, String )
visitCase ( _, expression ) =
    visitExpression expression


visitLetBlock : LetBlock -> Rope ( ModuleName, String )
visitLetBlock { declarations, expression } =
    declarations
        |> Rope.fromList
        |> Rope.concatMap visitDeclaration
        |> Rope.appendTo (visitExpression expression)


visitDeclaration : Node LetDeclaration -> Rope ( ModuleName, String )
visitDeclaration (Node _ letDeclaration) =
    case letDeclaration of
        LetFunction function ->
            visitFunction function

        LetDestructuring _ child ->
            visitExpression child


visitFunction : Function -> Rope ( ModuleName, String )
visitFunction { declaration } =
    visitFunctionImplementation (Node.value declaration)


testDefined : ( ( ModuleName, String ), List String ) -> Test
testDefined ( ( moduleName, name ), requiredBy ) =
    let
        fullName : String
        fullName =
            String.join "." (moduleName ++ [ name ])

        error : Expect.Expectation
        error =
            Expect.fail
                (fullName
                    ++ " is not defined, but it's requried by "
                    ++ String.join ", " requiredBy
                )
    in
    test fullName <|
        \_ ->
            case Dict.get moduleName Elm.Kernel.functions of
                Just kernelModule ->
                    if Dict.member name kernelModule then
                        Expect.pass

                    else
                        error

                Nothing ->
                    error
