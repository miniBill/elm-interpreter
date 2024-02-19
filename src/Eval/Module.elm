module Eval.Module exposing (eval, trace, traceOrEvalModule)

import Core
import Elm.Parser
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Expression(..))
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Module exposing (Module(..))
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Environment
import Eval.Expression
import FastDict as Dict
import List.Extra
import Result.MyExtra
import Rope exposing (Rope)
import Syntax exposing (fakeNode)
import Types exposing (CallTree, Env, Error(..), Value)
import Value exposing (unsupported)


eval : String -> Expression -> Result Error Value
eval source expression =
    let
        ( result, _, _ ) =
            traceOrEvalModule { trace = False } source expression
    in
    result


trace : String -> Expression -> ( Result Error Value, Rope CallTree, Rope String )
trace source expression =
    traceOrEvalModule { trace = True } source expression


traceOrEvalModule : { trace : Bool } -> String -> Expression -> ( Result Error Value, Rope CallTree, Rope String )
traceOrEvalModule cfg source expression =
    let
        maybeEnv : Result Error Env
        maybeEnv =
            source
                |> Elm.Parser.parseToFile
                |> Result.mapError ParsingError
                |> Result.andThen buildInitialEnv
    in
    case maybeEnv of
        Err e ->
            ( Err e, Rope.empty, Rope.empty )

        Ok env ->
            let
                maybeNode : a -> Node a
                maybeNode =
                    case expression of
                        FunctionOrValue [] name ->
                            let
                                needle : String
                                needle =
                                    name ++ " ="
                            in
                            source
                                |> String.split "\n"
                                |> List.Extra.findIndex
                                    (String.startsWith needle)
                                |> Maybe.map
                                    (\index ->
                                        Node
                                            { start = { row = index + 1, column = 1 }
                                            , end = { row = index + 1, column = 1 + String.length name }
                                            }
                                    )
                                |> Maybe.withDefault fakeNode

                        _ ->
                            fakeNode

                ( result, callTrees, logLines ) =
                    Eval.Expression.evalExpression
                        (maybeNode expression)
                        { trace = cfg.trace }
                        env
            in
            ( Result.mapError Types.EvalError result
            , callTrees
            , logLines
            )


buildInitialEnv : File -> Result Error Env
buildInitialEnv file =
    let
        moduleName : ModuleName
        moduleName =
            case Node.value file.moduleDefinition of
                NormalModule normal ->
                    Node.value normal.moduleName

                PortModule port_ ->
                    Node.value port_.moduleName

                EffectModule effect ->
                    Node.value effect.moduleName

        coreEnv : Env
        coreEnv =
            { currentModule = moduleName
            , callStack = []
            , functions = Core.functions
            , values = Dict.empty
            }

        addDeclaration : Node Declaration -> Env -> Result Error Env
        addDeclaration (Node _ decl) env =
            case decl of
                FunctionDeclaration function ->
                    let
                        (Node _ implementation) =
                            function.declaration
                    in
                    Ok (Environment.addFunction moduleName implementation env)

                PortDeclaration _ ->
                    Err <| Types.EvalError <| unsupported env "Port declaration"

                InfixDeclaration _ ->
                    Err <| Types.EvalError <| unsupported env "Infix declaration"

                Destructuring _ _ ->
                    Err <| Types.EvalError <| unsupported env "Top level destructuring"

                AliasDeclaration _ ->
                    Ok env

                CustomTypeDeclaration _ ->
                    Ok env
    in
    file.declarations
        |> Result.MyExtra.combineFoldl
            addDeclaration
            (Ok coreEnv)
