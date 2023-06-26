module Eval.Module exposing (eval, trace)

import Core
import Elm.Parser
import Elm.Processing
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Module exposing (Module(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Environment
import Eval.Expression
import FastDict as Dict
import Result.MyExtra
import Rope exposing (Rope)
import Syntax
import Types exposing (Env, Error(..), Expr, LogContinuation(..), LogLine, ModuleName, Value)


eval : String -> Expr -> Result Error Value
eval source expression =
    let
        ( result, _ ) =
            traceOrEvalModule { trace = False } source expression
    in
    result


trace : String -> Expr -> ( Result Error Value, Rope LogLine )
trace source expression =
    traceOrEvalModule { trace = True } source expression


traceOrEvalModule : { trace : Bool } -> String -> Expr -> ( Result Error Value, Rope LogLine )
traceOrEvalModule cfg source expression =
    let
        maybeEnv : Result Error Env
        maybeEnv =
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
                |> Result.andThen buildInitialEnv
    in
    case maybeEnv of
        Err e ->
            ( Err e, Rope.empty )

        Ok env ->
            let
                ( result, logLines ) =
                    Eval.Expression.evalExpression
                        expression
                        { trace = cfg.trace
                        , logContinuation = LogDone
                        }
                        env
            in
            ( Result.mapError Types.EvalError result
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
            { callStack = []
            , values =
                Dict.foldl
                    (\mn fs acc ->
                        Dict.foldl
                            (\fn f ->
                                Dict.insert
                                    (Syntax.qualifiedNameToString
                                        { moduleName = mn
                                        , name = fn
                                        }
                                    )
                                    (Expr f)
                            )
                            acc
                            fs
                    )
                    Dict.empty
                    Core.functions
            }

        addDeclaration : Node Declaration -> Env -> Result Error Env
        addDeclaration (Node _ decl) env =
            case decl of
                FunctionDeclaration function ->
                    let
                        (Node _ implementation) =
                            function.declaration
                    in
                    Ok (Environment.with moduleName implementation env)

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
