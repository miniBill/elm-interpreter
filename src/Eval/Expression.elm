module Eval.Expression exposing (evalExpression, evalFunction)

import Core
import Core.Basics
import Elm.Syntax.Expression as Expression exposing (Expression, LetDeclaration)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..), QualifiedNameRef)
import Env
import Eval.PartialResult as PartialResult
import Eval.Types as Types exposing (CallTree(..), CallTreeContinuation, Config, Eval, PartialEval, PartialEval2, PartialEval3, PartialResult(..))
import FastDict as Dict exposing (Dict)
import Kernel
import List.Extra
import Result.MyExtra
import Set exposing (Set)
import Syntax exposing (fakeNode)
import TopologicalSort
import Unicode
import Value exposing (Env, EnvValues, EvalError, EvalResult, Value(..), nameError, typeError, unsupported)


evalExpression : Eval (Node Expression) Value
evalExpression cfg env (Node _ expression) =
    let
        partial : PartialResult
        partial =
            case expression of
                Expression.UnitExpr ->
                    PartialValue [] Value.Unit

                Expression.Integer i ->
                    PartialValue [] (Value.Int i)

                Expression.Hex i ->
                    PartialValue [] (Value.Int i)

                Expression.Floatable f ->
                    PartialValue [] (Value.Float f)

                Expression.Literal string ->
                    PartialValue [] (Value.String string)

                Expression.CharLiteral c ->
                    PartialValue [] (Value.Char c)

                Expression.OperatorApplication "||" _ l r ->
                    evalShortCircuitOr cfg env l r

                Expression.OperatorApplication "&&" _ l r ->
                    evalShortCircuitAnd cfg env l r

                Expression.OperatorApplication opName _ l r ->
                    PartialExpression env
                        (fakeNode <|
                            Expression.Application
                                [ fakeNode <| Expression.Operator opName
                                , l
                                , r
                                ]
                        )
                        cfg.callTreeContinuation

                Expression.Application [] ->
                    PartialErr [] <| typeError env "Empty application"

                Expression.Application (first :: rest) ->
                    evalApplication cfg env first rest

                Expression.FunctionOrValue moduleName name ->
                    evalFunctionOrValue cfg env moduleName name

                Expression.IfBlock cond true false ->
                    evalIfBlock cfg env cond true false

                Expression.PrefixOperator opName ->
                    evalOperator env opName

                Expression.Operator opName ->
                    evalOperator env opName

                Expression.Negation child ->
                    evalNegation cfg env child

                Expression.TupledExpression exprs ->
                    evalTuple cfg env exprs

                Expression.ParenthesizedExpression child ->
                    PartialExpression env child cfg.callTreeContinuation

                Expression.LetExpression letBlock ->
                    evalLetBlock cfg env letBlock

                Expression.CaseExpression caseExpr ->
                    evalCase cfg env caseExpr

                Expression.LambdaExpression lambda ->
                    PartialValue [] <| PartiallyApplied env [] lambda.args Nothing lambda.expression

                Expression.RecordExpr fields ->
                    evalRecord cfg env fields

                Expression.ListExpr elements ->
                    evalList cfg env elements

                Expression.RecordAccess recordExpr field ->
                    evalRecordAccess cfg env recordExpr field

                Expression.RecordAccessFunction field ->
                    PartialValue [] <| evalRecordAccessFunction field

                Expression.RecordUpdateExpression name setters ->
                    evalRecordUpdate cfg env name setters

                Expression.GLSLExpression _ ->
                    PartialErr [] <| unsupported env "GLSL not supported"
    in
    case partial of
        PartialErr callTrees e ->
            ( Err e
            , callTrees
            )

        PartialValue callTrees v ->
            ( Ok v
            , callTrees
            )

        PartialExpression newEnv next callTreeContinuation ->
            evalExpression
                { cfg
                    | callTreeContinuation = callTreeContinuation
                }
                newEnv
                next


evalShortCircuitAnd : PartialEval2 (Node Expression) (Node Expression)
evalShortCircuitAnd cfg env l r =
    let
        ( value, callTrees ) =
            evalExpression cfg env l
    in
    case value of
        Ok (Bool False) ->
            PartialValue callTrees <| Bool False

        Ok (Bool True) ->
            PartialExpression env r <| \children -> cfg.callTreeContinuation (callTrees ++ children)

        Err e ->
            PartialErr callTrees e

        Ok v ->
            PartialErr callTrees <| typeError env <| "&& applied to non-Bool " ++ Value.toString v


evalShortCircuitOr : PartialEval2 (Node Expression) (Node Expression)
evalShortCircuitOr cfg env l r =
    let
        ( value, callTrees ) =
            evalExpression cfg env l
    in
    case value of
        Ok (Bool True) ->
            PartialValue callTrees <| Bool True

        Ok (Bool False) ->
            PartialExpression env r <| \children -> cfg.callTreeContinuation (callTrees ++ children)

        Err e ->
            PartialErr callTrees e

        Ok v ->
            PartialErr callTrees <| typeError env <| "|| applied to non-Bool " ++ Value.toString v


evalTuple : PartialEval (List (Node Expression))
evalTuple cfg env exprs =
    case exprs of
        [] ->
            PartialValue [] Value.Unit

        [ c ] ->
            PartialExpression env c cfg.callTreeContinuation

        [ l, r ] ->
            case evalExpression cfg env l of
                ( Err e, callTreeL ) ->
                    PartialErr callTreeL e

                ( Ok lValue, callTreeL ) ->
                    case evalExpression cfg env r of
                        ( Err e, callTreeR ) ->
                            PartialErr (callTreeL ++ callTreeR) e

                        ( Ok rValue, callTreeR ) ->
                            PartialValue (callTreeL ++ callTreeR) (Tuple lValue rValue)

        [ l, m, r ] ->
            case evalExpression cfg env l of
                ( Err e, callTreeL ) ->
                    PartialErr callTreeL e

                ( Ok lValue, callTreeL ) ->
                    case evalExpression cfg env m of
                        ( Err e, callTreeM ) ->
                            PartialErr (callTreeL ++ callTreeM) e

                        ( Ok mValue, callTreeM ) ->
                            case evalExpression cfg env r of
                                ( Err e, callTreeR ) ->
                                    PartialErr
                                        (callTreeL ++ callTreeM ++ callTreeR)
                                        e

                                ( Ok rValue, callTreeR ) ->
                                    PartialValue
                                        (callTreeL ++ callTreeM ++ callTreeR)
                                        (Triple lValue mValue rValue)

        _ :: _ :: _ :: _ :: _ ->
            PartialErr [] <| typeError env "Tuples with more than three elements are not supported"


evalApplication : PartialEval2 (Node Expression) (List (Node Expression))
evalApplication cfg env first rest =
    let
        ( firstValue, firstCallTrees ) =
            evalExpression cfg env first
    in
    case firstValue of
        Err e ->
            PartialErr firstCallTrees e

        Ok (Value.Custom name customArgs) ->
            case combineEval cfg env rest of
                ( Ok values, callTrees ) ->
                    PartialValue (firstCallTrees ++ callTrees) <| Value.Custom name (customArgs ++ values)

                ( Err e, callTrees ) ->
                    PartialErr (firstCallTrees ++ callTrees) e

        Ok (Value.PartiallyApplied localEnv oldArgs patterns maybeQualifiedName implementation) ->
            let
                ( used, leftover ) =
                    List.Extra.splitAt (patternsLength - oldArgsLength) rest

                oldArgsLength : Int
                oldArgsLength =
                    List.length oldArgs

                patternsLength : Int
                patternsLength =
                    List.length patterns
            in
            if not (List.isEmpty leftover) then
                -- Too many args, we split
                PartialExpression
                    env
                    (fakeNode <|
                        Expression.Application
                            (fakeNode
                                (Expression.Application (first :: used))
                                :: leftover
                            )
                    )
                    cfg.callTreeContinuation

            else
                case combineEval cfg env rest of
                    ( Err e, restCallTrees ) ->
                        PartialErr (firstCallTrees ++ restCallTrees) e

                    ( Ok values, restCallTrees ) ->
                        let
                            restLength : Int
                            restLength =
                                List.length rest

                            callTrees : List CallTree
                            callTrees =
                                firstCallTrees ++ restCallTrees
                        in
                        if oldArgsLength + restLength < patternsLength then
                            -- Still not enough
                            PartialValue callTrees <| Value.PartiallyApplied localEnv (oldArgs ++ values) patterns maybeQualifiedName implementation

                        else
                            -- Just right, we special case this for TCO
                            let
                                maybeNewEnvValues : EvalResult (Maybe EnvValues)
                                maybeNewEnvValues =
                                    match env
                                        (fakeNode <| ListPattern patterns)
                                        (List (oldArgs ++ values))
                            in
                            case maybeNewEnvValues of
                                Err e ->
                                    PartialErr callTrees e

                                Ok Nothing ->
                                    PartialErr callTrees <| typeError env "Could not match lambda patterns"

                                Ok (Just newEnvValues) ->
                                    case implementation of
                                        Node _ (Expression.FunctionOrValue (("Elm" :: "Kernel" :: _) as moduleName) name) ->
                                            let
                                                qualifiedName : QualifiedNameRef
                                                qualifiedName =
                                                    { moduleName = moduleName
                                                    , name = name
                                                    }

                                                fullName : String
                                                fullName =
                                                    Syntax.qualifiedNameToString qualifiedName
                                            in
                                            case Dict.get moduleName kernelFunctions of
                                                Nothing ->
                                                    PartialErr callTrees <| nameError env fullName

                                                Just kernelModule ->
                                                    case Dict.get name kernelModule of
                                                        Nothing ->
                                                            PartialErr callTrees <| nameError env fullName

                                                        Just ( _, f ) ->
                                                            let
                                                                ( kernelResult, children ) =
                                                                    f cfg
                                                                        (Env.call moduleName name env)
                                                                        values
                                                            in
                                                            PartialResult.fromValue
                                                                ( kernelResult
                                                                , if cfg.trace then
                                                                    CallNode "application"
                                                                        qualifiedName
                                                                        { args = values
                                                                        , result = kernelResult
                                                                        , children = children
                                                                        }
                                                                        :: callTrees

                                                                  else
                                                                    []
                                                                )

                                        _ ->
                                            PartialExpression
                                                (localEnv |> Env.with newEnvValues)
                                                implementation
                                                (call cfg maybeQualifiedName values callTrees)

        Ok other ->
            PartialErr [] <|
                typeError env <|
                    "Trying to apply "
                        ++ Value.toString other
                        ++ ", which is a non-lambda non-variant"


call : Config -> Maybe QualifiedNameRef -> List Value -> List CallTree -> CallTreeContinuation
call cfg maybeQualifiedName values callTrees =
    case maybeQualifiedName of
        Just qualifiedName ->
            \children result ->
                cfg.callTreeContinuation
                    (if cfg.trace then
                        CallNode "call"
                            qualifiedName
                            { args = values
                            , result = result
                            , children = children
                            }
                            :: callTrees

                     else
                        callTrees
                    )
                    result

        Nothing ->
            cfg.callTreeContinuation


combineEval : Eval (List (Node Expression)) (List Value)
combineEval cfg env expressions =
    Types.combineMap cfg env evalExpression expressions


evalFunctionOrValue : PartialEval2 ModuleName String
evalFunctionOrValue cfg env moduleName name =
    let
        fixedModuleName : ModuleName
        fixedModuleName =
            if List.isEmpty moduleName then
                env.currentModule

            else if moduleName == [ "JsArray" ] then
                -- TODO: Generic import aliases
                [ "Elm", "JsArray" ]

            else
                moduleName
    in
    if isVariant name then
        case ( moduleName, name ) of
            ( [], "True" ) ->
                PartialValue [] (Value.Bool True)

            ( [], "False" ) ->
                PartialValue [] (Value.Bool False)

            _ ->
                let
                    qualifiedNameRef : QualifiedNameRef
                    qualifiedNameRef =
                        { moduleName = fixedModuleName, name = name }
                in
                PartialValue [] (Value.Custom qualifiedNameRef [])

    else
        case moduleName of
            "Elm" :: "Kernel" :: _ ->
                case Dict.get moduleName env.functions of
                    Nothing ->
                        evalKernelFunction cfg env moduleName name

                    Just kernelModule ->
                        case Dict.get name kernelModule of
                            Nothing ->
                                evalKernelFunction cfg env moduleName name

                            Just function ->
                                PartiallyApplied
                                    (Env.call moduleName name env)
                                    []
                                    function.arguments
                                    (Just { moduleName = moduleName, name = name })
                                    function.expression
                                    |> PartialValue []

            _ ->
                case ( moduleName, Dict.get name env.values ) of
                    ( [], Just (PartiallyApplied localEnv [] [] maybeName implementation) ) ->
                        PartialExpression localEnv
                            implementation
                            (call cfg maybeName [] [])

                    ( [], Just value ) ->
                        PartialValue [] value

                    _ ->
                        let
                            maybeFunction : Maybe Expression.FunctionImplementation
                            maybeFunction =
                                let
                                    fromModule : Maybe Expression.FunctionImplementation
                                    fromModule =
                                        Dict.get fixedModuleName env.functions
                                            |> Maybe.andThen (Dict.get name)
                                in
                                if List.isEmpty moduleName then
                                    case fromModule of
                                        Just function ->
                                            Just function

                                        Nothing ->
                                            Dict.get name Core.Basics.functions

                                else
                                    fromModule
                        in
                        case maybeFunction of
                            Just function ->
                                if List.isEmpty function.arguments then
                                    PartialExpression
                                        (Env.call fixedModuleName name env)
                                        function.expression
                                        cfg.callTreeContinuation

                                else
                                    PartiallyApplied
                                        (Env.call fixedModuleName name env)
                                        []
                                        function.arguments
                                        (Just { moduleName = fixedModuleName, name = name })
                                        function.expression
                                        |> PartialValue []

                            Nothing ->
                                Syntax.qualifiedNameToString
                                    { moduleName = fixedModuleName
                                    , name = name
                                    }
                                    |> nameError env
                                    |> PartialErr []


evalIfBlock : PartialEval3 (Node Expression) (Node Expression) (Node Expression)
evalIfBlock cfg env cond true false =
    case evalExpression cfg env cond of
        ( Err e, callTrees ) ->
            PartialErr callTrees e

        ( Ok condValue, callTrees ) ->
            case condValue of
                Value.Bool True ->
                    PartialExpression env true <| \children -> cfg.callTreeContinuation (callTrees ++ children)

                Value.Bool False ->
                    PartialExpression env false <| \children -> cfg.callTreeContinuation (callTrees ++ children)

                _ ->
                    PartialErr callTrees <| typeError env "ifThenElse condition was not a boolean"


evalList : PartialEval (List (Node Expression))
evalList cfg env elements =
    elements
        |> combineEval cfg env
        |> Types.map List
        |> PartialResult.fromValue


evalRecord : PartialEval (List (Node Expression.RecordSetter))
evalRecord cfg env fields =
    let
        ( fieldNames, expressions ) =
            fields
                |> List.map (\(Node _ ( Node _ name, expression )) -> ( name, expression ))
                |> List.unzip
    in
    case
        combineEval cfg env expressions
    of
        ( Ok tuples, callTrees ) ->
            tuples
                |> List.map2 Tuple.pair fieldNames
                |> Dict.fromList
                |> Value.Record
                |> PartialValue callTrees

        ( Err e, callTrees ) ->
            PartialErr callTrees e


kernelFunctions : Dict ModuleName (Dict String ( Int, Eval (List Value) Value ))
kernelFunctions =
    Kernel.functions evalFunction


evalFunction : Kernel.EvalFunction
evalFunction cfg localEnv oldArgs patterns functionName implementation =
    let
        oldArgsLength : Int
        oldArgsLength =
            List.length oldArgs

        patternsLength : Int
        patternsLength =
            List.length patterns
    in
    if oldArgsLength < patternsLength then
        -- Still not enough
        ( Ok <| Value.PartiallyApplied localEnv oldArgs patterns functionName implementation, [] )

    else
        -- Just right, we special case this for TCO
        let
            maybeNewEnvValues : EvalResult (Maybe EnvValues)
            maybeNewEnvValues =
                match localEnv
                    (fakeNode <| ListPattern patterns)
                    (List oldArgs)
        in
        case maybeNewEnvValues of
            Err e ->
                ( Err e, [] )

            Ok Nothing ->
                ( Err <| typeError localEnv "Could not match lambda patterns", [] )

            Ok (Just newEnvValues) ->
                case implementation of
                    Node _ (Expression.FunctionOrValue (("Elm" :: "Kernel" :: _) as moduleName) name) ->
                        let
                            fullName : String
                            fullName =
                                Syntax.qualifiedNameToString { moduleName = moduleName, name = name }
                        in
                        case Dict.get moduleName kernelFunctions of
                            Nothing ->
                                ( Err <| nameError localEnv fullName, [] )

                            Just kernelModule ->
                                case Dict.get name kernelModule of
                                    Nothing ->
                                        ( Err <| nameError localEnv fullName, [] )

                                    Just ( _, f ) ->
                                        f cfg
                                            (Env.call moduleName name localEnv)
                                            []

                    _ ->
                        evalExpression
                            (case ( functionName, cfg.trace ) of
                                ( Just { moduleName, name }, True ) ->
                                    { cfg
                                        | callTreeContinuation =
                                            \children result ->
                                                cfg.callTreeContinuation
                                                    [ CallNode "evalFunction"
                                                        { moduleName = moduleName
                                                        , name = name
                                                        }
                                                        { args = oldArgs
                                                        , result = result
                                                        , children = children
                                                        }
                                                    ]
                                                    result
                                    }

                                _ ->
                                    cfg
                            )
                            (localEnv |> Env.with newEnvValues)
                            implementation


evalKernelFunction : PartialEval2 ModuleName String
evalKernelFunction cfg env moduleName name =
    case Dict.get moduleName kernelFunctions of
        Nothing ->
            PartialErr [] <| nameError env (String.join "." moduleName)

        Just kernelModule ->
            case Dict.get name kernelModule of
                Nothing ->
                    PartialErr [] <| nameError env <| Syntax.qualifiedNameToString { moduleName = moduleName, name = name }

                Just ( argCount, f ) ->
                    if argCount == 0 then
                        let
                            ( result, callTrees ) =
                                f cfg (Env.call moduleName name env) []
                        in
                        if cfg.trace then
                            let
                                callTree : CallTree
                                callTree =
                                    CallNode "evalKernelFunction"
                                        { moduleName = moduleName
                                        , name = name
                                        }
                                        { args = []
                                        , result = result
                                        , children = []
                                        }
                            in
                            case result of
                                Ok value ->
                                    PartialValue (callTree :: callTrees) value

                                Err e ->
                                    PartialErr (callTree :: callTrees) e

                        else
                            case result of
                                Ok value ->
                                    PartialValue callTrees value

                                Err e ->
                                    PartialErr callTrees e

                    else
                        PartiallyApplied (Env.empty moduleName)
                            []
                            (List.repeat argCount (fakeNode AllPattern))
                            (Just { moduleName = moduleName, name = name })
                            (fakeNode <| Expression.FunctionOrValue moduleName name)
                            |> PartialValue []


evalNegation : PartialEval (Node Expression)
evalNegation cfg env child =
    case evalExpression cfg env child of
        ( Err e, callTrees ) ->
            PartialErr callTrees e

        ( Ok (Value.Int i), callTrees ) ->
            PartialValue callTrees <| Value.Int -i

        ( Ok (Value.Float f), callTrees ) ->
            PartialValue callTrees <| Value.Float -f

        ( Ok _, callTrees ) ->
            PartialErr callTrees <| typeError env "Trying to negate a non-number"


evalLetBlock : PartialEval Expression.LetBlock
evalLetBlock cfg env letBlock =
    let
        envDefs : Set String
        envDefs =
            Set.union
                (Dict.get env.currentModule env.functions
                    |> Maybe.map (Dict.keys >> Set.fromList)
                    |> Maybe.withDefault Set.empty
                )
                (Dict.keys env.values |> Set.fromList)

        sortedDeclarations : Result TopologicalSort.SortError (List (Node LetDeclaration))
        sortedDeclarations =
            letBlock.declarations
                |> List.indexedMap
                    (\id declaration ->
                        { id = id + 1
                        , declaration = declaration
                        , defVars = declarationDefinedVariables declaration
                        , refVars = Set.diff (declarationFreeVariables declaration) envDefs
                        , cycleAllowed = isLetDeclarationFunction declaration
                        }
                    )
                |> TopologicalSort.sort
                    { id = .id
                    , defVars = .defVars
                    , refVars = .refVars
                    , cycleAllowed = .cycleAllowed
                    }
                |> Result.map (List.map .declaration >> List.reverse)

        newEnv : ( EvalResult Env, List CallTree )
        newEnv =
            case sortedDeclarations of
                Err TopologicalSort.IllegalCycle ->
                    ( Err <| typeError env "illegal cycle in let block"
                    , []
                    )

                Err TopologicalSort.InternalError ->
                    ( Err <| typeError env "internal error in let block"
                    , []
                    )

                Ok sd ->
                    -- We can't use combineMap and need to fold
                    -- because we need to change the environment for each call
                    List.foldl
                        (\declaration acc ->
                            case acc of
                                ( Err _, _ ) ->
                                    acc

                                ( Ok e, callTrees ) ->
                                    let
                                        ( res, callTree ) =
                                            addLetDeclaration cfg e declaration
                                    in
                                    ( res, callTree ++ callTrees )
                        )
                        ( Ok env, [] )
                        sd
    in
    case newEnv of
        ( Err e, callTrees ) ->
            PartialErr callTrees e

        ( Ok ne, callTrees ) ->
            PartialExpression ne letBlock.expression <| \children -> cfg.callTreeContinuation (callTrees ++ children)


isLetDeclarationFunction : Node LetDeclaration -> Bool
isLetDeclarationFunction (Node _ d) =
    case d of
        Expression.LetFunction { declaration } ->
            List.length (Node.value declaration).arguments > 0

        _ ->
            False


addLetDeclaration : Eval (Node LetDeclaration) Env
addLetDeclaration cfg acc ((Node _ letDeclaration) as node) =
    case letDeclaration of
        Expression.LetFunction { declaration } ->
            case declaration of
                Node _ ({ name, expression } as implementation) ->
                    if isLetDeclarationFunction node then
                        ( Ok <| Env.addFunction acc.currentModule implementation acc, [] )

                    else
                        case evalExpression cfg acc expression of
                            ( Err e, callTree ) ->
                                ( Err e, callTree )

                            ( Ok value, callTree ) ->
                                ( Ok <| Env.addValue (Node.value name) value acc, callTree )

        Expression.LetDestructuring letPattern letExpression ->
            case evalExpression cfg acc letExpression of
                ( Err e, callTree ) ->
                    ( Err e, callTree )

                ( Ok letValue, callTree ) ->
                    case match acc letPattern letValue of
                        Err e ->
                            ( Err e, callTree )

                        Ok Nothing ->
                            ( Err <| typeError acc "Could not match pattern inside let"
                            , callTree
                            )

                        Ok (Just patternEnv) ->
                            ( Ok (Env.with patternEnv acc), callTree )


declarationFreeVariables : Node LetDeclaration -> Set String
declarationFreeVariables (Node _ letDeclaration) =
    case letDeclaration of
        Expression.LetFunction { declaration } ->
            let
                { name, arguments, expression } =
                    Node.value declaration
            in
            Set.diff (freeVariables expression)
                (List.foldl (\p -> Set.union (patternDefinedVariables p))
                    (Set.singleton (Node.value name))
                    arguments
                )

        Expression.LetDestructuring pattern expression ->
            Set.diff (freeVariables expression) (patternDefinedVariables pattern)


letFreeVariables : Expression.LetBlock -> Set String
letFreeVariables { declarations, expression } =
    Set.diff
        (List.foldl (\d -> Set.union (declarationFreeVariables d)) (freeVariables expression) declarations)
        (List.foldl (\d -> Set.union (declarationDefinedVariables d)) Set.empty declarations)


caseFreeVariables : Expression.Case -> Set String
caseFreeVariables ( pattern, expression ) =
    Set.diff (freeVariables expression) (patternDefinedVariables pattern)


freeVariables : Node Expression -> Set String
freeVariables (Node _ expr) =
    case expr of
        Expression.Application expressions ->
            List.foldl (\e -> Set.union (freeVariables e)) Set.empty expressions

        Expression.OperatorApplication _ _ l r ->
            Set.union (freeVariables l) (freeVariables r)

        Expression.FunctionOrValue [] name ->
            if isVariant name then
                Set.empty

            else
                Set.singleton name

        Expression.IfBlock cond true false ->
            Set.union (freeVariables cond) (Set.union (freeVariables true) (freeVariables false))

        Expression.Negation child ->
            freeVariables child

        Expression.TupledExpression expressions ->
            List.foldl (\e -> Set.union (freeVariables e)) Set.empty expressions

        Expression.ParenthesizedExpression child ->
            freeVariables child

        Expression.LetExpression block ->
            letFreeVariables block

        Expression.CaseExpression { expression, cases } ->
            List.foldl (\c -> Set.union (caseFreeVariables c)) (freeVariables expression) cases

        Expression.LambdaExpression { expression, args } ->
            Set.diff (freeVariables expression)
                (List.foldl (\p -> Set.union (patternDefinedVariables p)) Set.empty args)

        Expression.RecordExpr setters ->
            List.foldl (\(Node _ ( _, e )) -> Set.union (freeVariables e)) Set.empty setters

        Expression.ListExpr expressions ->
            List.foldl (\e -> Set.union (freeVariables e)) Set.empty expressions

        Expression.RecordAccess record _ ->
            freeVariables record

        Expression.RecordUpdateExpression (Node _ s) setters ->
            List.foldl (\(Node _ ( _, e )) -> Set.union (freeVariables e)) (Set.singleton s) setters

        _ ->
            Set.empty


patternDefinedVariables : Node Pattern -> Set String
patternDefinedVariables (Node _ pattern) =
    case pattern of
        TuplePattern patterns ->
            List.foldl (\p -> Set.union (patternDefinedVariables p)) Set.empty patterns

        RecordPattern fields ->
            List.foldl (\(Node _ s) -> Set.insert s) Set.empty fields

        UnConsPattern head tail ->
            Set.union (patternDefinedVariables head) (patternDefinedVariables tail)

        ListPattern patterns ->
            List.foldl (\p -> Set.union (patternDefinedVariables p)) Set.empty patterns

        VarPattern name ->
            Set.singleton name

        NamedPattern _ patterns ->
            List.foldl (\p -> Set.union (patternDefinedVariables p)) Set.empty patterns

        AsPattern p (Node _ s) ->
            Set.insert s (patternDefinedVariables p)

        ParenthesizedPattern p ->
            patternDefinedVariables p

        _ ->
            Set.empty


declarationDefinedVariables : Node LetDeclaration -> Set String
declarationDefinedVariables (Node _ letDeclaration) =
    case letDeclaration of
        Expression.LetFunction { declaration } ->
            Set.singleton <| Node.value (Node.value declaration).name

        Expression.LetDestructuring letPattern _ ->
            patternDefinedVariables letPattern


evalRecordAccess : PartialEval2 (Node Expression) (Node String)
evalRecordAccess cfg env recordExpr (Node _ field) =
    case evalExpression cfg env recordExpr of
        ( Ok value, callTree ) ->
            case value of
                Value.Record fields ->
                    case Dict.get field fields of
                        Just fieldValue ->
                            PartialValue callTree fieldValue

                        Nothing ->
                            PartialErr callTree <| typeError env <| "Field " ++ field ++ " not found [record access]"

                _ ->
                    PartialErr callTree <| typeError env "Trying to access a field on a non-record value"

        ( Err e, callTree ) ->
            PartialErr callTree e


evalRecordAccessFunction : String -> Value
evalRecordAccessFunction field =
    PartiallyApplied
        (Env.empty [])
        []
        [ fakeNode (VarPattern "r") ]
        Nothing
        (fakeNode <|
            Expression.RecordAccess
                (fakeNode <| Expression.FunctionOrValue [] "r")
                (fakeNode <| String.dropLeft 1 field)
        )


evalRecordUpdate : PartialEval2 (Node String) (List (Node Expression.RecordSetter))
evalRecordUpdate cfg env (Node _ name) setters =
    case evalExpression cfg env (fakeNode <| Expression.FunctionOrValue [] name) of
        ( Err e, callTree ) ->
            PartialErr callTree e

        ( Ok (Value.Record _), callTree ) ->
            let
                ( fieldNames, fieldExpressions ) =
                    setters
                        |> List.map
                            (\(Node _ ( Node _ fieldName, fieldExpression )) ->
                                ( fieldName
                                , fieldExpression
                                )
                            )
                        |> List.unzip
            in
            case combineEval cfg env fieldExpressions of
                ( Err e, callTrees ) ->
                    PartialErr (callTree ++ callTrees) e

                ( Ok fieldValues, callTrees ) ->
                    List.map2 Tuple.pair fieldNames fieldValues
                        |> Dict.fromList
                        |> Value.Record
                        |> PartialValue (callTree ++ callTrees)

        ( Ok _, callTree ) ->
            PartialErr callTree <| typeError env "Trying to update fields on a value which is not a record"


evalOperator : Env -> String -> PartialResult
evalOperator env opName =
    case Dict.get opName Core.operators of
        Nothing ->
            PartialErr [] <| nameError env opName

        Just kernelFunction ->
            PartiallyApplied
                (Env.call kernelFunction.moduleName opName env)
                []
                [ fakeNode <| VarPattern "l", fakeNode <| VarPattern "r" ]
                Nothing
                (fakeNode <|
                    Expression.Application
                        [ fakeNode <| Expression.FunctionOrValue kernelFunction.moduleName kernelFunction.name
                        , fakeNode <| Expression.FunctionOrValue [] "l"
                        , fakeNode <| Expression.FunctionOrValue [] "r"
                        ]
                )
                |> PartialValue []


isVariant : String -> Bool
isVariant name =
    case String.uncons name of
        Nothing ->
            False

        Just ( first, _ ) ->
            Unicode.isUpper first


evalCase : PartialEval Expression.CaseBlock
evalCase cfg env { expression, cases } =
    case evalExpression cfg env expression of
        ( Err e, callTree ) ->
            PartialErr callTree e

        ( Ok exprValue, callTree ) ->
            let
                result : Result EvalError (Maybe PartialResult)
                result =
                    cases
                        |> Result.MyExtra.combineFoldl
                            (\( pattern, result2 ) acc ->
                                case acc of
                                    Just _ ->
                                        Ok acc

                                    Nothing ->
                                        case match env pattern exprValue of
                                            Err e ->
                                                Err e

                                            Ok Nothing ->
                                                Ok Nothing

                                            Ok (Just additionalEnv) ->
                                                PartialExpression
                                                    (Env.with additionalEnv env)
                                                    result2
                                                    (\children -> cfg.callTreeContinuation (callTree ++ children))
                                                    |> Just
                                                    |> Ok
                            )
                            (Ok Nothing)
            in
            case result of
                Ok Nothing ->
                    PartialErr callTree <| typeError env <| "Missing case branch for " ++ Value.toString exprValue

                Ok (Just res) ->
                    res

                Err e ->
                    PartialErr callTree e


match : Env -> Node Pattern -> Value -> EvalResult (Maybe EnvValues)
match env (Node _ pattern) value =
    let
        ok : a -> Result error (Maybe a)
        ok val =
            Ok (Just val)

        noMatch : Result error (Maybe a)
        noMatch =
            Ok Nothing

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
            ok Dict.empty

        ( UnitPattern, _ ) ->
            noMatch

        ( AllPattern, _ ) ->
            ok Dict.empty

        ( ParenthesizedPattern subPattern, _ ) ->
            match env subPattern value

        ( NamedPattern namePattern argsPatterns, Value.Custom variant args ) ->
            -- Two names from different modules can never have the same type
            -- so if we assume the code typechecks we can skip the module name check
            if namePattern.name == variant.name then
                let
                    matchNamedPatternHelper :
                        EnvValues
                        -> ( List (Node Pattern), List Value )
                        -> EvalResult (Maybe EnvValues)
                    matchNamedPatternHelper envValues queue =
                        case queue of
                            ( [], [] ) ->
                                ok envValues

                            ( patternHead :: patternTail, argHead :: argTail ) ->
                                match env patternHead argHead
                                    |> andThen
                                        (\newEnvValues ->
                                            matchNamedPatternHelper (Dict.union newEnvValues envValues) ( patternTail, argTail )
                                        )

                            _ ->
                                Err <| typeError env "Mismatched number of arguments to variant"
                in
                matchNamedPatternHelper Dict.empty ( argsPatterns, args )

            else
                noMatch

        ( NamedPattern _ _, _ ) ->
            noMatch

        ( ListPattern [], List [] ) ->
            -- We assume the code typechecks!
            ok Dict.empty

        ( ListPattern (patternHead :: patternTail), List (listHead :: listTail) ) ->
            match env patternHead listHead
                |> andThen
                    (\headEnv ->
                        match env (fakeNode <| ListPattern patternTail) (List listTail)
                            |> andThen
                                (\tailEnv ->
                                    ok
                                        (Dict.union tailEnv headEnv)
                                )
                    )

        ( UnConsPattern patternHead patternTail, Value.List (listHead :: listTail) ) ->
            match env patternHead listHead
                |> andThen
                    (\headEnv ->
                        match env patternTail (List listTail)
                            |> andThen
                                (\tailEnv ->
                                    ok
                                        (Dict.union tailEnv headEnv)
                                )
                    )

        ( UnConsPattern _ _, _ ) ->
            noMatch

        ( VarPattern name, _ ) ->
            ok <| Dict.insert name value Dict.empty

        ( ListPattern _, _ ) ->
            noMatch

        ( CharPattern c, Value.Char d ) ->
            if c == d then
                ok Dict.empty

            else
                noMatch

        ( CharPattern _, _ ) ->
            noMatch

        ( StringPattern c, Value.String d ) ->
            if c == d then
                ok Dict.empty

            else
                noMatch

        ( StringPattern _, _ ) ->
            noMatch

        ( IntPattern c, Value.Int d ) ->
            if c == d then
                ok Dict.empty

            else
                noMatch

        ( IntPattern _, _ ) ->
            noMatch

        ( HexPattern c, Value.Int d ) ->
            if c == d then
                ok Dict.empty

            else
                noMatch

        ( HexPattern _, _ ) ->
            noMatch

        ( FloatPattern c, Value.Float d ) ->
            if c == d then
                ok Dict.empty

            else
                noMatch

        ( FloatPattern _, _ ) ->
            noMatch

        ( TuplePattern [ lpattern, rpattern ], Value.Tuple lvalue rvalue ) ->
            match env lpattern lvalue
                |> andThen
                    (\lenv ->
                        match env rpattern rvalue
                            |> andThen
                                (\renv ->
                                    ok <| Dict.union renv lenv
                                )
                    )

        ( TuplePattern [ lpattern, mpattern, rpattern ], Value.Triple lvalue mvalue rvalue ) ->
            match env lpattern lvalue
                |> andThen
                    (\lenv ->
                        match env mpattern mvalue
                            |> andThen
                                (\menv ->
                                    match env rpattern rvalue
                                        |> andThen
                                            (\renv ->
                                                ok <| Dict.union renv <| Dict.union menv lenv
                                            )
                                )
                    )

        ( TuplePattern _, _ ) ->
            noMatch

        ( AsPattern childPattern (Node _ asName), _ ) ->
            match env childPattern value
                |> andThen
                    (\e -> ok <| Dict.insert asName value e)

        ( RecordPattern fields, Value.Record fieldValues ) ->
            List.foldl
                (\(Node _ fieldName) ->
                    andThen
                        (\acc ->
                            case Dict.get fieldName fieldValues of
                                Nothing ->
                                    Err <| typeError env <| "Field " ++ fieldName ++ " not found in record"

                                Just fieldValue ->
                                    ok <| Dict.insert fieldName fieldValue acc
                        )
                )
                (ok Dict.empty)
                fields

        ( RecordPattern _, _ ) ->
            noMatch
