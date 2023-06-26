module Eval.Expression exposing (evalExpression, evalFunction)

import Core
import Core.Basics
import Elm.Interface exposing (Exposed(..))
import Environment
import Eval exposing (match, matchInfallible, nameError, typeError, unsupported)
import Expr
import FastDict as Dict exposing (Dict)
import Kernel
import List.Extra
import Result.MyExtra
import Rope exposing (Rope)
import Set exposing (Set)
import Syntax exposing (fakeNode)
import TopologicalSort
import Types exposing (Env, Error(..), Eval, EvalConfig, EvalResult, Expr(..), ExprOrValue(..), LogContinuation(..), LogLine, Pattern(..), QualifiedNameRef, Value(..))
import Unicode


evalExpression : Expr -> Eval Value
evalExpression expression cfg env =
    let
        ( result, logLines ) =
            innerEvalExpr expression cfg env

        equal : String -> String -> String
        equal l r =
            if String.contains "\n" l || String.contains "\n" r then
                l ++ "\n\n==>\n\n" ++ r

            else
                l ++ " ==> " ++ r
    in
    case result of
        Err e ->
            ( Err e, logLines )

        Ok (Value v) ->
            ( Ok v
            , if cfg.trace then
                logLines
                    |> Rope.prepend
                        { message = Expr.toString expression
                        , env = relevantEnv env expression
                        }
                    |> Rope.append
                        { message = equal (Expr.toString expression) (Expr.valueToString v)
                        , env = relevantEnv env expression
                        }
                    |> applyLogContinuation cfg.logContinuation

              else
                Rope.empty
            )

        Ok (Expr nextEnv nextExpr) ->
            evalExpression nextExpr
                { cfg
                    | logContinuation =
                        if cfg.trace then
                            cfg.logContinuation
                                |> LogPrepend
                                    { stack = env.callStack
                                    , message = Expr.toString expression
                                    , env = relevantEnv env expression
                                    }

                        else
                            LogDone
                }
                nextEnv


innerEvalExpr : Expr -> Eval ExprOrValue
innerEvalExpr expr cfg env =
    case expr of
        BinOp l "||" r ->
            evalShortCircuitOr l r cfg env

        BinOp l "&&" r ->
            evalShortCircuitAnd l r cfg env

        BinOp l opName r ->
            evalBinOp l opName r cfg env

        Apply first rest ->
            evalApplication first rest cfg env

        Custom moduleName name ->
            evalFunctionOrValue moduleName name cfg env

        IfThenElse cond true false ->
            evalIfThenElse cond true false cfg env

        PrefixOperator opName ->
            evalOperator opName cfg env

        Operator opName ->
            evalOperator opName cfg env

        Negation child ->
            evalNegation child cfg env

        TupledExpression exprs ->
            evalTuple exprs cfg env

        ParenthesizedExpression child ->
            Expr child cfg env

        LetExpression letBlock ->
            evalLetBlock letBlock cfg env

        CaseExpression caseExpr ->
            evalCase caseExpr cfg env

        LambdaExpression lambda ->
            Eval.succeed <| PartiallyApplied env [] lambda.args Nothing lambda.expression

        RecordExpr fields ->
            evalRecord fields cfg env

        RecordAccess recordExpr field ->
            evalRecordAccess recordExpr field cfg env

        RecordAccessFunction field ->
            Eval.succeed <| evalRecordAccessFunction field

        RecordUpdateExpression name setters ->
            evalRecordUpdate name setters cfg env

        GLSLExpression _ ->
            Eval.fail <| unsupported env "GLSL not supported"

        RecordUpdate _ _ ->
            Debug.todo "branch 'RecordUpdate _ _' not implemented"

        Variable _ ->
            Debug.todo "branch 'Variable _' not implemented"

        Case _ _ ->
            Debug.todo "branch 'Case _ _' not implemented"

        Lambda _ _ _ ->
            Debug.todo "branch 'Lambda _ _ _' not implemented"

        Negate _ ->
            Debug.todo "branch 'Negate _' not implemented"

        LetIn _ _ ->
            Debug.todo "branch 'LetIn _ _' not implemented"


evalBinOp : Expr -> BinOp -> Expr -> EvalConfig -> Env -> EvalResult Expr
evalBinOp arg1 arg2 arg3 arg4 arg5 =
    Debug.todo "TODO"


applyLogContinuation : LogContinuation -> Rope LogLine -> Rope LogLine
applyLogContinuation k lines =
    case k of
        LogDone ->
            lines

        LogAppendTo before andThen ->
            lines
                |> Rope.appendTo before
                |> applyLogContinuation andThen

        LogPrepend line andThen ->
            lines
                |> Rope.prepend line
                |> applyLogContinuation andThen

        LogAppend line andThen ->
            lines
                |> Rope.append line
                |> applyLogContinuation andThen


relevantEnv : Env -> Expr -> Dict String Value
relevantEnv env expr =
    freeVariables expr
        |> Set.foldl
            (\var acc ->
                case Dict.get var env.values of
                    Nothing ->
                        acc

                    Just value ->
                        Dict.insert var value acc
            )
            Dict.empty


evalShortCircuitAnd : Expr -> Expr -> Eval Expr
evalShortCircuitAnd l r cfg env =
    evalExpression l cfg env
        |> Eval.andThen
            (\value ->
                case value of
                    Bool False ->
                        Eval.succeed <| Bool False

                    Bool True ->
                        Eval.succeed r

                    v ->
                        Eval.fail <| typeError env <| "&& applied to non-Bool " ++ Expr.toString v
            )


evalShortCircuitOr : Expr -> Expr -> Eval Expr
evalShortCircuitOr l r cfg env =
    evalExpression l cfg env
        |> Eval.andThen
            (\value ->
                case value of
                    Bool True ->
                        Eval.succeed <| Bool True

                    Bool False ->
                        Eval.succeed r

                    v ->
                        Eval.fail <| typeError env <| "|| applied to non-Bool " ++ Expr.toString v
            )


evalTuple : List Expr -> Eval Expr
evalTuple exprs cfg env =
    case exprs of
        [] ->
            Eval.succeed Expr.Unit

        [ c ] ->
            Expr c cfg env

        [ l, r ] ->
            Expr l cfg env
                |> Eval.andThen
                    (\lValue ->
                        Expr r cfg env
                            |> Eval.andThen
                                (\rValue ->
                                    Eval.succeed (Tuple lValue rValue)
                                )
                    )
                |> PartialValue

        [ l, m, r ] ->
            Expr l cfg env
                |> Eval.andThen
                    (\lValue ->
                        Expr m cfg env
                            |> Eval.andThen
                                (\mValue ->
                                    Expr r cfg env
                                        |> Eval.andThen
                                            (\rValue ->
                                                Eval.succeed (Triple lValue mValue rValue)
                                            )
                                )
                    )
                |> PartialValue

        _ :: _ :: _ :: _ :: _ ->
            Eval.fail <| typeError env "Tuples with more than three elements are not supported"


evalApplication : Expr -> List Expr -> Eval Expr
evalApplication first rest cfg env =
    let
        inner : Env -> List Value -> List (Node Pattern) -> Maybe QualifiedNameRef -> Expr -> PartialResult
        inner localEnv oldArgs patterns maybeQualifiedName implementation =
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
                Expr
                    (fakeNode <|
                        Expr.Application
                            (fakeNode
                                (Expr.Application (first :: used))
                                :: leftover
                            )
                    )
                    cfg
                    env

            else
                Types.combineMap Expr rest cfg env
                    |> Eval.andThen
                        (\values ->
                            let
                                restLength : Int
                                restLength =
                                    List.length rest

                                args : List Value
                                args =
                                    oldArgs ++ values
                            in
                            if oldArgsLength + restLength < patternsLength then
                                -- Still not enough
                                Eval.succeed <| Expr.PartiallyApplied localEnv args patterns maybeQualifiedName implementation

                            else
                                -- Just right, we special case this for TCO
                                evalFullyApplied localEnv args patterns maybeQualifiedName implementation cfg env
                        )
    in
    Expr first cfg env
        |> Eval.andThen
            (\firstValue ->
                case firstValue of
                    Expr.Custom name customArgs ->
                        Types.combineMap Expr rest cfg env
                            |> Types.map (\values -> Expr.Custom name (customArgs ++ values))
                            |> PartialValue

                    Expr.PartiallyApplied localEnv oldArgs patterns maybeQualifiedName implementation ->
                        inner localEnv oldArgs patterns maybeQualifiedName implementation

                    other ->
                        Eval.fail <|
                            typeError env <|
                                "Trying to apply "
                                    ++ Expr.toString other
                                    ++ ", which is a non-lambda non-variant"
            )


evalFullyApplied : Env -> List Value -> List (Node Pattern) -> Maybe QualifiedNameRef -> Expr -> Eval Expr
evalFullyApplied localEnv args patterns maybeQualifiedName implementation cfg env =
    let
        maybeNewEnvValues : Result EvalError (Maybe EnvValues)
        maybeNewEnvValues =
            match env
                (fakeNode <| ListPattern patterns)
                (List args)
    in
    case maybeNewEnvValues of
        Err e ->
            Eval.fail e

        Ok Nothing ->
            Eval.fail <| typeError env "Could not match lambda patterns"

        Ok (Just newEnvValues) ->
            case implementation of
                Node _ (FunctionOrValue (("Elm" :: "Kernel" :: _) as moduleName) name) ->
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
                            Eval.fail <| nameError env fullName

                        Just kernelModule ->
                            case Dict.get name kernelModule of
                                Nothing ->
                                    Eval.fail <| nameError env fullName

                                Just ( _, f ) ->
                                    f args
                                        cfg
                                        (Environment.call qualifiedName env)

                _ ->
                    call
                        maybeQualifiedName
                        implementation
                        args
                        cfg
                        (localEnv |> Environment.with newEnvValues)


call : Maybe QualifiedNameRef -> Expr -> List Expr -> Eval Expr
call maybeQualifiedName implementation values cfg env =
    case maybeQualifiedName of
        Just qualifiedName ->
            Expr implementation
                { cfg
                    | callTreeContinuation =
                        if cfg.trace then
                            CTCCall qualifiedName values cfg.callTreeContinuation

                        else
                            cfg.callTreeContinuation
                }
                (Environment.call qualifiedName.moduleName qualifiedName.name env)

        Nothing ->
            Expr implementation cfg env


evalFunctionOrValue : ModuleName -> String -> Eval Expr
evalFunctionOrValue moduleName name cfg env =
    if isVariant name then
        evalVariant moduleName env name

    else
        evalNonVariant moduleName name cfg env


fixModuleName : ModuleName -> Env -> ModuleName
fixModuleName moduleName env =
    if List.isEmpty moduleName then
        env.currentModule

    else if moduleName == [ "JsArray" ] then
        -- TODO: Generic import aliases
        [ "Elm", "JsArray" ]

    else
        moduleName


evalVariant : ModuleName -> Env -> String -> PartialResult
evalVariant moduleName env name =
    let
        variant0 : ModuleName -> String -> PartialResult
        variant0 modName ctorName =
            Eval.succeed <| Expr.Custom { moduleName = modName, name = ctorName } []

        variant1 : ModuleName -> String -> PartialResult
        variant1 modName ctorName =
            Eval.succeed <|
                Expr.PartiallyApplied
                    (Environment.empty modName)
                    []
                    [ fakeNode <| VarPattern "$x" ]
                    Nothing
                    (fakeNode <|
                        Expr.Application
                            [ fakeNode <| FunctionOrValue modName ctorName
                            , fakeNode <| FunctionOrValue [] "$x"
                            ]
                    )
    in
    case ( moduleName, name ) of
        ( [], "True" ) ->
            Eval.succeed <| Expr.Bool True

        ( [], "False" ) ->
            Eval.succeed <| Expr.Bool False

        ( [], "Nothing" ) ->
            variant0 [ "Maybe" ] "Nothing"

        ( [], "Just" ) ->
            variant1 [ "Maybe" ] "Just"

        ( [], "Err" ) ->
            variant1 [ "Result" ] "Err"

        ( [], "Ok" ) ->
            variant1 [ "Result" ] "Ok"

        ( [], "LT" ) ->
            variant0 [ "Basics" ] "LT"

        ( [], "EQ" ) ->
            variant0 [ "Basics" ] "EQ"

        ( [], "GT" ) ->
            variant0 [ "Basics" ] "GT"

        _ ->
            let
                fixedModuleName : ModuleName
                fixedModuleName =
                    fixModuleName moduleName env

                qualifiedNameRef : QualifiedNameRef
                qualifiedNameRef =
                    { moduleName = fixedModuleName, name = name }
            in
            Eval.succeed <| Expr.Custom qualifiedNameRef []


evalNonVariant : ModuleName -> String -> Eval Expr
evalNonVariant moduleName name cfg env =
    case moduleName of
        "Elm" :: "Kernel" :: _ ->
            case Dict.get moduleName env.functions of
                Nothing ->
                    evalKernelFunction moduleName name cfg env

                Just kernelModule ->
                    case Dict.get name kernelModule of
                        Nothing ->
                            evalKernelFunction moduleName name cfg env

                        Just function ->
                            PartiallyApplied
                                (Environment.call moduleName name env)
                                []
                                function.arguments
                                (Just { moduleName = moduleName, name = name })
                                function.expression
                                |> Eval.succeed

        _ ->
            case ( moduleName, Dict.get name env.values ) of
                ( [], Just (PartiallyApplied localEnv [] [] maybeName implementation) ) ->
                    call maybeName implementation [] cfg localEnv

                ( [], Just value ) ->
                    Eval.succeed value

                _ ->
                    let
                        fixedModuleName : ModuleName
                        fixedModuleName =
                            fixModuleName moduleName env

                        maybeFunction : Maybe FunctionImplementation
                        maybeFunction =
                            let
                                fromModule : Maybe FunctionImplementation
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
                            let
                                qualifiedNameRef : QualifiedNameRef
                                qualifiedNameRef =
                                    { moduleName = fixedModuleName, name = name }
                            in
                            if List.isEmpty function.arguments then
                                call (Just qualifiedNameRef) function.expression [] cfg env

                            else
                                PartiallyApplied
                                    (Environment.call fixedModuleName name env)
                                    []
                                    function.arguments
                                    (Just qualifiedNameRef)
                                    function.expression
                                    |> Eval.succeed

                        Nothing ->
                            Syntax.qualifiedNameToString
                                { moduleName = fixedModuleName
                                , name = name
                                }
                                |> nameError env
                                |> Eval.fail


evalIfThenElse : Expr -> Expr -> Expr -> Eval Expr
evalIfThenElse cond true false cfg env =
    Expr cond cfg env
        |> Eval.andThen
            (\condValue ->
                case condValue of
                    Expr.Bool True ->
                        Expr true cfg env

                    Expr.Bool False ->
                        Expr false cfg env

                    _ ->
                        Eval.fail <| typeError env "ifThenElse condition was not a boolean"
            )


evalList : List Expr -> Eval Expr
evalList elements cfg env =
    Types.combineMap Expr elements cfg env
        |> Types.map List
        |> PartialValue


evalRecord : List (Node RecordSetter) -> Eval Expr
evalRecord fields cfg env =
    let
        ( fieldNames, expressions ) =
            fields
                |> List.map (\(Node _ ( Node _ name, expression )) -> ( name, expression ))
                |> List.unzip
    in
    Types.combineMap Expr expressions cfg env
        |> Types.map
            (\tuples ->
                tuples
                    |> List.map2 Tuple.pair fieldNames
                    |> Dict.fromList
                    |> Expr.Record
            )
        |> PartialValue


kernelFunctions : Dict ModuleName (Dict String ( Int, List Value -> Eval Value ))
kernelFunctions =
    Kernel.functions evalFunction


evalFunction : Kernel.EvalFunction
evalFunction oldArgs patterns functionName implementation cfg localEnv =
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
        Eval.succeed <| Expr.PartiallyApplied localEnv oldArgs patterns functionName implementation

    else
        -- Just right, we special case this for TCO
        let
            maybeNewEnvValues : Result EvalError (Dict String Expr)
            maybeNewEnvValues =
                matchInfallible localEnv
                    (fakeNode <| ListPattern patterns)
                    (List oldArgs)
        in
        case maybeNewEnvValues of
            Err e ->
                Eval.fail e

            Ok (Just newEnvValues) ->
                case implementation of
                    Node _ (FunctionOrValue (("Elm" :: "Kernel" :: _) as moduleName) name) ->
                        let
                            fullName : String
                            fullName =
                                Syntax.qualifiedNameToString { moduleName = moduleName, name = name }
                        in
                        case Dict.get moduleName kernelFunctions of
                            Nothing ->
                                Eval.fail <| nameError localEnv fullName

                            Just kernelModule ->
                                case Dict.get name kernelModule of
                                    Nothing ->
                                        Eval.fail <| nameError localEnv fullName

                                    Just ( _, f ) ->
                                        f []
                                            cfg
                                            (Environment.call moduleName name localEnv)

                    _ ->
                        Expr
                            implementation
                            (case ( functionName, cfg.trace ) of
                                ( Just { moduleName, name }, True ) ->
                                    { cfg
                                        | callTreeContinuation =
                                            CTCCall
                                                { moduleName = moduleName
                                                , name = name
                                                }
                                                oldArgs
                                                cfg.callTreeContinuation
                                    }

                                _ ->
                                    cfg
                            )
                            (localEnv |> Environment.with newEnvValues)


evalKernelFunction : ModuleName -> String -> Eval Expr
evalKernelFunction moduleName name cfg env =
    case Dict.get moduleName kernelFunctions of
        Nothing ->
            Eval.fail <| nameError env (String.join "." moduleName)

        Just kernelModule ->
            case Dict.get name kernelModule of
                Nothing ->
                    Eval.fail <| nameError env <| Syntax.qualifiedNameToString { moduleName = moduleName, name = name }

                Just ( argCount, f ) ->
                    if argCount == 0 then
                        let
                            ( result, callTrees, logLines ) =
                                f [] cfg (Environment.call moduleName name env)
                        in
                        if cfg.trace then
                            let
                                callTree : CallTree
                                callTree =
                                    CallNode
                                        { moduleName = moduleName
                                        , name = name
                                        }
                                        { args = []
                                        , result = result
                                        , children = callTrees
                                        }
                            in
                            PartialValue ( result, Rope.singleton callTree, logLines )

                        else
                            PartialValue <| Types.fromResult result

                    else
                        PartiallyApplied (Environment.empty moduleName)
                            []
                            (List.repeat argCount (fakeNode AllPattern))
                            (Just { moduleName = moduleName, name = name })
                            (fakeNode <| Expr.FunctionOrValue moduleName name)
                            |> Eval.succeed


evalNegation : Expr -> Eval Expr
evalNegation child cfg env =
    Expr child cfg env
        |> Eval.andThen
            (\value ->
                case value of
                    Expr.Int i ->
                        Eval.succeed <| Expr.Int -i

                    Expr.Float f ->
                        Eval.succeed <| Expr.Float -f

                    _ ->
                        Eval.fail <| typeError env "Trying to negate a non-number"
            )
        |> PartialValue


evalLetBlock : LetBlock -> Eval Expr
evalLetBlock letBlock cfg env =
    let
        envDefs : Set String
        envDefs =
            Set.diff
                (Set.union
                    (Dict.get env.currentModule env.functions
                        |> Maybe.map (Dict.keys >> Set.fromList)
                        |> Maybe.withDefault Set.empty
                    )
                    (Dict.keys env.values |> Set.fromList)
                )
                allDefVars

        allDefVars : Set String
        allDefVars =
            letBlock.declarations
                |> List.foldl (\e -> Set.union (declarationDefinedVariables e)) Set.empty

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

        newEnv : EvalResult Env
        newEnv =
            case sortedDeclarations of
                Err TopologicalSort.IllegalCycle ->
                    Eval.fail <| typeError env "illegal cycle in let block"

                Err TopologicalSort.InternalError ->
                    Eval.fail <| typeError env "internal error in let block"

                Ok sd ->
                    -- We can't use combineMap and need to fold
                    -- because we need to change the environment for each call
                    List.foldl
                        (\declaration acc ->
                            Eval.andThen
                                (\e -> addLetDeclaration declaration cfg e)
                                acc
                        )
                        (Eval.succeed env)
                        sd
    in
    newEnv
        |> Eval.andThen
            (\ne ->
                Expr letBlock.expression
                    cfg
                    ne
            )


isLetDeclarationFunction : Node LetDeclaration -> Bool
isLetDeclarationFunction (Node _ d) =
    case d of
        LetFunction { declaration } ->
            List.length (Node.value declaration).arguments > 0

        _ ->
            False


addLetDeclaration : Node LetDeclaration -> Eval Env
addLetDeclaration ((Node _ letDeclaration) as node) cfg env =
    case letDeclaration of
        LetFunction { declaration } ->
            case declaration of
                Node _ ({ name, expression } as implementation) ->
                    if isLetDeclarationFunction node then
                        Eval.succeed <| Environment.addFunction env.currentModule implementation env

                    else
                        Expr expression cfg env
                            |> Types.map (\value -> Environment.withValue (Node.value name) value env)

        LetDestructuring letPattern _ ->
            Expr Expr cfg env
                |> Types.onValue
                    (\letValue ->
                        case matchInfallible env letPattern letValue of
                            Err e ->
                                Err e

                            Ok patternEnv ->
                                Ok (Environment.with patternEnv env)
                    )


declarationFreeVariables : Node LetDeclaration -> Set String
declarationFreeVariables (Node _ letDeclaration) =
    case letDeclaration of
        LetFunction { declaration } ->
            let
                { name, arguments, expression } =
                    Node.value declaration
            in
            Set.diff (freeVariables expression)
                (List.foldl (\p -> Set.union (patternDefinedVariables p))
                    (Set.singleton (Node.value name))
                    arguments
                )

        LetDestructuring pattern expression ->
            Set.diff (freeVariables expression) (patternDefinedVariables pattern)


letFreeVariables : LetBlock -> Set String
letFreeVariables { declarations, expression } =
    Set.diff
        (List.foldl (\d -> Set.union (declarationFreeVariables d)) (freeVariables expression) declarations)
        (List.foldl (\d -> Set.union (declarationDefinedVariables d)) Set.empty declarations)


caseFreeVariables : Case -> Set String
caseFreeVariables ( pattern, expression ) =
    Set.diff (freeVariables expression) (patternDefinedVariables pattern)


freeVariables : Expr -> Set String
freeVariables expr =
    case expr of
        Apply l r ->
            Set.union (freeVariables l) (freeVariables r)

        BinOp l _ r ->
            Set.union (freeVariables l) (freeVariables r)

        Custom _ args ->
            List.foldl (\arg -> Set.union (freeVariables arg)) Set.empty args

        IfThenElse cond true false ->
            Set.union (freeVariables cond)
                (Set.union (freeVariables true) (freeVariables false))

        Negate child ->
            freeVariables child

        Tuple expressions ->
            List.foldl (\e -> Set.union (freeVariables e)) Set.empty expressions

        LetExpression block ->
            letFreeVariables block

        CaseExpression { expression, cases } ->
            List.foldl (\c -> Set.union (caseFreeVariables c)) (freeVariables expression) cases

        LambdaExpression { expression, args } ->
            Set.diff (freeVariables expression)
                (List.foldl (\p -> Set.union (patternDefinedVariables p)) Set.empty args)

        RecordExpr setters ->
            List.foldl (\(Node _ ( _, e )) -> Set.union (freeVariables e)) Set.empty setters

        ListExpr expressions ->
            List.foldl (\e -> Set.union (freeVariables e)) Set.empty expressions

        RecordAccess record _ ->
            freeVariables record

        RecordUpdateExpression (Node _ s) setters ->
            List.foldl (\(Node _ ( _, e )) -> Set.union (freeVariables e)) (Set.singleton s) setters


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
        LetFunction { declaration } ->
            Set.singleton <| Node.value (Node.value declaration).name

        LetDestructuring letPattern _ ->
            patternDefinedVariables letPattern


evalRecordAccess : Expr -> Node String -> Eval Expr
evalRecordAccess recordExpr (Node _ field) cfg env =
    Expr recordExpr cfg env
        |> Eval.andThen
            (\value ->
                case value of
                    Expr.Record fields ->
                        case Dict.get field fields of
                            Just fieldValue ->
                                Eval.succeed fieldValue

                            Nothing ->
                                Eval.fail <| typeError env <| "Field " ++ field ++ " not found [record access]"

                    _ ->
                        Eval.fail <| typeError env "Trying to access a field on a non-record value"
            )
        |> PartialValue


evalRecordAccessFunction : String -> Expr
evalRecordAccessFunction field =
    PartiallyApplied
        (Environment.empty [])
        []
        [ fakeNode (VarPattern "$r") ]
        Nothing
        (fakeNode <|
            Expr.RecordAccess
                (fakeNode <| Expr.FunctionOrValue [] "$r")
                (fakeNode <| String.dropLeft 1 field)
        )


evalRecordUpdate : Node String -> List (Node RecordSetter) -> Eval Expr
evalRecordUpdate (Node _ name) setters cfg env =
    Expr (fakeNode <| Expr.FunctionOrValue [] name) cfg env
        |> Eval.andThen
            (\value ->
                case value of
                    Expr.Record _ ->
                        let
                            ( fieldNames, _ ) =
                                setters
                                    |> List.map
                                        (\(Node _ ( Node _ fieldName, fieldExpression )) ->
                                            ( fieldName
                                            , Expr
                                            )
                                        )
                                    |> List.unzip
                        in
                        Types.combineMap Expr Exprs cfg env
                            |> Types.map
                                (\fieldValues ->
                                    List.map2 Tuple.pair fieldNames fieldValues
                                        |> Dict.fromList
                                        |> Expr.Record
                                )

                    _ ->
                        Eval.fail <| typeError env "Trying to update fields on a value which is not a record"
            )
        |> PartialValue


evalOperator : String -> Eval Expr
evalOperator opName _ env =
    case Dict.get opName Core.operators of
        Nothing ->
            Eval.fail <| nameError env opName

        Just kernelFunction ->
            PartiallyApplied
                (Environment.call kernelFunction.moduleName opName env)
                []
                [ fakeNode <| VarPattern "$l", fakeNode <| VarPattern "$r" ]
                Nothing
                (fakeNode <|
                    Expr.Application
                        [ fakeNode <| Expr.FunctionOrValue kernelFunction.moduleName kernelFunction.name
                        , fakeNode <| Expr.FunctionOrValue [] "$l"
                        , fakeNode <| Expr.FunctionOrValue [] "$r"
                        ]
                )
                |> Eval.succeed


isVariant : String -> Bool
isVariant name =
    case String.uncons name of
        Nothing ->
            False

        Just ( first, _ ) ->
            Unicode.isUpper first


evalCase : CaseBlock -> Eval Expr
evalCase { expression, cases } cfg env =
    Expr expression cfg env
        |> Eval.andThen
            (\exprValue ->
                let
                    maybePartial : Result EvalError (Maybe (Eval Expr))
                    maybePartial =
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
                                                    Expr result2
                                                        cfg
                                                        (Environment.with additionalEnv env)
                                                        |> Just
                                                        |> Ok
                                )
                                (Ok Nothing)
                in
                case maybePartial of
                    Ok Nothing ->
                        Eval.fail <| typeError env <| "Missing case branch for " ++ Expr.toString exprValue

                    Ok (Just res) ->
                        res

                    Err e ->
                        Eval.fail e
            )
