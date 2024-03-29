module Eval.Expression exposing (evalExpression, evalFunction)

import Core
import Core.Basics
import Elm.Syntax.Expression as Expression exposing (Expression(..), LetDeclaration)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..), QualifiedNameRef)
import Elm.Syntax.Range as Range
import Environment
import Eval.Types as Types
import EvalResult
import FastDict as Dict exposing (Dict)
import Kernel
import List.Extra
import Recursion
import Result.MyExtra
import Rope
import Set exposing (Set)
import Syntax exposing (fakeNode)
import TopologicalSort
import Types exposing (CallTree(..), Env, EnvValues, Eval, EvalErrorData, EvalResult, PartialEval, PartialResult, Value(..))
import Unicode
import Value exposing (nameError, typeError, unsupported)


evalExpression : Node Expression -> Eval Value
evalExpression initExpression initCfg initEnv =
    Recursion.runRecursion
        (\( Node range expression, cfg, env ) ->
            let
                result : PartialResult Value
                result =
                    case expression of
                        Expression.UnitExpr ->
                            Types.succeedPartial Unit

                        Expression.Integer i ->
                            Types.succeedPartial <| Int i

                        Expression.Hex i ->
                            Types.succeedPartial <| Int i

                        Expression.Floatable f ->
                            Types.succeedPartial <| Float f

                        Expression.Literal string ->
                            Types.succeedPartial <| String string

                        Expression.CharLiteral c ->
                            Types.succeedPartial <| Char c

                        Expression.OperatorApplication "||" _ l r ->
                            evalShortCircuitOr l r cfg env

                        Expression.OperatorApplication "&&" _ l r ->
                            evalShortCircuitAnd l r cfg env

                        Expression.OperatorApplication opName _ l r ->
                            let
                                first : Node Expression
                                first =
                                    fakeNode <| Expression.Operator opName

                                rest : List (Node Expression)
                                rest =
                                    [ l, r ]
                            in
                            evalApplication first rest cfg env

                        Expression.Application [] ->
                            Types.failPartial <| typeError env "Empty application"

                        Expression.Application (first :: rest) ->
                            evalApplication first rest cfg env

                        Expression.FunctionOrValue moduleName name ->
                            evalFunctionOrValue moduleName name cfg env

                        Expression.IfBlock cond true false ->
                            evalIfBlock cond true false cfg env

                        Expression.PrefixOperator opName ->
                            evalOperator opName cfg env

                        Expression.Operator opName ->
                            evalOperator opName cfg env

                        Expression.Negation child ->
                            evalNegation child cfg env

                        Expression.TupledExpression exprs ->
                            evalTuple exprs cfg env

                        Expression.ParenthesizedExpression child ->
                            Recursion.recurse ( child, cfg, env )

                        Expression.LetExpression letBlock ->
                            evalLetBlock letBlock cfg env

                        Expression.CaseExpression caseExpr ->
                            evalCase caseExpr cfg env

                        Expression.LambdaExpression lambda ->
                            Types.succeedPartial <| PartiallyApplied env [] lambda.args Nothing lambda.expression

                        Expression.RecordExpr fields ->
                            evalRecord fields cfg env

                        Expression.ListExpr elements ->
                            evalList elements cfg env

                        Expression.RecordAccess recordExpr field ->
                            evalRecordAccess recordExpr field cfg env

                        Expression.RecordAccessFunction field ->
                            Types.succeedPartial <| evalRecordAccessFunction field

                        Expression.RecordUpdateExpression name setters ->
                            evalRecordUpdate name setters cfg env

                        Expression.GLSLExpression _ ->
                            Types.failPartial <| unsupported env "GLSL not supported"
            in
            result
                |> Recursion.map
                    (\( value, trees, logs ) ->
                        ( value
                        , CallNode
                            { env = env
                            , expression = Node range expression
                            , children = trees
                            , result = value
                            }
                            |> Rope.singleton
                        , logs
                        )
                    )
        )
        ( initExpression, initCfg, initEnv )


evalShortCircuitAnd : Node Expression -> Node Expression -> PartialEval Value
evalShortCircuitAnd l r cfg env =
    Types.recurseThen ( l, cfg, env )
        (\value ->
            case value of
                Bool False ->
                    Types.succeedPartial <| Bool False

                Bool True ->
                    Recursion.recurse ( r, cfg, env )

                v ->
                    Types.failPartial <| typeError env <| "&& applied to non-Bool " ++ Value.toString v
        )


evalShortCircuitOr : Node Expression -> Node Expression -> PartialEval Value
evalShortCircuitOr l r cfg env =
    Types.recurseThen ( l, cfg, env )
        (\value ->
            case value of
                Bool True ->
                    Types.succeedPartial <| Bool True

                Bool False ->
                    Recursion.recurse ( r, cfg, env )

                v ->
                    Types.failPartial <| typeError env <| "|| applied to non-Bool " ++ Value.toString v
        )


evalTuple : List (Node Expression) -> PartialEval Value
evalTuple exprs cfg env =
    case exprs of
        [] ->
            Types.succeedPartial Unit

        [ c ] ->
            Recursion.recurse ( c, cfg, env )

        [ l, r ] ->
            Types.recurseThen ( l, cfg, env )
                (\lValue ->
                    Types.recurseThen ( r, cfg, env )
                        (\rValue ->
                            Types.succeedPartial (Tuple lValue rValue)
                        )
                )

        [ l, m, r ] ->
            Types.recurseThen ( l, cfg, env )
                (\lValue ->
                    Types.recurseThen ( m, cfg, env )
                        (\mValue ->
                            Types.recurseThen ( r, cfg, env )
                                (\rValue ->
                                    Types.succeedPartial (Triple lValue mValue rValue)
                                )
                        )
                )

        _ :: _ :: _ :: _ :: _ ->
            Types.failPartial <| typeError env "Tuples with more than three elements are not supported"


evalApplication : Node Expression -> List (Node Expression) -> PartialEval Value
evalApplication first rest cfg env =
    let
        inner : Env -> List Value -> List (Node Pattern) -> Maybe QualifiedNameRef -> Node Expression -> PartialResult Value
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
                Recursion.recurse
                    ( fakeNode <|
                        Expression.Application
                            (fakeNode
                                (Expression.Application (first :: used))
                                :: leftover
                            )
                    , cfg
                    , env
                    )

            else
                Types.recurseMapThen ( rest, cfg, env )
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
                            Types.succeedPartial <| PartiallyApplied localEnv args patterns maybeQualifiedName implementation

                        else
                            -- Just right, we special case this for TCO
                            evalFullyApplied localEnv args patterns maybeQualifiedName implementation cfg env
                    )
    in
    Types.recurseThen ( first, cfg, env )
        (\firstValue ->
            case firstValue of
                Custom name customArgs ->
                    Types.recurseMapThen ( rest, cfg, env )
                        (\values -> Types.succeedPartial <| Custom name (customArgs ++ values))

                PartiallyApplied localEnv oldArgs patterns maybeQualifiedName implementation ->
                    inner localEnv oldArgs patterns maybeQualifiedName implementation

                other ->
                    Types.failPartial <|
                        typeError env <|
                            "Trying to apply "
                                ++ Value.toString other
                                ++ ", which is a non-lambda non-variant"
        )


evalFullyApplied : Env -> List Value -> List (Node Pattern) -> Maybe QualifiedNameRef -> Node Expression -> PartialEval Value
evalFullyApplied localEnv args patterns maybeQualifiedName implementation cfg env =
    let
        maybeNewEnvValues : Result EvalErrorData (Maybe EnvValues)
        maybeNewEnvValues =
            match env
                (fakeNode <| ListPattern patterns)
                (List args)
    in
    case maybeNewEnvValues of
        Err e ->
            Types.failPartial e

        Ok Nothing ->
            Types.failPartial <| typeError env "Could not match lambda patterns"

        Ok (Just newEnvValues) ->
            case implementation of
                Node range (FunctionOrValue (("Elm" :: "Kernel" :: _) as moduleName) name) ->
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
                            Types.failPartial <| nameError env fullName

                        Just kernelModule ->
                            case Dict.get name kernelModule of
                                Nothing ->
                                    Types.failPartial <| nameError env fullName

                                Just ( _, f ) ->
                                    let
                                        childEnv : Env
                                        childEnv =
                                            Environment.call moduleName name env

                                        ( kernelResult, children, logLines ) =
                                            f args
                                                cfg
                                                childEnv
                                    in
                                    ( kernelResult
                                    , if cfg.trace then
                                        CallNode
                                            { env = childEnv
                                            , expression =
                                                Node range <|
                                                    Application <|
                                                        Node range (FunctionOrValue moduleName name)
                                                            :: List.map Value.toExpression args
                                            , result = kernelResult
                                            , children = children
                                            }
                                            |> Rope.singleton

                                      else
                                        Rope.empty
                                    , logLines
                                    )
                                        |> Recursion.base

                _ ->
                    call
                        maybeQualifiedName
                        implementation
                        cfg
                        (localEnv |> Environment.with newEnvValues)


call : Maybe QualifiedNameRef -> Node Expression -> PartialEval Value
call maybeQualifiedName implementation cfg env =
    case maybeQualifiedName of
        Just qualifiedName ->
            Recursion.recurse
                ( implementation
                , cfg
                , Environment.call qualifiedName.moduleName qualifiedName.name env
                )

        Nothing ->
            Recursion.recurse ( implementation, cfg, env )


evalFunctionOrValue : ModuleName -> String -> PartialEval Value
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


evalVariant : ModuleName -> Env -> String -> PartialResult Value
evalVariant moduleName env name =
    let
        variant0 : ModuleName -> String -> PartialResult Value
        variant0 modName ctorName =
            Types.succeedPartial <| Custom { moduleName = modName, name = ctorName } []

        variant1 : ModuleName -> String -> PartialResult Value
        variant1 modName ctorName =
            Types.succeedPartial <|
                PartiallyApplied
                    (Environment.empty modName)
                    []
                    [ fakeNode <| VarPattern "$x" ]
                    Nothing
                    (fakeNode <|
                        Expression.Application
                            [ fakeNode <| FunctionOrValue modName ctorName
                            , fakeNode <| FunctionOrValue [] "$x"
                            ]
                    )
    in
    case ( moduleName, name ) of
        ( [], "True" ) ->
            Types.succeedPartial <| Bool True

        ( [], "False" ) ->
            Types.succeedPartial <| Bool False

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
            Types.succeedPartial <| Custom qualifiedNameRef []


evalNonVariant : ModuleName -> String -> PartialEval Value
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
                                |> Types.succeedPartial

        _ ->
            case ( moduleName, Dict.get name env.values ) of
                ( [], Just (PartiallyApplied localEnv [] [] maybeName implementation) ) ->
                    call maybeName implementation cfg localEnv

                ( [], Just value ) ->
                    Types.succeedPartial value

                _ ->
                    let
                        fixedModuleName : ModuleName
                        fixedModuleName =
                            fixModuleName moduleName env

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
                            let
                                qualifiedNameRef : QualifiedNameRef
                                qualifiedNameRef =
                                    { moduleName = fixedModuleName, name = name }
                            in
                            if List.isEmpty function.arguments then
                                call (Just qualifiedNameRef) function.expression cfg env

                            else
                                PartiallyApplied
                                    (Environment.call fixedModuleName name env)
                                    []
                                    function.arguments
                                    (Just qualifiedNameRef)
                                    function.expression
                                    |> Types.succeedPartial

                        Nothing ->
                            Syntax.qualifiedNameToString
                                { moduleName = fixedModuleName
                                , name = name
                                }
                                |> nameError env
                                |> Types.failPartial


evalIfBlock : Node Expression -> Node Expression -> Node Expression -> PartialEval Value
evalIfBlock cond true false cfg env =
    Types.recurseThen ( cond, cfg, env )
        (\condValue ->
            case condValue of
                Bool True ->
                    Recursion.recurse ( true, cfg, env )

                Bool False ->
                    Recursion.recurse ( false, cfg, env )

                _ ->
                    Types.failPartial <| typeError env "ifThenElse condition was not a boolean"
        )


evalList : List (Node Expression) -> PartialEval Value
evalList elements cfg env =
    Types.recurseMapThen ( elements, cfg, env )
        (\values -> Types.succeedPartial <| List values)


evalRecord : List (Node Expression.RecordSetter) -> PartialEval Value
evalRecord fields cfg env =
    let
        ( fieldNames, expressions ) =
            fields
                |> List.map (\(Node _ ( Node _ name, expression )) -> ( name, expression ))
                |> List.unzip
    in
    Types.recurseMapThen ( expressions, cfg, env )
        (\tuples ->
            tuples
                |> List.map2 Tuple.pair fieldNames
                |> Dict.fromList
                |> Record
                |> Types.succeedPartial
        )


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
        EvalResult.succeed <| PartiallyApplied localEnv oldArgs patterns functionName implementation

    else
        -- Just right, we special case this for TCO
        let
            maybeNewEnvValues : Result EvalErrorData (Maybe EnvValues)
            maybeNewEnvValues =
                match localEnv
                    (Node (Range.combine (List.map Node.range patterns)) <| ListPattern patterns)
                    (List oldArgs)
        in
        case maybeNewEnvValues of
            Err e ->
                EvalResult.fail e

            Ok Nothing ->
                EvalResult.fail <| typeError localEnv "Could not match lambda patterns"

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
                                EvalResult.fail <| nameError localEnv fullName

                            Just kernelModule ->
                                case Dict.get name kernelModule of
                                    Nothing ->
                                        EvalResult.fail <| nameError localEnv fullName

                                    Just ( _, f ) ->
                                        f []
                                            cfg
                                            (Environment.call moduleName name localEnv)

                    _ ->
                        -- This is fine because it's never going to be recursive. FOR NOW. TODO: fix
                        evalExpression implementation
                            cfg
                            (localEnv |> Environment.with newEnvValues)


evalKernelFunction : ModuleName -> String -> PartialEval Value
evalKernelFunction moduleName name cfg env =
    case Dict.get moduleName kernelFunctions of
        Nothing ->
            Types.failPartial <| nameError env (String.join "." moduleName)

        Just kernelModule ->
            case Dict.get name kernelModule of
                Nothing ->
                    Types.failPartial <| nameError env <| Syntax.qualifiedNameToString { moduleName = moduleName, name = name }

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
                                        { env = env
                                        , expression = fakeNode <| FunctionOrValue moduleName name
                                        , result = result
                                        , children = callTrees
                                        }
                            in
                            Recursion.base ( result, Rope.singleton callTree, logLines )

                        else
                            Recursion.base <| EvalResult.fromResult result

                    else
                        PartiallyApplied (Environment.empty moduleName)
                            []
                            (List.repeat argCount (fakeNode AllPattern))
                            (Just { moduleName = moduleName, name = name })
                            (fakeNode <| Expression.FunctionOrValue moduleName name)
                            |> Types.succeedPartial


evalNegation : Node Expression -> PartialEval Value
evalNegation child cfg env =
    Types.recurseThen ( child, cfg, env )
        (\value ->
            case value of
                Int i ->
                    Types.succeedPartial <| Int -i

                Float f ->
                    Types.succeedPartial <| Float -f

                _ ->
                    Types.failPartial <| typeError env "Trying to negate a non-number"
        )


evalLetBlock : Expression.LetBlock -> PartialEval Value
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
                    EvalResult.fail <| typeError env "illegal cycle in let block"

                Err TopologicalSort.InternalError ->
                    EvalResult.fail <| typeError env "internal error in let block"

                Ok sd ->
                    -- We can't use combineMap and need to fold
                    -- because we need to change the environment for each call
                    List.foldl
                        (\declaration acc ->
                            EvalResult.andThen
                                (\e -> addLetDeclaration declaration cfg e)
                                acc
                        )
                        (EvalResult.succeed env)
                        sd
    in
    case newEnv of
        ( Ok ne, trees, logs ) ->
            Types.recurseThen ( letBlock.expression, cfg, ne )
                (\res -> Recursion.base ( Ok res, trees, logs ))

        ( Err e, trees, logs ) ->
            Recursion.base ( Err e, trees, logs )


isLetDeclarationFunction : Node LetDeclaration -> Bool
isLetDeclarationFunction (Node _ d) =
    case d of
        Expression.LetFunction { declaration } ->
            List.length (Node.value declaration).arguments > 0

        _ ->
            False


addLetDeclaration : Node LetDeclaration -> Eval Env
addLetDeclaration ((Node _ letDeclaration) as node) cfg env =
    case letDeclaration of
        Expression.LetFunction { declaration } ->
            case declaration of
                Node _ ({ name, expression } as implementation) ->
                    if isLetDeclarationFunction node then
                        EvalResult.succeed <| Environment.addFunction env.currentModule implementation env

                    else
                        evalExpression expression cfg env
                            |> EvalResult.map (\value -> Environment.addValue (Node.value name) value env)

        Expression.LetDestructuring letPattern letExpression ->
            evalExpression letExpression cfg env
                |> EvalResult.onValue
                    (\letValue ->
                        case match env letPattern letValue of
                            Err e ->
                                Err e

                            Ok Nothing ->
                                Err <| typeError env "Could not match pattern inside let"

                            Ok (Just patternEnv) ->
                                Ok (Environment.with patternEnv env)
                    )


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


evalRecordAccess : Node Expression -> Node String -> PartialEval Value
evalRecordAccess recordExpr (Node _ field) cfg env =
    Types.recurseThen ( recordExpr, cfg, env )
        (\value ->
            case value of
                Record fields ->
                    case Dict.get field fields of
                        Just fieldValue ->
                            Types.succeedPartial fieldValue

                        Nothing ->
                            Types.failPartial <| typeError env <| "Field " ++ field ++ " not found [record access]"

                _ ->
                    Types.failPartial <| typeError env "Trying to access a field on a non-record value"
        )


evalRecordAccessFunction : String -> Value
evalRecordAccessFunction field =
    PartiallyApplied
        (Environment.empty [])
        []
        [ fakeNode (VarPattern "$r") ]
        Nothing
        (fakeNode <|
            Expression.RecordAccess
                (fakeNode <| Expression.FunctionOrValue [] "$r")
                (fakeNode <| String.dropLeft 1 field)
        )


evalRecordUpdate : Node String -> List (Node Expression.RecordSetter) -> PartialEval Value
evalRecordUpdate (Node range name) setters cfg env =
    Types.recurseThen ( Node range <| Expression.FunctionOrValue [] name, cfg, env )
        (\value ->
            case value of
                Record _ ->
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
                    Types.recurseMapThen ( fieldExpressions, cfg, env )
                        (\fieldValues ->
                            List.map2 Tuple.pair fieldNames fieldValues
                                |> Dict.fromList
                                |> Record
                                |> Types.succeedPartial
                        )

                _ ->
                    Types.failPartial <| typeError env "Trying to update fields on a value which is not a record"
        )


evalOperator : String -> PartialEval Value
evalOperator opName _ env =
    case Dict.get opName Core.operators of
        Nothing ->
            Types.failPartial <| nameError env opName

        Just kernelFunction ->
            PartiallyApplied
                (Environment.call kernelFunction.moduleName opName env)
                []
                [ fakeNode <| VarPattern "$l", fakeNode <| VarPattern "$r" ]
                Nothing
                (fakeNode <|
                    Expression.Application
                        [ fakeNode <| Expression.FunctionOrValue kernelFunction.moduleName kernelFunction.name
                        , fakeNode <| Expression.FunctionOrValue [] "$l"
                        , fakeNode <| Expression.FunctionOrValue [] "$r"
                        ]
                )
                |> Types.succeedPartial


isVariant : String -> Bool
isVariant name =
    case String.uncons name of
        Nothing ->
            False

        Just ( first, _ ) ->
            Unicode.isUpper first


evalCase : Expression.CaseBlock -> PartialEval Value
evalCase { expression, cases } cfg env =
    Types.recurseThen ( expression, cfg, env )
        (\exprValue ->
            let
                maybePartial : Result EvalErrorData (Maybe ( EnvValues, Node Expression ))
                maybePartial =
                    Result.MyExtra.combineFoldl
                        (\( pattern, branchExpression ) acc ->
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
                                            Ok <| Just ( additionalEnv, branchExpression )
                        )
                        (Ok Nothing)
                        cases
            in
            case maybePartial of
                Ok Nothing ->
                    Types.failPartial <| typeError env <| "Missing case branch for " ++ Value.toString exprValue

                Ok (Just ( additionalEnv, branchExpression )) ->
                    Recursion.recurse
                        ( branchExpression
                        , cfg
                        , Environment.with additionalEnv env
                        )

                Err e ->
                    Types.failPartial e
        )


match : Env -> Node Pattern -> Value -> Result EvalErrorData (Maybe EnvValues)
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
        ( UnitPattern, Unit ) ->
            ok Dict.empty

        ( UnitPattern, _ ) ->
            noMatch

        ( AllPattern, _ ) ->
            ok Dict.empty

        ( ParenthesizedPattern subPattern, _ ) ->
            match env subPattern value

        ( NamedPattern namePattern argsPatterns, Custom variant args ) ->
            -- Two names from different modules can never have the same type
            -- so if we assume the code typechecks we can skip the module name check
            if namePattern.name == variant.name then
                let
                    matchNamedPatternHelper :
                        EnvValues
                        -> ( List (Node Pattern), List Value )
                        -> Result EvalErrorData (Maybe EnvValues)
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

        ( UnConsPattern patternHead patternTail, List (listHead :: listTail) ) ->
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

        ( CharPattern c, Char d ) ->
            if c == d then
                ok Dict.empty

            else
                noMatch

        ( CharPattern _, _ ) ->
            noMatch

        ( StringPattern c, String d ) ->
            if c == d then
                ok Dict.empty

            else
                noMatch

        ( StringPattern _, _ ) ->
            noMatch

        ( IntPattern c, Int d ) ->
            if c == d then
                ok Dict.empty

            else
                noMatch

        ( IntPattern _, _ ) ->
            noMatch

        ( HexPattern c, Int d ) ->
            if c == d then
                ok Dict.empty

            else
                noMatch

        ( HexPattern _, _ ) ->
            noMatch

        ( FloatPattern c, Float d ) ->
            if c == d then
                ok Dict.empty

            else
                noMatch

        ( FloatPattern _, _ ) ->
            noMatch

        ( TuplePattern [ lpattern, rpattern ], Tuple lvalue rvalue ) ->
            match env lpattern lvalue
                |> andThen
                    (\lenv ->
                        match env rpattern rvalue
                            |> andThen
                                (\renv ->
                                    ok <| Dict.union renv lenv
                                )
                    )

        ( TuplePattern [ lpattern, mpattern, rpattern ], Triple lvalue mvalue rvalue ) ->
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

        ( RecordPattern fields, Record fieldValues ) ->
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
