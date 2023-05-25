module Eval exposing (Error(..), eval, evalFunction, evalModule)

import Core
import Core.Basics
import Elm.Parser
import Elm.Processing
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Module exposing (Module(..))
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..), QualifiedNameRef)
import Env
import Eval.PartialResult as PartialResult exposing (PartialResult(..))
import FastDict as Dict exposing (Dict)
import Kernel
import List.Extra
import Parser exposing (DeadEnd)
import Result.Extra
import Result.MyExtra
import Set exposing (Set)
import Syntax exposing (fakeNode)
import TopologicalSort
import Unicode
import Value exposing (Env, EnvValues, EvalErrorKind, EvalResult, Value(..), nameError, typeError, unsupported)


type Error
    = ParsingError (List DeadEnd)
    | EvalError
        { currentModule : ModuleName
        , callStack : List QualifiedNameRef
        , error : EvalErrorKind
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
        |> Result.andThen buildInitialEnv
        |> Result.andThen
            (\env ->
                Result.mapError EvalError <|
                    evalExpression env (fakeNode expression)
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
                    Ok (Env.addFunction moduleName implementation env)

                PortDeclaration _ ->
                    Err <| EvalError <| unsupported env "Port declaration"

                InfixDeclaration _ ->
                    Err <| EvalError <| unsupported env "Infix declaration"

                Destructuring _ _ ->
                    Err <| EvalError <| unsupported env "Top level destructuring"

                AliasDeclaration _ ->
                    Ok env

                CustomTypeDeclaration _ ->
                    Ok env
    in
    file.declarations
        |> Result.MyExtra.combineFoldl
            addDeclaration
            (Ok coreEnv)


evalExpression : Env -> Node Expression -> EvalResult Value
evalExpression env (Node _ expression) =
    case expression of
        Expression.UnitExpr ->
            Ok Value.Unit

        Expression.OperatorApplication "||" _ l r ->
            case evalExpression env l of
                Ok (Bool True) ->
                    Ok (Bool True)

                Ok (Bool False) ->
                    evalExpression env r

                Err e ->
                    Err e

                Ok v ->
                    Err <| typeError env <| "|| applied to non-Bool " ++ Value.toString v

        Expression.OperatorApplication "&&" _ l r ->
            case evalExpression env l of
                Ok (Bool False) ->
                    Ok (Bool False)

                Ok (Bool True) ->
                    evalExpression env r

                Err e ->
                    Err e

                Ok v ->
                    Err <| typeError env <| "&& applied to non-Bool " ++ Value.toString v

        Expression.OperatorApplication opName _ l r ->
            evalExpression env
                (fakeNode <|
                    Expression.Application
                        [ fakeNode <| Expression.Operator opName
                        , l
                        , r
                        ]
                )

        Expression.Application [] ->
            Err <| typeError env "Empty application"

        Expression.Application (first :: rest) ->
            case evalApplication env first rest of
                PartialValue v ->
                    Ok v

                PartialExpression newEnv expr ->
                    evalExpression newEnv expr

                PartialErr e ->
                    Err e

        Expression.FunctionOrValue moduleName name ->
            case evalFunctionOrValue env moduleName name of
                PartialValue v ->
                    Ok v

                PartialExpression newEnv expr ->
                    evalExpression newEnv expr

                PartialErr e ->
                    Err e

        Expression.IfBlock cond true false ->
            case evalIfBlock env cond true false of
                PartialErr e ->
                    Err e

                PartialValue v ->
                    Ok v

                PartialExpression newEnv next ->
                    evalExpression newEnv next

        Expression.PrefixOperator opName ->
            evalOperator env opName

        Expression.Operator opName ->
            evalOperator env opName

        Expression.Integer i ->
            Ok (Value.Int i)

        Expression.Hex i ->
            Ok (Value.Int i)

        Expression.Floatable f ->
            Ok (Value.Float f)

        Expression.Negation child ->
            evalNegation env child

        Expression.Literal string ->
            Ok (Value.String string)

        Expression.CharLiteral c ->
            Ok (Value.Char c)

        Expression.TupledExpression exprs ->
            case exprs of
                [] ->
                    Ok Value.Unit

                [ c ] ->
                    evalExpression env c

                [ l, r ] ->
                    evalExpression2
                        env
                        l
                        r
                        (\lvalue rvalue -> Ok (Value.Tuple lvalue rvalue))

                [ l, m, r ] ->
                    Result.map3 Value.Triple
                        (evalExpression env l)
                        (evalExpression env m)
                        (evalExpression env r)

                _ :: _ :: _ :: _ :: _ ->
                    Err <| typeError env "Tuples with more than three elements are not supported"

        Expression.ParenthesizedExpression child ->
            evalExpression env child

        Expression.LetExpression letBlock ->
            case evalLetBlock env letBlock of
                Err e ->
                    Err e

                Ok newEnv ->
                    evalExpression newEnv letBlock.expression

        Expression.CaseExpression caseExpr ->
            case evalCase env caseExpr of
                Err e ->
                    Err e

                Ok ( newEnv, next ) ->
                    evalExpression newEnv next

        Expression.LambdaExpression lambda ->
            Ok <| PartiallyApplied env [] lambda.args lambda.expression

        Expression.RecordExpr fields ->
            evalRecord env fields

        Expression.ListExpr elements ->
            evalList env elements

        Expression.RecordAccess recordExpr field ->
            evalRecordAccess env recordExpr field

        Expression.RecordAccessFunction field ->
            Ok <| evalRecordAccessFunction field

        Expression.RecordUpdateExpression name setters ->
            evalRecordUpdate env name setters

        Expression.GLSLExpression _ ->
            Err <| unsupported env "GLSL not supported"


evalApplication : Env -> Node Expression -> List (Node Expression) -> PartialResult
evalApplication env first rest =
    case evalExpression env first of
        Err e ->
            PartialErr e

        Ok (Value.Custom name customArgs) ->
            case Result.Extra.combineMap (\arg -> evalExpression env arg) rest of
                Ok values ->
                    PartialValue <| Value.Custom name (customArgs ++ values)

                Err e ->
                    PartialErr e

        Ok (Value.PartiallyApplied localEnv oldArgs patterns implementation) ->
            let
                ( used, leftover ) =
                    List.Extra.splitAt (patternsLength - oldArgsLength) rest

                oldArgsLength : Int
                oldArgsLength =
                    List.length oldArgs

                restLength : Int
                restLength =
                    List.length rest

                patternsLength : Int
                patternsLength =
                    List.length patterns
            in
            if oldArgsLength + restLength < patternsLength then
                -- Still not enough
                case Result.Extra.combineMap (\arg -> evalExpression env arg) rest of
                    Ok values ->
                        PartialValue <| Value.PartiallyApplied localEnv (oldArgs ++ values) patterns implementation

                    Err e ->
                        PartialErr e

            else if oldArgsLength + restLength > patternsLength then
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

            else
                -- Just right, we special case this for TCO
                case Result.Extra.combineMap (\arg -> evalExpression env arg) rest of
                    Err e ->
                        PartialErr e

                    Ok values ->
                        let
                            maybeNewEnvValues : EvalResult (Maybe EnvValues)
                            maybeNewEnvValues =
                                match env
                                    (fakeNode <| ListPattern patterns)
                                    (List (oldArgs ++ values))
                        in
                        case maybeNewEnvValues of
                            Err e ->
                                PartialErr e

                            Ok Nothing ->
                                PartialErr <| typeError env "Could not match lambda patterns"

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
                                                PartialErr <| nameError env fullName

                                            Just kernelModule ->
                                                case Dict.get name kernelModule of
                                                    Nothing ->
                                                        PartialErr <| nameError env fullName

                                                    Just ( _, f ) ->
                                                        f
                                                            (Env.call moduleName name env)
                                                            values
                                                            |> PartialResult.fromValue

                                    _ ->
                                        PartialExpression
                                            (localEnv |> Env.with newEnvValues)
                                            implementation

        Ok other ->
            PartialErr <|
                typeError env <|
                    "Trying to apply "
                        ++ Value.toString other
                        ++ ", which is a non-lambda non-variant"


evalFunctionOrValue : Env -> ModuleName -> String -> PartialResult
evalFunctionOrValue env moduleName name =
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
                PartialValue (Value.Bool True)

            ( [], "False" ) ->
                PartialValue (Value.Bool False)

            _ ->
                let
                    qualifiedNameRef : QualifiedNameRef
                    qualifiedNameRef =
                        { moduleName = fixedModuleName, name = name }
                in
                PartialValue (Value.Custom qualifiedNameRef [])

    else
        case moduleName of
            "Elm" :: "Kernel" :: _ ->
                case Dict.get moduleName env.functions of
                    Nothing ->
                        evalKernelFunction env moduleName name

                    Just kernelModule ->
                        case Dict.get name kernelModule of
                            Nothing ->
                                evalKernelFunction env moduleName name

                            Just function ->
                                PartiallyApplied
                                    (Env.call moduleName name env)
                                    []
                                    function.arguments
                                    function.expression
                                    |> PartialValue

            _ ->
                case ( moduleName, Dict.get name env.values ) of
                    ( [], Just (PartiallyApplied localEnv [] [] implementation) ) ->
                        PartialExpression localEnv implementation

                    ( [], Just value ) ->
                        PartialValue value

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

                                else
                                    PartiallyApplied
                                        (Env.call fixedModuleName name env)
                                        []
                                        function.arguments
                                        function.expression
                                        |> PartialValue

                            Nothing ->
                                Syntax.qualifiedNameToString
                                    { moduleName = fixedModuleName
                                    , name = name
                                    }
                                    |> nameError env
                                    |> PartialErr


evalIfBlock : Env -> Node Expression -> Node Expression -> Node Expression -> PartialResult
evalIfBlock env cond true false =
    case evalExpression env cond of
        Err e ->
            PartialErr e

        Ok condValue ->
            case condValue of
                Value.Bool True ->
                    PartialExpression env true

                Value.Bool False ->
                    PartialExpression env false

                _ ->
                    PartialErr <| typeError env "ifThenElse condition was not a boolean"


evalList : Env -> List (Node Expression) -> EvalResult Value
evalList env elements =
    elements
        |> Result.Extra.combineMap (\element -> evalExpression env element)
        |> Result.map List


evalRecord : Env -> List (Node Expression.RecordSetter) -> EvalResult Value
evalRecord env fields =
    fields
        |> Result.Extra.combineMap
            (\(Node _ ( Node _ fieldName, fieldExpr )) ->
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


kernelFunctions : Dict ModuleName (Dict String ( Int, Env -> List Value -> EvalResult Value ))
kernelFunctions =
    Kernel.functions evalFunction


evalFunction : Kernel.EvalFunction
evalFunction localEnv oldArgs patterns implementation =
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
        Ok <| Value.PartiallyApplied localEnv oldArgs patterns implementation

    else
        -- Just right, we special case this for TCO
        let
            env : Env
            env =
                localEnv

            maybeNewEnvValues : EvalResult (Maybe EnvValues)
            maybeNewEnvValues =
                match env
                    (fakeNode <| ListPattern patterns)
                    (List oldArgs)
        in
        case maybeNewEnvValues of
            Err e ->
                Err e

            Ok Nothing ->
                Err <| typeError env "Could not match lambda patterns"

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
                                Err <| nameError env fullName

                            Just kernelModule ->
                                case Dict.get name kernelModule of
                                    Nothing ->
                                        Err <| nameError env fullName

                                    Just ( _, f ) ->
                                        f
                                            (Env.call moduleName name env)
                                            []

                    _ ->
                        evalExpression
                            (localEnv |> Env.with newEnvValues)
                            implementation


evalKernelFunction : Env -> ModuleName -> String -> PartialResult
evalKernelFunction env moduleName name =
    case Dict.get moduleName kernelFunctions of
        Nothing ->
            PartialErr <| nameError env (String.join "." moduleName)

        Just kernelModule ->
            case Dict.get name kernelModule of
                Nothing ->
                    PartialErr <| nameError env <| Syntax.qualifiedNameToString { moduleName = moduleName, name = name }

                Just ( argCount, f ) ->
                    if argCount == 0 then
                        case f (Env.call moduleName name env) [] of
                            Ok value ->
                                PartialValue value

                            Err e ->
                                PartialErr e

                    else
                        PartiallyApplied (Env.empty moduleName)
                            []
                            (List.repeat argCount (fakeNode AllPattern))
                            (fakeNode <| Expression.FunctionOrValue moduleName name)
                            |> PartialValue


evalNegation : Env -> Node Expression -> EvalResult Value
evalNegation env child =
    case evalExpression env child of
        Err e ->
            Err e

        Ok (Value.Int i) ->
            Ok <| Value.Int -i

        Ok (Value.Float f) ->
            Ok <| Value.Float -f

        _ ->
            Err <| typeError env "Trying to negate a non-number"


evalLetBlock : Env -> Expression.LetBlock -> EvalResult Env
evalLetBlock env letBlock =
    let
        envDefs : Set String
        envDefs =
            Set.union
                (Dict.get env.currentModule env.functions
                    |> Maybe.map (Dict.keys >> Set.fromList)
                    |> Maybe.withDefault Set.empty
                )
                (Dict.keys env.values |> Set.fromList)

        isFunction : Node Expression.LetDeclaration -> Bool
        isFunction (Node _ d) =
            case d of
                Expression.LetFunction { declaration } ->
                    List.length (Node.value declaration).arguments > 0

                _ ->
                    False

        sortedDeclarations : Result TopologicalSort.SortError (List (Node Expression.LetDeclaration))
        sortedDeclarations =
            letBlock.declarations
                |> List.indexedMap
                    (\id declaration ->
                        { id = id + 1
                        , declaration = declaration
                        , defVars = declarationDefinedVariables declaration
                        , refVars = Set.diff (declarationFreeVariables declaration) envDefs
                        , cycleAllowed = isFunction declaration
                        }
                    )
                |> TopologicalSort.sort
                    { id = .id
                    , defVars = .defVars
                    , refVars = .refVars
                    , cycleAllowed = .cycleAllowed
                    }
                |> Result.map (List.map .declaration >> List.reverse)

        mapSortError : Env -> Result TopologicalSort.SortError a -> EvalResult a
        mapSortError errEnv sortResult =
            case sortResult of
                Ok a ->
                    Ok a

                Err TopologicalSort.IllegalCycle ->
                    Err <| typeError errEnv "illegal cycle in let block"

                Err TopologicalSort.InternalError ->
                    Err <| typeError errEnv "internal error in let block"

        addDeclaration : Node Expression.LetDeclaration -> Env -> EvalResult Env
        addDeclaration ((Node _ letDeclaration) as node) acc =
            case letDeclaration of
                Expression.LetFunction { declaration } ->
                    case declaration of
                        Node _ ({ name, expression } as implementation) ->
                            if isFunction node then
                                Ok <| Env.addFunction acc.currentModule implementation acc

                            else
                                case evalExpression acc expression of
                                    Err e ->
                                        Err e

                                    Ok value ->
                                        Ok <| Env.addValue (Node.value name) value acc

                Expression.LetDestructuring letPattern letExpression ->
                    case evalExpression acc letExpression of
                        Err e ->
                            Err e

                        Ok letValue ->
                            case match acc letPattern letValue of
                                Err e ->
                                    Err e

                                Ok Nothing ->
                                    Err <| typeError acc "Could not match pattern inside let"

                                Ok (Just patternEnv) ->
                                    Ok (Env.with patternEnv acc)
    in
    sortedDeclarations
        |> mapSortError env
        |> Result.andThen (Result.MyExtra.combineFoldl addDeclaration (Ok env))


declarationFreeVariables : Node Expression.LetDeclaration -> Set String
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


declarationDefinedVariables : Node Expression.LetDeclaration -> Set String
declarationDefinedVariables (Node _ letDeclaration) =
    case letDeclaration of
        Expression.LetFunction { declaration } ->
            Set.singleton <| Node.value (Node.value declaration).name

        Expression.LetDestructuring letPattern _ ->
            patternDefinedVariables letPattern


evalRecordAccess : Env -> Node Expression -> Node String -> EvalResult Value
evalRecordAccess env recordExpr (Node _ field) =
    evalExpression env recordExpr
        |> Result.andThen
            (\value ->
                case value of
                    Value.Record fields ->
                        case Dict.get field fields of
                            Just fieldValue ->
                                Ok fieldValue

                            Nothing ->
                                Err <| typeError env <| "Field " ++ field ++ " not found [record access]"

                    _ ->
                        Err <| typeError env "Trying to access a field on a non-record value"
            )


evalRecordAccessFunction : String -> Value
evalRecordAccessFunction field =
    PartiallyApplied
        (Env.empty [])
        []
        [ fakeNode (VarPattern "r") ]
        (fakeNode <|
            Expression.RecordAccess
                (fakeNode <| Expression.FunctionOrValue [] "r")
                (fakeNode <| String.dropLeft 1 field)
        )


evalRecordUpdate : Env -> Node String -> List (Node Expression.RecordSetter) -> EvalResult Value
evalRecordUpdate env (Node _ name) setters =
    case evalExpression env (fakeNode <| Expression.FunctionOrValue [] name) of
        Err e ->
            Err e

        Ok (Value.Record fields) ->
            Result.MyExtra.combineFoldl
                (\(Node _ ( Node _ fieldName, fieldExpression )) acc ->
                    evalExpression env fieldExpression
                        |> Result.map
                            (\fieldValue ->
                                Dict.insert fieldName fieldValue acc
                            )
                )
                (Ok fields)
                setters
                |> Result.map Value.Record

        Ok _ ->
            Err <| typeError env "Trying to update fields on a value which is not a record"


evalOperator : Env -> String -> EvalResult Value
evalOperator env opName =
    case Dict.get opName Core.operators of
        Nothing ->
            Err <| nameError env opName

        Just kernelFunction ->
            PartiallyApplied
                (Env.call kernelFunction.moduleName opName env)
                []
                [ fakeNode <| VarPattern "l", fakeNode <| VarPattern "r" ]
                (fakeNode <|
                    Expression.Application
                        [ fakeNode <| Expression.FunctionOrValue kernelFunction.moduleName kernelFunction.name
                        , fakeNode <| Expression.FunctionOrValue [] "l"
                        , fakeNode <| Expression.FunctionOrValue [] "r"
                        ]
                )
                |> Ok


isVariant : String -> Bool
isVariant name =
    case String.uncons name of
        Nothing ->
            False

        Just ( first, _ ) ->
            Unicode.isUpper first


evalCase : Env -> Expression.CaseBlock -> EvalResult ( Env, Node Expression )
evalCase env { expression, cases } =
    case evalExpression env expression of
        Err e ->
            Err e

        Ok exprValue ->
            cases
                |> Result.MyExtra.combineFoldl
                    (\( pattern, result ) acc ->
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
                                        ( Env.with additionalEnv env, result )
                                            |> Just
                                            |> Ok
                    )
                    (Ok Nothing)
                |> Result.andThen
                    (\result ->
                        case result of
                            Nothing ->
                                Err <| typeError env <| "Missing case branch for " ++ Value.toString exprValue

                            Just res ->
                                Ok res
                    )


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


evalExpression2 :
    Env
    -> Node Expression
    -> Node Expression
    -> (Value -> Value -> EvalResult value)
    -> EvalResult value
evalExpression2 env l r f =
    case evalExpression env l of
        Err e ->
            Err e

        Ok lValue ->
            case evalExpression env r of
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
