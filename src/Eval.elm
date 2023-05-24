module Eval exposing (Error(..), eval, evalModule)

import Core
import Core.Basics
import Elm.Parser
import Elm.Processing
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression as Expression exposing (Expression, Function)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Module exposing (Module(..))
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..), QualifiedNameRef)
import Env
import FastDict as Dict
import Kernel
import List.Extra
import Parser exposing (DeadEnd)
import Result.Extra
import Result.MyExtra
import Syntax exposing (fakeNode)
import Unicode
import Value exposing (Env, EnvValues, EvalError(..), EvalResult, Value(..), nameError, typeError, unsupported)


type Error
    = ParsingError (List DeadEnd)
    | EvalError
        { currentModule : ModuleName
        , currentFunction : Maybe String
        , error : EvalError
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
            , currentFunction = Nothing
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
                    Result.mapError EvalError <| unsupported env "Port declaration"

                InfixDeclaration _ ->
                    Result.mapError EvalError <| unsupported env "Infix declaration"

                Destructuring _ _ ->
                    Result.mapError EvalError <| unsupported env "Top level destructuring"

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
                    typeError env <| "|| applied to non-Bool " ++ Value.toString v

        Expression.OperatorApplication "&&" _ l r ->
            case evalExpression env l of
                Ok (Bool False) ->
                    Ok (Bool False)

                Ok (Bool True) ->
                    evalExpression env r

                Err e ->
                    Err e

                Ok v ->
                    typeError env <| "&& applied to non-Bool " ++ Value.toString v

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
            typeError env "Empty application"

        Expression.Application (first :: rest) ->
            case evalExpression env first of
                Err e ->
                    Err e

                Ok (Value.Custom name customArgs) ->
                    rest
                        |> Result.Extra.combineMap (\arg -> evalExpression env arg)
                        |> Result.map (\values -> Value.Custom name (customArgs ++ values))

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
                        rest
                            |> Result.Extra.combineMap (\arg -> evalExpression env arg)
                            |> Result.map (\values -> Value.PartiallyApplied localEnv (oldArgs ++ values) patterns implementation)

                    else if oldArgsLength + restLength > patternsLength then
                        -- Too many args, we split
                        evalExpression
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
                                Err e

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
                                        Err e

                                    Ok Nothing ->
                                        typeError env "Could not match lambda patterns"

                                    Ok (Just newEnvValues) ->
                                        case implementation of
                                            Node _ (Expression.FunctionOrValue (("Elm" :: "Kernel" :: _) as moduleName) name) ->
                                                let
                                                    fullName : String
                                                    fullName =
                                                        Syntax.qualifiedNameToString { moduleName = moduleName, name = name }
                                                in
                                                case Dict.get moduleName Kernel.functions of
                                                    Nothing ->
                                                        nameError env fullName

                                                    Just kernelModule ->
                                                        case Dict.get name kernelModule of
                                                            Nothing ->
                                                                nameError env fullName

                                                            Just ( _, f ) ->
                                                                f
                                                                    { env
                                                                        | currentModule = moduleName
                                                                        , currentFunction = Just name
                                                                    }
                                                                    values

                                            _ ->
                                                evalExpression
                                                    (localEnv () |> Env.with newEnvValues)
                                                    implementation

                Ok other ->
                    typeError env <|
                        "Trying to apply "
                            ++ Value.toString other
                            ++ ", which is a non-lambda non-variant"

        Expression.FunctionOrValue moduleName name ->
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
                        Ok (Value.Bool True)

                    ( [], "False" ) ->
                        Ok (Value.Bool False)

                    _ ->
                        let
                            qualifiedNameRef : QualifiedNameRef
                            qualifiedNameRef =
                                { moduleName = fixedModuleName, name = name }
                        in
                        Ok (Value.Custom qualifiedNameRef [])

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
                                            (\_ -> { env | currentModule = moduleName, currentFunction = Just name })
                                            []
                                            function.arguments
                                            function.expression
                                            |> Ok

                    _ ->
                        case Dict.get name env.values of
                            Just (PartiallyApplied localEnv [] [] implementation) ->
                                evalExpression (localEnv ()) implementation

                            Just value ->
                                Ok value

                            Nothing ->
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
                                            evalExpression
                                                { env
                                                    | currentModule = fixedModuleName
                                                    , currentFunction = Just name
                                                }
                                                function.expression

                                        else
                                            PartiallyApplied
                                                (\_ ->
                                                    { env
                                                        | currentModule = fixedModuleName
                                                        , currentFunction = Just name
                                                    }
                                                )
                                                []
                                                function.arguments
                                                function.expression
                                                |> Ok

                                    Nothing ->
                                        Syntax.qualifiedNameToString
                                            { moduleName = fixedModuleName
                                            , name = name
                                            }
                                            |> nameError env

        Expression.IfBlock cond true false ->
            case evalExpression env cond of
                Err e ->
                    Err e

                Ok condValue ->
                    case condValue of
                        Value.Bool True ->
                            evalExpression env true

                        Value.Bool False ->
                            evalExpression env false

                        _ ->
                            typeError env "ifThenElse condition was not a boolean"

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
                    typeError env "Tuples with more than three elements are not supported"

        Expression.ParenthesizedExpression child ->
            evalExpression env child

        Expression.LetExpression letBlock ->
            case evalLetBlock env letBlock of
                Err e ->
                    Err e

                Ok newEnv ->
                    evalExpression newEnv letBlock.expression

        Expression.CaseExpression caseExpr ->
            evalCase env caseExpr

        Expression.LambdaExpression lambda ->
            Ok <| PartiallyApplied (\_ -> env) [] lambda.args lambda.expression

        Expression.RecordExpr fields ->
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

        Expression.ListExpr elements ->
            elements
                |> Result.Extra.combineMap
                    (\element -> evalExpression env element)
                |> Result.map List

        Expression.RecordAccess recordExpr field ->
            evalRecordAccess env recordExpr field

        Expression.RecordAccessFunction field ->
            Ok <| evalRecordAccessFunction field

        Expression.RecordUpdateExpression name setters ->
            evalRecordUpdate env name setters

        Expression.GLSLExpression _ ->
            unsupported env "GLSL not supported"


evalKernelFunction : Env -> ModuleName -> String -> EvalResult Value
evalKernelFunction env moduleName name =
    case Dict.get moduleName Kernel.functions of
        Nothing ->
            nameError env (String.join "." moduleName)

        Just kernelModule ->
            case Dict.get name kernelModule of
                Nothing ->
                    nameError env <| Syntax.qualifiedNameToString { moduleName = moduleName, name = name }

                Just ( argCount, f ) ->
                    if argCount == 0 then
                        f { env | currentModule = moduleName, currentFunction = Just name } []

                    else
                        PartiallyApplied (\_ -> Env.empty { moduleName = moduleName, functionName = Just name })
                            []
                            (List.repeat argCount (fakeNode AllPattern))
                            (fakeNode <| Expression.FunctionOrValue moduleName name)
                            |> Ok


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
            typeError env "Trying to negate a non-number"


evalLetBlock : Env -> Expression.LetBlock -> EvalResult Env
evalLetBlock env letBlock =
    let
        evalLetFunction : Function -> Env -> Env
        evalLetFunction function acc =
            let
                (Node _ implementation) =
                    function.declaration

                functionVal : Value
                functionVal =
                    -- The error is irrelevant, it will fail earlier
                    PartiallyApplied (\_ -> knot () |> Result.withDefault (withFunctions ()))
                        []
                        implementation.arguments
                        implementation.expression
            in
            Env.addValue
                (Node.value implementation.name)
                functionVal
                acc

        withFunctions : () -> Env
        withFunctions () =
            List.foldl
                (\(Node _ letDeclaration) acc ->
                    case letDeclaration of
                        Expression.LetFunction function ->
                            evalLetFunction function acc

                        Expression.LetDestructuring _ _ ->
                            acc
                )
                env
                letBlock.declarations

        letPatterns : List ( Node Pattern, Node Expression )
        letPatterns =
            List.filterMap
                (\(Node _ letDeclaration) ->
                    case letDeclaration of
                        Expression.LetDestructuring letPattern letExpression ->
                            Just ( letPattern, letExpression )

                        Expression.LetFunction _ ->
                            Nothing
                )
                letBlock.declarations

        knot : () -> EvalResult Env
        knot () =
            let
                knotHelperRound : List ( Node Pattern, Node Expression ) -> Env -> EvalResult Env
                knotHelperRound remaining acc =
                    knotHelper remaining [] False acc

                -- TODO: use topological sort to avoid retries and get better errors
                knotHelper : List ( Node Pattern, Node Expression ) -> List ( Node Pattern, Node Expression ) -> Bool -> Env -> EvalResult Env
                knotHelper remaining later loop acc =
                    case remaining of
                        [] ->
                            if List.isEmpty later then
                                Ok acc

                            else if not loop then
                                typeError env "Loop detected evaluating values in a loop block"

                            else
                                knotHelperRound later acc

                        (( letPattern, letExpression ) as remaningHead) :: remainingQueue ->
                            case evalExpression acc letExpression of
                                Err e ->
                                    case e.error of
                                        NameError _ ->
                                            knotHelper remainingQueue (remaningHead :: later) loop acc

                                        _ ->
                                            Err e

                                Ok letValue ->
                                    case match env letPattern letValue of
                                        Err e ->
                                            Err e

                                        Ok Nothing ->
                                            typeError env "Match failed in let block"

                                        Ok (Just newEnvValues) ->
                                            knotHelper remainingQueue later True (Env.with newEnvValues acc)
            in
            knotHelperRound letPatterns (withFunctions ())
    in
    knot ()


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
                                typeError env <| "Field " ++ field ++ " not found [record access]"

                    _ ->
                        typeError env "Trying to access a field on a non-record value"
            )


evalRecordAccessFunction : String -> Value
evalRecordAccessFunction field =
    PartiallyApplied
        (\_ -> Env.empty { moduleName = [], functionName = Just <| "." ++ field })
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
            typeError env "Trying to update fields on a value which is not a record"


evalOperator : Env -> String -> EvalResult Value
evalOperator env opName =
    case Dict.get opName Core.operators of
        Nothing ->
            nameError env opName

        Just kernelFunction ->
            PartiallyApplied
                (\_ ->
                    { currentModule = kernelFunction.moduleName
                    , currentFunction = Just opName
                    , values = Dict.empty
                    , functions = Core.functions
                    }
                )
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


evalCase : Env -> Expression.CaseBlock -> EvalResult Value
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
                                        evalExpression (Env.with additionalEnv env) result
                                            |> Result.map Just
                    )
                    (Ok Nothing)
                |> Result.andThen
                    (\result ->
                        case result of
                            Nothing ->
                                typeError env <| "Missing case branch for " ++ Value.toString exprValue

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
                                typeError env "Mismatched number of arguments to variant"
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
                                    typeError env <| "Field " ++ fieldName ++ " not found in record"

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
