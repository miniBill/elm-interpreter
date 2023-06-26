module Eval exposing (andThen, combineMap, errorToString, fail, foldl, foldr, fromResult, map, map2, match, matchInfallible, nameError, onExpr, succeed, toModule, toResult, todo, typeError, unsupported)

import FastDict as Dict exposing (Dict)
import Parser
import Rope
import Syntax
import Types exposing (Env, Error(..), Eval, EvalErrorData, EvalErrorKind(..), EvalResult, Expr(..), Pattern(..))


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


typeError : Env -> String -> EvalErrorData
typeError env msg =
    error env (TypeError msg)


nameError : Env -> String -> EvalErrorData
nameError env msg =
    error env (NameError msg)


unsupported : Env -> String -> EvalErrorData
unsupported env msg =
    error env (Unsupported msg)


todo : Env -> String -> EvalErrorData
todo env msg =
    error env (Todo msg)


error : Env -> EvalErrorKind -> EvalErrorData
error env msg =
    { callStack = env.callStack
    , error = msg
    }


onExpr : (a -> Result EvalErrorData out) -> EvalResult a -> EvalResult out
onExpr f ( x, logs ) =
    ( Result.andThen f x
    , logs
    )


andThen : (a -> EvalResult b) -> EvalResult a -> EvalResult b
andThen f ( v, logs ) =
    case v of
        Err e ->
            ( Err e, logs )

        Ok w ->
            let
                ( y, fxLogs ) =
                    f w
            in
            ( y
            , Rope.appendTo logs fxLogs
            )


map : (a -> out) -> EvalResult a -> EvalResult out
map f ( x, logs ) =
    ( Result.map f x
    , logs
    )


map2 : (a -> b -> out) -> EvalResult a -> EvalResult b -> EvalResult out
map2 f ( lv, ll ) ( rv, rl ) =
    ( Result.map2 f lv rv
    , Rope.appendTo ll rl
    )


combineMap : (a -> Eval b) -> List a -> Eval (List b)
combineMap f xs cfg env =
    List.foldr
        (\el acc ->
            case toResult acc of
                Err _ ->
                    acc

                Ok _ ->
                    map2 (::)
                        (f el cfg env)
                        acc
        )
        (succeed [])
        xs


foldl : (a -> out -> Eval out) -> out -> List a -> Eval out
foldl f init xs cfg env =
    List.foldl
        (\el acc ->
            case toResult acc of
                Err _ ->
                    acc

                Ok a ->
                    f el a cfg env
        )
        (succeed init)
        xs


foldr : (a -> out -> Eval out) -> out -> List a -> Eval out
foldr f init xs cfg env =
    List.foldr
        (\el acc ->
            case toResult acc of
                Err _ ->
                    acc

                Ok a ->
                    f el a cfg env
        )
        (succeed init)
        xs


succeed : a -> EvalResult a
succeed x =
    fromResult <| Ok x


fail : EvalErrorData -> EvalResult a
fail e =
    fromResult <| Err e


fromResult : Result EvalErrorData a -> EvalResult a
fromResult x =
    ( x, Rope.empty )


toResult : EvalResult out -> Result EvalErrorData out
toResult ( res, _ ) =
    res


errorToString : Error -> String
errorToString err =
    case err of
        ParsingError deadEnds ->
            "Parsing error: " ++ Parser.deadEndsToString deadEnds

        EvalError evalError ->
            evalErrorToString evalError


evalErrorToString : EvalErrorData -> String
evalErrorToString data =
    let
        messageWithType : String
        messageWithType =
            case data.error of
                TypeError message ->
                    "Type error: " ++ message

                Unsupported message ->
                    "Unsupported: " ++ message

                NameError name ->
                    "Name error: " ++ name ++ " not found"

                Todo message ->
                    "Todo: " ++ message
    in
    messageWithType
        ++ "\nCall stack:\n - "
        ++ String.join "\n - " (List.reverse <| List.map Syntax.qualifiedNameToString data.callStack)


matchInfallible : Env -> Pattern -> Expr -> Result EvalErrorData (Dict String Expr)
matchInfallible env pattern value =
    case match env pattern value of
        Err e ->
            Err e

        Ok Nothing ->
            Err <| typeError env "Could not match pattern"

        Ok (Just r) ->
            Ok r


match : Env -> Pattern -> Expr -> Result EvalErrorData (Maybe (Dict String Expr))
match env pattern value =
    let
        ok : a -> Result error (Maybe a)
        ok val =
            Ok (Just val)

        noMatch : Result error (Maybe a)
        noMatch =
            Ok Nothing

        andThen_ : (a -> Result error (Maybe a)) -> Result error (Maybe a) -> Result error (Maybe a)
        andThen_ f v =
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
                        Dict String Expr
                        -> ( List Pattern, List Expr )
                        -> Result EvalErrorData (Maybe (Dict String Expr))
                    matchNamedPatternHelper envValues queue =
                        case queue of
                            ( [], [] ) ->
                                ok envValues

                            ( patternHead :: patternTail, argHead :: argTail ) ->
                                match env patternHead argHead
                                    |> andThen_
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
                |> andThen_
                    (\headEnv ->
                        match env (ListPattern patternTail) (List listTail)
                            |> andThen_
                                (\tailEnv ->
                                    ok
                                        (Dict.union tailEnv headEnv)
                                )
                    )

        ( UnConsPattern patternHead patternTail, List (listHead :: listTail) ) ->
            match env patternHead listHead
                |> andThen_
                    (\headEnv ->
                        match env patternTail (List listTail)
                            |> andThen_
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

        ( TuplePattern [ lpattern, rpattern ], Tuple [ lvalue, rvalue ] ) ->
            match env lpattern lvalue
                |> andThen_
                    (\lenv ->
                        match env rpattern rvalue
                            |> andThen_
                                (\renv ->
                                    ok <| Dict.union renv lenv
                                )
                    )

        ( TuplePattern [ lpattern, mpattern, rpattern ], Tuple [ lvalue, mvalue, rvalue ] ) ->
            match env lpattern lvalue
                |> andThen_
                    (\lenv ->
                        match env mpattern mvalue
                            |> andThen_
                                (\menv ->
                                    match env rpattern rvalue
                                        |> andThen_
                                            (\renv ->
                                                ok <| Dict.union renv <| Dict.union menv lenv
                                            )
                                )
                    )

        ( TuplePattern _, _ ) ->
            noMatch

        ( AsPattern childPattern asName, _ ) ->
            match env childPattern value
                |> andThen_
                    (\e -> ok <| Dict.insert asName value e)

        ( RecordPattern fields, Record fieldValues ) ->
            List.foldl
                (\fieldName ->
                    andThen_
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
