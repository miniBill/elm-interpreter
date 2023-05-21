module Generate exposing (main)

{-| -}

import Elm
import Elm.Annotation as Type
import Elm.Parser
import Elm.Processing
import Elm.Syntax.Declaration as Declaration
import Elm.Syntax.Expression as Expression
import Elm.Syntax.File as File
import Elm.Syntax.Infix as Infix
import Elm.Syntax.Module as Module
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern
import Gen.CodeGen.Generate as Generate
import Gen.Elm.Syntax.Expression
import Gen.Elm.Syntax.Infix
import Gen.Elm.Syntax.ModuleName
import Gen.Elm.Syntax.Pattern
import Gen.FastDict
import Gen.List
import Gen.Maybe
import Gen.Syntax
import Result.Extra


main : Program String () ()
main =
    Generate.fromText toFiles


toFiles : String -> List Elm.File
toFiles modulesSource =
    let
        maybeFiles : Result String (List { moduleName : ModuleName, file : Elm.File, hasOperators : Bool })
        maybeFiles =
            modulesSource
                |> String.split "---SNIP---"
                |> Result.Extra.combineMap toFile
                |> Result.map (List.filterMap identity)
    in
    case maybeFiles of
        Err e ->
            [ Elm.file [ "Core" ]
                [ Elm.declaration "somethingWentWrong" (Elm.string e) ]
            ]

        Ok files ->
            let
                functions : Elm.Declaration
                functions =
                    files
                        |> List.map
                            (\{ moduleName } ->
                                Elm.tuple
                                    (Elm.list <| List.map Elm.string <| List.drop 1 moduleName)
                                    (Elm.value
                                        { importFrom = moduleName
                                        , name = "functions"
                                        , annotation = Nothing
                                        }
                                    )
                            )
                        |> Gen.FastDict.fromList
                        |> Elm.withType
                            (Gen.FastDict.annotation_.dict
                                Gen.Elm.Syntax.ModuleName.annotation_.moduleName
                                (Gen.FastDict.annotation_.dict
                                    Type.string
                                    Gen.Elm.Syntax.Expression.annotation_.functionImplementation
                                )
                            )
                        |> Elm.declaration "functions"
                        |> Elm.expose

                operators : Elm.Declaration
                operators =
                    files
                        |> List.filter .hasOperators
                        |> List.map
                            (\{ moduleName } ->
                                Elm.value
                                    { importFrom = moduleName
                                    , name = "operators"
                                    , annotation = Nothing
                                    }
                            )
                        |> Elm.list
                        |> Gen.List.call_.concat
                        |> Gen.FastDict.call_.fromList
                        |> Elm.withType
                            (Gen.FastDict.annotation_.dict
                                Type.string
                                Gen.Elm.Syntax.Pattern.annotation_.qualifiedNameRef
                            )
                        |> Elm.declaration "operators"
                        |> Elm.expose

                core : Elm.File
                core =
                    [ functions, operators ]
                        |> Elm.file [ "Core" ]
            in
            core :: List.map .file files


toFile : String -> Result String (Maybe { moduleName : ModuleName, file : Elm.File, hasOperators : Bool })
toFile moduleSource =
    case Elm.Parser.parse moduleSource of
        Err _ ->
            moduleSource
                |> String.split "\n"
                |> List.head
                |> Maybe.withDefault ""
                |> Err

        Ok rawFile ->
            let
                file : File.File
                file =
                    Elm.Processing.process
                        Elm.Processing.init
                        rawFile
            in
            case Node.value file.moduleDefinition of
                Module.EffectModule _ ->
                    -- Effect modules are not supported
                    Ok Nothing

                Module.PortModule _ ->
                    -- Port modules are not supported
                    Ok Nothing

                Module.NormalModule { moduleName } ->
                    Ok <| Just <| normalModuleToFile moduleName file


normalModuleToFile : Node ModuleName -> File.File -> { moduleName : ModuleName, file : Elm.File, hasOperators : Bool }
normalModuleToFile (Node _ moduleName) file =
    let
        generatedModuleName : ModuleName
        generatedModuleName =
            "Core" :: moduleName

        namesAndDeclarations : List ( String, Elm.Declaration )
        namesAndDeclarations =
            file.declarations
                |> List.filterMap (declarationToGen moduleName)

        names : List String
        names =
            List.map Tuple.first namesAndDeclarations

        declarations : List Elm.Declaration
        declarations =
            List.map Tuple.second namesAndDeclarations

        functions : Elm.Declaration
        functions =
            names
                |> List.map
                    (\name ->
                        Elm.tuple
                            (Elm.string name)
                            (Elm.value
                                { importFrom = []
                                , name = name
                                , annotation =
                                    Just
                                        Gen.Elm.Syntax.Expression.annotation_.functionImplementation
                                }
                            )
                    )
                |> Gen.FastDict.fromList
                |> Elm.declaration "functions"
                |> Elm.expose

        operators : List Elm.Expression
        operators =
            List.filterMap
                (\(Node _ declaration) ->
                    case declaration of
                        Declaration.InfixDeclaration { operator, function } ->
                            let
                                functionName =
                                    Node.value function
                            in
                            case List.reverse <| List.map Elm.string <| String.split "." functionName of
                                name :: reverseModule ->
                                    Just
                                        (Elm.tuple
                                            (Elm.string <| Node.value operator)
                                            (Gen.Elm.Syntax.Pattern.make_.qualifiedNameRef
                                                { moduleName = Elm.list <| List.reverse reverseModule
                                                , name = name
                                                }
                                            )
                                        )

                                [] ->
                                    Nothing

                        _ ->
                            Nothing
                )
                file.declarations

        result : Elm.File
        result =
            if List.isEmpty operators then
                (functions :: declarations)
                    |> Elm.file generatedModuleName

            else
                let
                    operatorsDeclaration : Elm.Declaration
                    operatorsDeclaration =
                        operators
                            |> Elm.list
                            |> Elm.declaration "operators"
                            |> Elm.expose
                in
                (functions :: operatorsDeclaration :: declarations)
                    |> Elm.file generatedModuleName
    in
    { moduleName = generatedModuleName
    , file = result
    , hasOperators = not (List.isEmpty operators)
    }


declarationToGen : ModuleName -> Node Declaration.Declaration -> Maybe ( String, Elm.Declaration )
declarationToGen moduleName (Node _ declaration) =
    case declaration of
        Declaration.FunctionDeclaration function ->
            let
                implementation : Expression.FunctionImplementation
                implementation =
                    Node.value function.declaration

                name : String
                name =
                    Node.value implementation.name
            in
            Just
                ( name
                , functionImplementationToGen
                    { implementation
                        | name =
                            Node
                                (Node.range implementation.name)
                                (String.join "." (moduleName ++ [ name ]))
                    }
                    |> Elm.declaration name
                )

        _ ->
            Nothing


functionImplementationToGen : Expression.FunctionImplementation -> Elm.Expression
functionImplementationToGen { name, arguments, expression } =
    Gen.Elm.Syntax.Expression.make_.functionImplementation
        { name = renode Elm.string name
        , arguments = arguments |> List.map (\pattern -> renode patternToGen pattern) |> Elm.list
        , expression = renode expressionToGen expression
        }


patternToGen : Pattern.Pattern -> Elm.Expression
patternToGen pattern =
    case pattern of
        Pattern.AllPattern ->
            Gen.Elm.Syntax.Pattern.make_.allPattern

        Pattern.UnitPattern ->
            Gen.Elm.Syntax.Pattern.make_.unitPattern

        Pattern.CharPattern c ->
            Gen.Elm.Syntax.Pattern.make_.charPattern (Elm.char c)

        Pattern.StringPattern s ->
            Gen.Elm.Syntax.Pattern.make_.stringPattern (Elm.string s)

        Pattern.IntPattern i ->
            Gen.Elm.Syntax.Pattern.make_.intPattern (Elm.int i)

        Pattern.HexPattern x ->
            Gen.Elm.Syntax.Pattern.make_.hexPattern (Elm.hex x)

        Pattern.FloatPattern f ->
            Gen.Elm.Syntax.Pattern.make_.floatPattern (Elm.float f)

        Pattern.TuplePattern children ->
            Gen.Elm.Syntax.Pattern.make_.tuplePattern (renodeList patternToGen children)

        Pattern.RecordPattern fields ->
            Gen.Elm.Syntax.Pattern.make_.recordPattern (renodeList Elm.string fields)

        Pattern.VarPattern name ->
            Gen.Elm.Syntax.Pattern.make_.varPattern (Elm.string name)

        Pattern.ParenthesizedPattern child ->
            Gen.Elm.Syntax.Pattern.make_.parenthesizedPattern (renode patternToGen child)

        Pattern.AsPattern child name ->
            Gen.Elm.Syntax.Pattern.make_.asPattern (renode patternToGen child) (renode Elm.string name)

        Pattern.UnConsPattern head tail ->
            Gen.Elm.Syntax.Pattern.make_.unConsPattern (renode patternToGen head) (renode patternToGen tail)

        Pattern.ListPattern children ->
            Gen.Elm.Syntax.Pattern.make_.listPattern (renodeList patternToGen children)

        Pattern.NamedPattern qualifiedNameRef children ->
            Gen.Elm.Syntax.Pattern.make_.namedPattern (qualifiedNameRefToGen qualifiedNameRef) (renodeList patternToGen children)


qualifiedNameRefToGen : Pattern.QualifiedNameRef -> Elm.Expression
qualifiedNameRefToGen { name, moduleName } =
    Gen.Elm.Syntax.Pattern.make_.qualifiedNameRef
        { name = Elm.string name
        , moduleName = Elm.list (List.map Elm.string moduleName)
        }


renode : (a -> Elm.Expression) -> Node a -> Elm.Expression
renode toGen (Node _ value) =
    Gen.Syntax.fakeNode (toGen value)


renodeList : (a -> Elm.Expression) -> List (Node a) -> Elm.Expression
renodeList f list =
    Elm.list (List.map (renode f) list)


expressionToGen : Expression.Expression -> Elm.Expression
expressionToGen expression =
    case expression of
        Expression.UnitExpr ->
            Gen.Elm.Syntax.Expression.make_.unitExpr

        Expression.Application children ->
            Gen.Elm.Syntax.Expression.make_.application (renodeList expressionToGen children)

        Expression.OperatorApplication opName infix_ l r ->
            Gen.Elm.Syntax.Expression.make_.operatorApplication
                (Elm.string opName)
                (infixToGen infix_)
                (renode expressionToGen l)
                (renode expressionToGen r)

        Expression.FunctionOrValue moduleName name ->
            Gen.Elm.Syntax.Expression.make_.functionOrValue (Elm.list <| List.map Elm.string moduleName) (Elm.string name)

        Expression.IfBlock cond true false ->
            Gen.Elm.Syntax.Expression.make_.ifBlock
                (renode expressionToGen cond)
                (renode expressionToGen true)
                (renode expressionToGen false)

        Expression.PrefixOperator opName ->
            Gen.Elm.Syntax.Expression.make_.prefixOperator (Elm.string opName)

        Expression.Operator opName ->
            Gen.Elm.Syntax.Expression.make_.operator (Elm.string opName)

        Expression.Integer i ->
            Gen.Elm.Syntax.Expression.make_.integer (Elm.int i)

        Expression.Hex x ->
            Gen.Elm.Syntax.Expression.make_.hex (Elm.hex x)

        Expression.Floatable f ->
            Gen.Elm.Syntax.Expression.make_.floatable (Elm.float f)

        Expression.Negation child ->
            Gen.Elm.Syntax.Expression.make_.negation (renode expressionToGen child)

        Expression.Literal s ->
            Gen.Elm.Syntax.Expression.make_.literal (Elm.string s)

        Expression.CharLiteral c ->
            Gen.Elm.Syntax.Expression.make_.charLiteral (Elm.char c)

        Expression.TupledExpression children ->
            Gen.Elm.Syntax.Expression.make_.tupledExpression (renodeList expressionToGen children)

        Expression.ParenthesizedExpression child ->
            Gen.Elm.Syntax.Expression.make_.parenthesizedExpression (renode expressionToGen child)

        Expression.LetExpression letBlock ->
            Gen.Elm.Syntax.Expression.make_.letExpression (letBlockToGen letBlock)

        Expression.CaseExpression caseBlock ->
            Gen.Elm.Syntax.Expression.make_.caseExpression (caseBlockToGen caseBlock)

        Expression.LambdaExpression lambda ->
            Gen.Elm.Syntax.Expression.make_.lambdaExpression (lambdaToGen lambda)

        Expression.RecordExpr setters ->
            Gen.Elm.Syntax.Expression.make_.recordExpr (renodeList recordSetterToGen setters)

        Expression.ListExpr children ->
            Gen.Elm.Syntax.Expression.make_.listExpr (renodeList expressionToGen children)

        Expression.RecordAccess child field ->
            Gen.Elm.Syntax.Expression.make_.recordAccess (renode expressionToGen child) (renode Elm.string field)

        Expression.RecordAccessFunction name ->
            Gen.Elm.Syntax.Expression.make_.recordAccessFunction (Elm.string name)

        Expression.RecordUpdateExpression name setters ->
            Gen.Elm.Syntax.Expression.make_.recordUpdateExpression (renode Elm.string name) (renodeList recordSetterToGen setters)

        Expression.GLSLExpression s ->
            Gen.Elm.Syntax.Expression.make_.gLSLExpression (Elm.string s)


caseBlockToGen : Expression.CaseBlock -> Elm.Expression
caseBlockToGen { expression, cases } =
    Gen.Elm.Syntax.Expression.make_.caseBlock
        { expression = renode expressionToGen expression
        , cases = Elm.list <| List.map caseToGen cases
        }


caseToGen : Expression.Case -> Elm.Expression
caseToGen ( pattern, expression ) =
    Elm.tuple
        (renode patternToGen pattern)
        (renode expressionToGen expression)


lambdaToGen : Expression.Lambda -> Elm.Expression
lambdaToGen { args, expression } =
    Gen.Elm.Syntax.Expression.make_.lambda
        { args = renodeList patternToGen args
        , expression = renode expressionToGen expression
        }


letBlockToGen : Expression.LetBlock -> Elm.Expression
letBlockToGen { declarations, expression } =
    Gen.Elm.Syntax.Expression.make_.letBlock
        { declarations = renodeList letDeclarationToGen declarations
        , expression = renode expressionToGen expression
        }


letDeclarationToGen : Expression.LetDeclaration -> Elm.Expression
letDeclarationToGen declaration =
    case declaration of
        Expression.LetFunction function ->
            Gen.Elm.Syntax.Expression.make_.letFunction (functionToGen function)

        Expression.LetDestructuring pattern expression ->
            Gen.Elm.Syntax.Expression.make_.letDestructuring (renode patternToGen pattern) (renode expressionToGen expression)


functionToGen : Expression.Function -> Elm.Expression
functionToGen { declaration } =
    Gen.Elm.Syntax.Expression.make_.function
        { documentation = Gen.Maybe.make_.nothing
        , signature = Gen.Maybe.make_.nothing
        , declaration = renode functionImplementationToGen declaration
        }


recordSetterToGen : Expression.RecordSetter -> Elm.Expression
recordSetterToGen ( name, value ) =
    Elm.tuple (renode Elm.string name) (renode expressionToGen value)


infixToGen : Infix.InfixDirection -> Elm.Expression
infixToGen direction =
    case direction of
        Infix.Left ->
            Gen.Elm.Syntax.Infix.make_.left

        Infix.Right ->
            Gen.Elm.Syntax.Infix.make_.right

        Infix.Non ->
            Gen.Elm.Syntax.Infix.make_.non
