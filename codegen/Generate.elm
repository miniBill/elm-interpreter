module Generate exposing (main)

{-| -}

import Dict
import Elm
import Elm.Annotation as Type
import Elm.Parser
import Elm.Processing
import Elm.Syntax.Declaration as Declaration
import Elm.Syntax.Expression as Expression
import Elm.Syntax.File as File
import Elm.Syntax.Module as Module
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern
import Gen.CodeGen.Generate as Generate exposing (Directory(..))
import Gen.Debug
import Gen.Elm.CodeGen
import Gen.Expr
import Gen.FastDict
import Gen.List
import Gen.Maybe
import Gen.Types
import Json.Decode exposing (Value)
import List.Extra
import Result.Extra


main : Program Value () ()
main =
    Generate.fromDirectory toFiles


toFiles : Directory -> List Elm.File
toFiles modulesSource =
    let
        allFiles : List String
        allFiles =
            traverseDirectoryForFiles modulesSource

        maybeFiles :
            Result
                String
                (List
                    { moduleName : ModuleName
                    , file : Elm.File
                    , hasOperators : Bool
                    }
                )
        maybeFiles =
            allFiles
                |> Result.Extra.combineMap toFile
                |> Result.map
                    (List.filterMap identity
                        >> List.Extra.gatherEqualsBy .moduleName
                        >> List.map
                            (\( { moduleName } as first, rest ) ->
                                { moduleName = moduleName
                                , file =
                                    (first :: rest)
                                        |> List.concatMap .declarations
                                        |> Elm.file moduleName
                                , hasOperators = List.any .hasOperators (first :: rest)
                                }
                            )
                    )
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
                                (Type.list Type.string)
                                (Gen.FastDict.annotation_.dict
                                    Type.string
                                    Gen.Types.annotation_.expr
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
                                Gen.Types.annotation_.qualifiedNameRef
                            )
                        |> Elm.declaration "operators"
                        |> Elm.expose

                core : Elm.File
                core =
                    [ functions, operators ]
                        |> Elm.file [ "Core" ]
            in
            core :: List.map .file files


traverseDirectoryForFiles : Directory -> List String
traverseDirectoryForFiles d =
    let
        go (Directory directory) acc =
            Dict.foldl (\_ subdir -> go subdir)
                (Dict.foldl
                    (\name content iacc ->
                        if String.endsWith ".elm" name then
                            content :: iacc

                        else
                            iacc
                    )
                    acc
                    directory.files
                )
                directory.directories
    in
    go d []


toFile : String -> Result String (Maybe { moduleName : ModuleName, declarations : List Elm.Declaration, hasOperators : Bool })
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


normalModuleToFile : Node ModuleName -> File.File -> { moduleName : ModuleName, declarations : List Elm.Declaration, hasOperators : Bool }
normalModuleToFile (Node _ moduleName) file =
    let
        generatedModuleName : ModuleName
        generatedModuleName =
            "Core" :: moduleName

        namesAndDeclarations : List ( String, Elm.Declaration )
        namesAndDeclarations =
            file.declarations
                |> List.filterMap declarationToGen

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
                                    Just Gen.Types.annotation_.expr
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
                                    let
                                        fixedModule : List Elm.Expression
                                        fixedModule =
                                            if List.isEmpty reverseModule then
                                                List.map Elm.string moduleName

                                            else
                                                List.reverse reverseModule
                                    in
                                    Just
                                        (Elm.tuple
                                            (Elm.string <| Node.value operator)
                                            (Gen.Types.make_.qualifiedNameRef
                                                { moduleName = Elm.list fixedModule
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

        outputDeclarations : List Elm.Declaration
        outputDeclarations =
            if List.isEmpty operators then
                functions :: declarations

            else
                let
                    operatorsDeclaration : Elm.Declaration
                    operatorsDeclaration =
                        operators
                            |> Elm.list
                            |> Elm.declaration "operators"
                            |> Elm.expose
                in
                functions :: operatorsDeclaration :: declarations
    in
    { moduleName = generatedModuleName
    , declarations = outputDeclarations
    , hasOperators = not (List.isEmpty operators)
    }


declarationToGen : Node Declaration.Declaration -> Maybe ( String, Elm.Declaration )
declarationToGen (Node _ declaration) =
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
                , lambdaToGen
                    { args = implementation.arguments
                    , expression = implementation.expression
                    }
                    |> Elm.withType Gen.Types.annotation_.expr
                    |> Elm.declaration name
                    |> Elm.expose
                )

        _ ->
            Nothing


lambdaToGen :
    { args : List (Node Pattern.Pattern)
    , expression : Node Expression.Expression
    }
    -> Elm.Expression
lambdaToGen { args, expression } =
    case args of
        [] ->
            expressionToGen expression

        head :: tail ->
            Gen.Expr.lambda
                (patternToGen head)
                (lambdaToGen { args = tail, expression = expression })


patternToGen : Node Pattern.Pattern -> Elm.Expression
patternToGen (Node _ pattern) =
    case pattern of
        Pattern.AllPattern ->
            Gen.Types.make_.allPattern

        Pattern.UnitPattern ->
            Gen.Types.make_.unitPattern

        Pattern.CharPattern c ->
            Gen.Types.make_.charPattern (Elm.char c)

        Pattern.StringPattern s ->
            Gen.Types.make_.stringPattern (Elm.string s)

        Pattern.IntPattern i ->
            Gen.Types.make_.intPattern (Elm.int i)

        Pattern.HexPattern x ->
            Gen.Types.make_.hexPattern (Elm.hex x)

        Pattern.FloatPattern f ->
            Gen.Types.make_.floatPattern (Elm.float f)

        Pattern.TuplePattern children ->
            Gen.Types.make_.tuplePattern (Elm.list <| List.map patternToGen children)

        Pattern.RecordPattern fields ->
            fields
                |> List.map (\(Node _ field) -> Elm.string field)
                |> Elm.list
                |> Gen.Types.make_.recordPattern

        Pattern.VarPattern name ->
            Gen.Types.make_.varPattern (Elm.string name)

        Pattern.ParenthesizedPattern child ->
            Gen.Types.make_.parenthesizedPattern (patternToGen child)

        Pattern.AsPattern child (Node _ name) ->
            Gen.Types.make_.asPattern
                (patternToGen child)
                (Elm.string name)

        Pattern.UnConsPattern head tail ->
            Gen.Types.make_.unConsPattern (patternToGen head) (patternToGen tail)

        Pattern.ListPattern children ->
            Gen.Types.make_.listPattern (Elm.list <| List.map patternToGen children)

        Pattern.NamedPattern qualifiedNameRef children ->
            Gen.Types.make_.namedPattern (qualifiedNameRefToGen qualifiedNameRef) (Elm.list <| List.map patternToGen children)


qualifiedNameRefToGen : Pattern.QualifiedNameRef -> Elm.Expression
qualifiedNameRefToGen { name, moduleName } =
    Gen.Types.make_.qualifiedNameRef
        { name = Elm.string name
        , moduleName = Elm.list (List.map Elm.string moduleName)
        }


expressionToGen : Node Expression.Expression -> Elm.Expression
expressionToGen (Node _ expression) =
    case expression of
        Expression.UnitExpr ->
            Gen.Types.make_.unit

        Expression.Application [] ->
            Gen.Types.make_.unit

        Expression.Application (head :: tail) ->
            List.foldl Gen.Types.make_.apply
                (expressionToGen head)
                (List.map expressionToGen tail)

        Expression.OperatorApplication opName _ l r ->
            Gen.Types.make_.binOp
                (expressionToGen l)
                (opNameToGen opName)
                (expressionToGen r)

        Expression.FunctionOrValue moduleName name ->
            qualifiedNameRefToGen
                { moduleName = moduleName
                , name = name
                }
                |> Gen.Types.make_.variable

        Expression.IfBlock cond true false ->
            Gen.Types.make_.ifThenElse
                (expressionToGen cond)
                (expressionToGen true)
                (expressionToGen false)

        Expression.PrefixOperator opName ->
            operatorToGen opName

        Expression.Operator opName ->
            operatorToGen opName

        Expression.Integer i ->
            Gen.Types.make_.int (Elm.int i)

        Expression.Hex x ->
            Gen.Types.make_.int (Elm.hex x)

        Expression.Floatable f ->
            Gen.Types.make_.float (Elm.float f)

        Expression.Negation child ->
            Gen.Types.make_.negation (expressionToGen child)

        Expression.Literal s ->
            Gen.Types.make_.string (Elm.string s)

        Expression.CharLiteral c ->
            Gen.Types.make_.char (Elm.char c)

        Expression.TupledExpression children ->
            Gen.Expr.tuple (List.map expressionToGen children)

        Expression.ParenthesizedExpression child ->
            expressionToGen child

        Expression.LetExpression letBlock ->
            letBlockToGen letBlock

        Expression.CaseExpression caseBlock ->
            Gen.Expr.case_
                (expressionToGen caseBlock.expression)
                (List.map caseBranchToGen caseBlock.cases)

        Expression.LambdaExpression lambda ->
            lambdaToGen lambda

        Expression.RecordExpr setters ->
            Gen.Expr.record (List.map recordSetterToGen setters)

        Expression.ListExpr children ->
            Gen.Expr.list (List.map expressionToGen children)

        Expression.RecordAccess child (Node _ field) ->
            Gen.Types.make_.recordAccess (expressionToGen child) (Elm.string field)

        Expression.RecordAccessFunction name ->
            Gen.Types.make_.recordAccessFunction (Elm.string name)

        Expression.RecordUpdateExpression (Node _ name) setters ->
            Gen.Expr.recordUpdate name (List.map recordSetterToGen setters)

        Expression.GLSLExpression s ->
            Gen.Types.make_.gLSLExpression (Elm.string s)


operatorToGen : String -> Elm.Expression
operatorToGen opName =
    Gen.Expr.lambda
        (Gen.Types.make_.varPattern <| Elm.string "$l")
        (Gen.Expr.lambda
            (Gen.Types.make_.varPattern <| Elm.string "$r")
            (Gen.Types.make_.binOp
                (Gen.Expr.val "$l")
                (opNameToGen opName)
                (Gen.Expr.val "$r")
            )
        )


opNameToGen : String -> Elm.Expression
opNameToGen opName =
    case opName of
        "+" ->
            Gen.Elm.CodeGen.plus

        ">>" ->
            Gen.Elm.CodeGen.composer

        "<<" ->
            Gen.Elm.CodeGen.composel

        "^" ->
            Gen.Elm.CodeGen.power

        "*" ->
            Gen.Elm.CodeGen.mult

        "/" ->
            Gen.Elm.CodeGen.div

        "//" ->
            Gen.Elm.CodeGen.intDiv

        "%" ->
            Gen.Elm.CodeGen.modulo

        "-" ->
            Gen.Elm.CodeGen.minus

        "++" ->
            Gen.Elm.CodeGen.append

        "::" ->
            Gen.Elm.CodeGen.cons

        "==" ->
            Gen.Elm.CodeGen.equals

        "/=" ->
            Gen.Elm.CodeGen.notEqual

        "<" ->
            Gen.Elm.CodeGen.lt

        ">" ->
            Gen.Elm.CodeGen.gt

        "<=" ->
            Gen.Elm.CodeGen.lte

        ">=" ->
            Gen.Elm.CodeGen.gte

        "&&" ->
            Gen.Elm.CodeGen.and

        "||" ->
            Gen.Elm.CodeGen.or

        "|>" ->
            Gen.Elm.CodeGen.piper

        "<|" ->
            Gen.Elm.CodeGen.pipel

        _ ->
            Gen.Debug.todo "TODO"


caseBranchToGen : Expression.Case -> Elm.Expression
caseBranchToGen ( pattern, expression ) =
    Elm.tuple
        (patternToGen pattern)
        (expressionToGen expression)


letBlockToGen : Expression.LetBlock -> Elm.Expression
letBlockToGen { declarations, expression } =
    Gen.Expr.letIn
        (List.map letDeclarationToGen declarations)
        (expressionToGen expression)


letDeclarationToGen : Node Expression.LetDeclaration -> Elm.Expression
letDeclarationToGen (Node _ declaration) =
    case declaration of
        Expression.LetFunction function ->
            let
                (Node _ implementation) =
                    function.declaration
            in
            Elm.tuple
                (Gen.Types.make_.varPattern <| Elm.string <| Node.value implementation.name)
                (lambdaToGen
                    { args = implementation.arguments
                    , expression = implementation.expression
                    }
                )

        Expression.LetDestructuring pattern expression ->
            Elm.tuple
                (patternToGen pattern)
                (expressionToGen expression)


recordSetterToGen : Node Expression.RecordSetter -> Elm.Expression
recordSetterToGen (Node _ ( name, value )) =
    Elm.tuple (Elm.string <| Node.value name) (expressionToGen value)
