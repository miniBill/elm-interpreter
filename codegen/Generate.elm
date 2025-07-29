module Generate exposing (main)

{-| -}

import Dict
import Elm
import Elm.Annotation as Type
import Elm.Dependency exposing (Dependency)
import Elm.Interface exposing (Exposed, Interface)
import Elm.Parser
import Elm.Processing
import Elm.RawFile exposing (RawFile)
import Elm.Syntax.Declaration as Declaration
import Elm.Syntax.Expression as Expression
import Elm.Syntax.File as File
import Elm.Syntax.Infix as Infix
import Elm.Syntax.Module as Module
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern
import Gen.CodeGen.Generate as Generate exposing (Directory(..))
import Gen.Dict
import Gen.Elm.Dependency
import Gen.Elm.Interface
import Gen.Elm.Syntax.Expression
import Gen.Elm.Syntax.Infix
import Gen.Elm.Syntax.ModuleName
import Gen.Elm.Syntax.Pattern
import Gen.FastDict
import Gen.H
import Gen.List
import Gen.Maybe
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
                    , interface : Interface
                    }
                )
        maybeFiles =
            allFiles
                |> List.filterMap
                    (\file ->
                        case Elm.Parser.parse file of
                            Err _ ->
                                case String.split "\n" file of
                                    [] ->
                                        Just (Err "Empty")

                                    head :: _ ->
                                        Just (Err head)

                            Ok rawFile ->
                                let
                                    selfDependencies : List Dependency
                                    selfDependencies =
                                        []
                                in
                                toFile selfDependencies rawFile
                                    |> Maybe.map Ok
                    )
                |> Result.Extra.combine
                |> Result.map
                    (\files ->
                        files
                            |> List.Extra.gatherEqualsBy .moduleName
                            |> List.map
                                (\( { moduleName } as first, rest ) ->
                                    let
                                        all :
                                            List
                                                { moduleName : ModuleName
                                                , declarations : List Elm.Declaration
                                                , hasOperators : Bool
                                                , interface : Interface
                                                }
                                        all =
                                            first :: rest
                                    in
                                    { moduleName = moduleName
                                    , file =
                                        all
                                            |> List.concatMap .declarations
                                            |> Elm.file moduleName
                                    , hasOperators = List.any .hasOperators all
                                    , interface = List.concatMap .interface all
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

                dependency : Elm.Declaration
                dependency =
                    Gen.Elm.Dependency.make_.dependency
                        { name = Elm.string "elm/core"
                        , version = Elm.string "1.0.0"
                        , interfaces =
                            files
                                |> List.map
                                    (\{ moduleName, interface } ->
                                        Elm.tuple
                                            (Elm.list <| List.map Elm.string <| List.drop 1 moduleName)
                                            (interfaceToGen interface)
                                    )
                                |> Gen.Dict.fromList
                        }
                        |> Elm.declaration "dependency"
                        |> Elm.expose

                core : Elm.File
                core =
                    [ functions
                    , operators
                    , dependency
                    ]
                        |> Elm.file [ "Core" ]
            in
            core :: List.map .file files


interfaceToGen : Interface -> Elm.Expression
interfaceToGen interface =
    Elm.list (List.map exposedToGen interface)


exposedToGen : Exposed -> Elm.Expression
exposedToGen exposed =
    case exposed of
        Elm.Interface.Function name ->
            Gen.Elm.Interface.make_.function (Elm.string name)

        Elm.Interface.CustomType ( name, ctors ) ->
            Gen.Elm.Interface.make_.customType
                (Elm.tuple (Elm.string name)
                    (Elm.list <| List.map Elm.string ctors)
                )

        Elm.Interface.Alias name ->
            Gen.Elm.Interface.make_.alias (Elm.string name)

        Elm.Interface.Operator fixity ->
            Gen.Elm.Interface.make_.operator
                (Gen.Elm.Syntax.Infix.make_.infix
                    { direction = renode directionToGen fixity.direction
                    , function = renode Elm.string fixity.function
                    , operator = renode Elm.string fixity.operator
                    , precedence = renode Elm.int fixity.precedence
                    }
                )


directionToGen : Infix.InfixDirection -> Elm.Expression
directionToGen direction =
    case direction of
        Infix.Left ->
            Gen.Elm.Syntax.Infix.make_.left

        Infix.Right ->
            Gen.Elm.Syntax.Infix.make_.right

        Infix.Non ->
            Gen.Elm.Syntax.Infix.make_.non


traverseDirectoryForFiles : Directory -> List String
traverseDirectoryForFiles d =
    let
        go : Bool -> Directory -> List String -> List String
        go inSrc (Directory directory) acc =
            case Dict.get "src" directory.directories of
                Just src ->
                    go True src acc

                Nothing ->
                    Dict.foldl
                        (\_ subdir iacc ->
                            go inSrc subdir iacc
                        )
                        (if inSrc then
                            Dict.foldl
                                (\name content iacc ->
                                    if String.endsWith ".elm" name then
                                        content :: iacc

                                    else
                                        iacc
                                )
                                acc
                                directory.files

                         else
                            acc
                        )
                        directory.directories
    in
    go False d []


type alias FileResult a =
    { a
        | moduleName : ModuleName
        , declarations : List Elm.Declaration
        , hasOperators : Bool
    }


toFile : List Dependency -> RawFile -> Maybe (FileResult { interface : Interface })
toFile selfDependencies rawFile =
    let
        context : Elm.Processing.ProcessContext
        context =
            List.foldl Elm.Processing.addDependency Elm.Processing.init selfDependencies

        file : File.File
        file =
            Elm.Processing.process context rawFile
    in
    case Node.value file.moduleDefinition of
        Module.EffectModule _ ->
            -- Effect modules are not supported
            Nothing

        Module.PortModule _ ->
            -- Port modules are not supported
            Nothing

        Module.NormalModule { moduleName } ->
            let
                normal : FileResult {}
                normal =
                    normalModuleToFile moduleName file
            in
            { moduleName = normal.moduleName
            , declarations = normal.declarations
            , hasOperators = normal.hasOperators
            , interface = Elm.Interface.build rawFile
            }
                |> Just


normalModuleToFile : Node ModuleName -> File.File -> FileResult {}
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
                                functionName : String
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
                                            (Gen.Elm.Syntax.Pattern.make_.qualifiedNameRef
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
                    |> Elm.expose
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
renode toGen (Node range value) =
    if range.start.row == range.end.row then
        Gen.H.node1 range.start.row range.start.column range.end.column (toGen value)

    else
        Gen.H.node range.start.row range.start.column range.end.row range.end.column (toGen value)


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

        Expression.FunctionOrValue [] name ->
            Gen.H.val name

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
