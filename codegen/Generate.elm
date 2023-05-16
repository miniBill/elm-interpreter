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
import Elm.Syntax.ModuleName as ModuleName
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern
import Gen.CodeGen.Generate as Generate
import Gen.Debug
import Gen.Elm.Syntax.Expression
import Gen.Elm.Syntax.Infix
import Gen.Elm.Syntax.Node
import Gen.Elm.Syntax.Pattern
import Gen.FastDict
import Gen.List
import Gen.Maybe
import Gen.Syntax


main : Program String () ()
main =
    Generate.fromText toFiles


toFiles : String -> List Elm.File
toFiles modulesSource =
    let
        namesAndFiles : List ( Maybe (List String), Elm.File )
        namesAndFiles =
            modulesSource
                |> String.split "---SNIP---"
                |> List.map toFile

        files : List Elm.File
        files =
            List.map Tuple.second
                namesAndFiles

        core : Elm.File
        core =
            namesAndFiles
                |> List.filterMap
                    (\( maybeName, _ ) ->
                        Maybe.map
                            (\name ->
                                Elm.value
                                    { importFrom = name
                                    , name = "functions"
                                    , annotation = Nothing
                                    }
                            )
                            maybeName
                    )
                |> Elm.list
                |> Gen.List.call_.concat
                |> Gen.List.call_.map
                    (Elm.fn ( "fun", Nothing ) <|
                        \fun ->
                            Elm.tuple
                                (Gen.Elm.Syntax.Node.value (fun |> Elm.get "name"))
                                fun
                    )
                |> Gen.FastDict.call_.fromList
                |> Elm.withType
                    (Gen.FastDict.annotation_.dict
                        Type.string
                        Gen.Elm.Syntax.Expression.annotation_.functionImplementation
                    )
                |> Elm.declaration "functions"
                |> Elm.expose
                |> List.singleton
                |> Elm.file [ "Core" ]
    in
    core :: files


toFile : String -> ( Maybe (List String), Elm.File )
toFile moduleSource =
    case Elm.Parser.parse moduleSource of
        Err _ ->
            let
                firstLine : String
                firstLine =
                    moduleSource
                        |> String.split "\n"
                        |> List.head
                        |> Maybe.withDefault ""

                moduleName : String
                moduleName =
                    firstLine
                        |> String.split " "
                        |> List.drop 1
                        |> List.head
                        |> Maybe.withDefault "???"
            in
            ( Nothing
            , Elm.file [ "Core", moduleName ] [ Elm.declaration "somethingWentWrong" <| Elm.string firstLine ]
            )

        Ok rawFile ->
            let
                file : File.File
                file =
                    Elm.Processing.process
                        Elm.Processing.init
                        rawFile
            in
            case Node.value file.moduleDefinition of
                Module.EffectModule { moduleName } ->
                    ( Nothing
                    , Elm.file ("Core" :: Node.value moduleName)
                        [ Elm.expose <|
                            Elm.declaration "unsupported" <|
                                Elm.string "Effect modules are not supported"
                        ]
                    )

                Module.PortModule { moduleName } ->
                    ( Nothing
                    , Elm.file ("Core" :: Node.value moduleName)
                        [ Elm.expose <|
                            Elm.declaration "unsupported" <|
                                Elm.string "Port modules are not supported"
                        ]
                    )

                Module.NormalModule { moduleName } ->
                    normalModuleToFile moduleName file


normalModuleToFile : Node ModuleName.ModuleName -> File.File -> ( Maybe (List String), Elm.File )
normalModuleToFile (Node _ moduleName) file =
    let
        generatedModuleName : List String
        generatedModuleName =
            "Core" :: moduleName

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
                        Elm.value
                            { importFrom = []
                            , name = name
                            , annotation =
                                Just
                                    Gen.Elm.Syntax.Expression.annotation_.functionImplementation
                            }
                    )
                |> Elm.list
                |> Elm.withType
                    (Type.list
                        Gen.Elm.Syntax.Expression.annotation_.functionImplementation
                    )
                |> Elm.declaration "functions"
                |> Elm.expose
    in
    ( Just generatedModuleName
    , (functions :: declarations)
        |> Elm.file generatedModuleName
    )


declarationToGen : ModuleName.ModuleName -> Node Declaration.Declaration -> Maybe ( String, Elm.Declaration )
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

        Expression.RecordAccessFunction _ ->
            --Gen.Elm.Syntax.Expression.make_.recordAccessFunction
            Gen.Debug.todo "recordAccessFunction"

        Expression.RecordUpdateExpression _ _ ->
            --Gen.Elm.Syntax.Expression.make_.recordUpdateExpression
            Gen.Debug.todo "recordUpdateExpression"

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
