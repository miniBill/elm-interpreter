module Generate exposing (main)

{-| -}

import Dict
import Elm
import Elm.Annotation as Type
import Elm.Dependency exposing (Dependency)
import Elm.Interface exposing (Interface)
import Elm.Parser
import Elm.Processing
import Elm.RawFile exposing (RawFile)
import Elm.Syntax.Declaration as Declaration
import Elm.Syntax.Expression as Expression
import Elm.Syntax.File as File
import Elm.Syntax.Module as Module
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Gen.CodeGen.Generate as Generate exposing (Directory(..))
import Gen.Elm.Dependency
import Gen.Elm.Syntax.Expression
import Gen.Elm.Syntax.ModuleName
import Gen.Elm.Syntax.Pattern
import Gen.FastDict
import Gen.List
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
                    Gen.Elm.Dependency.toExpression_Dependency
                        { name = "elm/core"
                        , version = "1.0.0"
                        , interfaces =
                            files
                                |> List.map
                                    (\{ moduleName, interface } ->
                                        ( List.drop 1 moduleName
                                        , interface
                                        )
                                    )
                                |> Dict.fromList
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


traverseDirectoryForFiles : Directory -> List String
traverseDirectoryForFiles d =
    let
        go : Directory -> List String -> List String
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
                            case List.reverse <| String.split "." functionName of
                                name :: reverseModule ->
                                    let
                                        fixedModule : List String
                                        fixedModule =
                                            if List.isEmpty reverseModule then
                                                moduleName

                                            else
                                                List.reverse reverseModule
                                    in
                                    Just
                                        (Elm.tuple
                                            (Elm.string <| Node.value operator)
                                            (Gen.Elm.Syntax.Pattern.toExpression_QualifiedNameRef
                                                { moduleName = fixedModule
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
                , Gen.Elm.Syntax.Expression.toExpression_FunctionImplementation
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
