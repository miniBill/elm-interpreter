module Generate exposing (main)

{-| -}

import Elm
import Elm.Parser
import Elm.Processing
import Elm.Syntax.Declaration as Declaration
import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.File as File
import Elm.Syntax.Module as Module
import Elm.Syntax.Node as Node
import Gen.CodeGen.Generate as Generate
import Gen.Debug
import Gen.Dict
import Gen.Elm.Syntax.Expression
import Gen.Elm.Syntax.Node
import Gen.List


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
                                    , annotation = Just Gen.Elm.Syntax.Expression.annotation_.functionImplementation
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
                |> Gen.Dict.call_.fromList
                |> Elm.declaration "functions"
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

                Module.NormalModule ({ moduleName } as module_) ->
                    let
                        generatedModuleName : List String
                        generatedModuleName =
                            "Core" :: Node.value moduleName
                    in
                    ( Just generatedModuleName
                    , file.declarations
                        |> List.map (declarationToGen (Node.value module_.exposingList))
                        |> Elm.file generatedModuleName
                    )


declarationToGen : Exposing.Exposing -> Node.Node Declaration.Declaration -> Elm.Declaration
declarationToGen exposing_ declaration =
    Elm.declaration "todo" (Gen.Debug.todo "todo")
