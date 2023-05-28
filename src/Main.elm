module Main exposing (Model, Msg, main)

import Browser
import Element exposing (Element, column, fill, padding, paragraph, row, spacing, text, textColumn, width)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Writer
import Eval
import Eval.Module
import Eval.Types as Types exposing (CallTree(..), Error, PartialResult)
import Rope
import Syntax
import Value


type Msg
    = Input String
    | Eval Bool


type alias Model =
    { input : String
    , output : Result String String
    , callTree : List CallTree
    }


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = \model -> Element.layout [] (innerView model)
        , update = update
        }


innerView : Model -> Element Msg
innerView model =
    column
        [ spacing 10
        , padding 10
        ]
        [ Input.multiline
            [ width fill
            , Font.family [ Font.monospace ]
            ]
            { spellcheck = False
            , text = model.input
            , onChange = Input
            , label = Input.labelAbove [] <| text "Input"
            , placeholder = Nothing
            }
        , let
            toRun : String
            toRun =
                if String.startsWith "module " model.input then
                    let
                        moduleName : Maybe String
                        moduleName =
                            model.input
                                |> String.split "\n"
                                |> List.head
                                |> Maybe.withDefault ""
                                |> String.split " "
                                |> List.drop 1
                                |> List.head
                    in
                    case moduleName of
                        Nothing ->
                            "main"

                        Just name ->
                            name ++ ".main"

                else
                    ""
          in
          row [ spacing 10 ]
            [ Input.button
                [ padding 10
                , Border.width 1
                ]
                { onPress = Just (Eval False)
                , label = text <| "Eval " ++ toRun
                }
            , Input.button
                [ padding 10
                , Border.width 1
                ]
                { onPress = Just (Eval True)
                , label = text <| "Trace " ++ toRun
                }
            ]
        , case model.output of
            Ok output ->
                paragraph [] [ text output ]

            Err e ->
                e
                    |> String.split "\n"
                    |> List.map (\line -> paragraph [] [ text line ])
                    |> textColumn [ Font.family [ Font.monospace ] ]
        , if List.isEmpty model.callTree then
            Element.none

          else
            column
                [ Font.family [ Font.monospace ]
                , spacing 10
                ]
                (List.map viewCallTree model.callTree)
        ]


viewCallTree : CallTree -> Element msg
viewCallTree (CallNode kind name { args, children, result }) =
    let
        maybeParens : String -> String
        maybeParens s =
            case String.uncons s of
                Just ( '[', _ ) ->
                    s

                Just ( '(', _ ) ->
                    s

                Just ( '{', _ ) ->
                    s

                Nothing ->
                    s

                Just _ ->
                    if String.contains " " s then
                        "(" ++ s ++ ")"

                    else
                        s

        from : String
        from =
            (Syntax.qualifiedNameToString name
                :: List.map (maybeParens << Value.toString) args
            )
                |> String.join " "

        nameRow : Element msg
        nameRow =
            (from ++ " = " ++ resultString ++ " [" ++ kind ++ "]")
                |> text

        resultString : String
        resultString =
            case result of
                Ok v ->
                    Value.toString v

                Err e ->
                    Types.evalErrorToString e

        childrenRows : List (Element msg)
        childrenRows =
            List.map viewCallTree children
    in
    (nameRow :: childrenRows)
        |> column
            [ Border.widthEach
                { top = 0
                , left = 1
                , right = 0
                , bottom = 0
                }
            , Element.paddingEach
                { top = 0
                , bottom = 0
                , left = 10
                , right = 0
                }
            , spacing 10
            ]


init : Model
init =
    { input = """let
    boom x =
        if x <= 0 then
            False
        else
            boom (x - 1)
in
boom 100000"""
    , output = Ok ""
    , callTree = []
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Input input ->
            { model | input = input }

        Eval tracing ->
            let
                ( result, callTree ) =
                    if tracing then
                        if String.startsWith "module " model.input then
                            Eval.Module.trace model.input (Expression.FunctionOrValue [] "main")

                        else
                            Eval.trace model.input

                    else
                        ( if String.startsWith "module " model.input then
                            Eval.Module.eval model.input (Expression.FunctionOrValue [] "main")

                          else
                            Eval.eval model.input
                        , []
                        )
            in
            { model
                | output = resultToString result
                , callTree = callTree
            }


resultToString : Result Error Value.Value -> Result String String
resultToString result =
    case result of
        Err e ->
            Err <| Types.errorToString e

        Ok value ->
            Ok <| Value.toString value
