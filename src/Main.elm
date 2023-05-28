module Main exposing (Model, Msg, main)

import Browser
import Element exposing (Element, column, el, fill, padding, paragraph, row, spacing, text, textColumn, width)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Elm.Syntax.Expression as Expression
import Eval
import Eval.Types exposing (CallTree(..), Error(..))
import Parser
import Syntax
import Value exposing (EvalErrorKind(..))


type Msg
    = Input String
    | Eval Bool


type alias Model =
    { input : String
    , output : Result String String
    , trace : Maybe CallTree
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
        [ Input.multiline [ width fill ]
            { spellcheck = False
            , text = model.input
            , onChange = Input
            , label = Input.labelAbove [] <| text "Input"
            , placeholder = Nothing
            }
        , let
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
        , case model.trace of
            Nothing ->
                Element.none

            Just trace ->
                el [ Font.family [ Font.monospace ] ] <| viewTrace trace
        ]


viewTrace : CallTree -> Element msg
viewTrace (CallNode name { args, children, result }) =
    let
        nameRow : Element msg
        nameRow =
            text <|
                Syntax.qualifiedNameToString name
                    ++ " : "
                    ++ String.join " -> " (List.map Value.toString args)
                    ++ " -> "
                    ++ resultString

        resultString : String
        resultString =
            case result of
                Ok v ->
                    Value.toString v

                Err e ->
                    evalErrorToString e

        childrenRows : List (Element msg)
        childrenRows =
            List.map viewTrace children
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
    , trace = Nothing
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Input input ->
            { model | input = input }

        Eval trace ->
            let
                ( result, traced ) =
                    if trace then
                        (if String.startsWith "module " model.input then
                            Eval.traceModule model.input (Expression.FunctionOrValue [] "main")

                         else
                            Eval.trace model.input
                        )
                            |> Tuple.mapSecond Just

                    else
                        ( if String.startsWith "module " model.input then
                            Eval.evalModule model.input (Expression.FunctionOrValue [] "main")

                          else
                            Eval.eval model.input
                        , Nothing
                        )
            in
            { model
                | output = resultToString result
                , trace = traced
            }


resultToString : Result Error Value.Value -> Result String String
resultToString result =
    case result of
        Err e ->
            Err <| errorToString e

        Ok value ->
            Ok <| Value.toString value


errorToString : Error -> String
errorToString err =
    case err of
        ParsingError deadEnds ->
            "Parsing error: " ++ Parser.deadEndsToString deadEnds

        EvalError evalError ->
            evalErrorToString evalError


evalErrorToString : Value.EvalError -> String
evalErrorToString { callStack, error } =
    let
        messageWithType : String
        messageWithType =
            case error of
                TypeError message ->
                    "Type error: " ++ message

                Unsupported message ->
                    "Unsupported: " ++ message

                NameError name ->
                    "Name error: " ++ name ++ " not found"
    in
    messageWithType
        ++ "\nCall stack:\n - "
        ++ String.join "\n - " (List.reverse <| List.map Syntax.qualifiedNameToString callStack)
