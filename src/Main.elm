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
import Eval.Types exposing (CallTree(..), Error(..), PartialResult(..), TraceLine)
import Parser
import Rope
import Syntax
import Value exposing (EvalErrorKind(..))


type Msg
    = Input String
    | Eval Bool


type alias Model =
    { input : String
    , output : Result String String
    , callTree : List CallTree
    , trace : List TraceLine
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
        , if List.isEmpty model.trace then
            Element.none

          else
            column [ spacing 10 ] (List.map viewTraceLine model.trace)
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
                    evalErrorToString e

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


viewTraceLine : ( Expression, PartialResult ) -> Element Msg
viewTraceLine ( expr, res ) =
    [ Elm.Writer.write <| Elm.Writer.writeExpression <| Syntax.fakeNode expr
    , " => "
    , partialResultToString res
    ]
        |> String.concat
        |> text


partialResultToString : PartialResult -> String
partialResultToString result =
    case result of
        PartialValue ( Ok v, _, _ ) ->
            Value.toString v

        PartialExpression _ _ _ _ ->
            Debug.todo "branch 'PartialExpression _ _ _' not implemented"

        PartialValue ( Err e, _, _ ) ->
            errorToString (EvalError e)


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
    , trace = []
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Input input ->
            { model | input = input }

        Eval tracing ->
            let
                ( result, callTree, traceLines ) =
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
                        , Rope.empty
                        )
            in
            { model
                | output = resultToString result
                , callTree = callTree
                , trace = Rope.toList traceLines
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
