module Main exposing (Model, Msg, main)

import Browser
import Element exposing (Element, column, fill, padding, paragraph, spacing, text, width)
import Element.Border as Border
import Element.Input as Input
import Elm.Syntax.Expression as Expression
import Eval exposing (Error(..))
import Parser
import Value exposing (EvalError(..))


type Msg
    = Input String
    | Eval


type alias Model =
    { input : String
    , output : String
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
        , Input.button
            [ padding 10
            , Border.width 1
            ]
            { onPress = Just Eval
            , label =
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
                            text "Eval main"

                        Just name ->
                            text <| "Eval " ++ name ++ ".main"

                else
                    text "Eval"
            }
        , paragraph [] [ text model.output ]
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
    , output = ""
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Input input ->
            { model | input = input }

        Eval ->
            { model
                | output =
                    let
                        result : Result Error Value.Value
                        result =
                            if String.startsWith "module " model.input then
                                Eval.evalModule model.input (Expression.FunctionOrValue [] "main")

                            else
                                Eval.eval model.input
                    in
                    case result of
                        Err e ->
                            errorToString e

                        Ok value ->
                            Value.toString value
            }


errorToString : Error -> String
errorToString err =
    case err of
        ParsingError deadEnds ->
            "Parsing error: " ++ Parser.deadEndsToString deadEnds

        EvalError (TypeError message) ->
            "Type error: " ++ message

        EvalError (Unsupported message) ->
            "Unsupported: " ++ message

        EvalError (NameError name) ->
            "Name error: " ++ name ++ " not found"
