module Main exposing (Model, Msg, main)

import Browser
import Element exposing (Element, column, fill, padding, spacing, text, width)
import Element.Border as Border
import Element.Input as Input
import Eval
import Value


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
            { onPress = Just Eval, label = text "Eval" }
        , text model.output
        ]


init : Model
init =
    { input = "{ a = 13, b = 'c'}.b"
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
                    case Eval.eval model.input of
                        Err e ->
                            Debug.toString e

                        Ok value ->
                            Value.toString value
            }
