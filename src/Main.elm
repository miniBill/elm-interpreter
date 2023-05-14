module Main exposing (Model, Msg, main)

import Browser
import Element


type alias Msg =
    ()


type alias Model =
    ()


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = \_ -> Element.layout [] Element.none
        , update = update
        }


init : Model
init =
    ()


update : Msg -> Model -> Model
update msg model =
    model
