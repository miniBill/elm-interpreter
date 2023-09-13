module UI.Theme exposing (button, column, padding, row, rythm)

import Element exposing (Attribute, Element)
import Element.Border as Border
import Element.Input as Input


rythm : number
rythm =
    10


padding : Attribute msg
padding =
    Element.padding rythm


spacing : Attribute msg
spacing =
    Element.spacing rythm


row : List (Attribute msg) -> List (Element msg) -> Element msg
row attrs children =
    Element.row (spacing :: attrs) children


column : List (Attribute msg) -> List (Element msg) -> Element msg
column attrs children =
    Element.column (spacing :: attrs) children


button : List (Attribute msg) -> { onPress : Maybe msg, label : Element msg } -> Element msg
button attrs config =
    Input.button (padding :: Border.width 1 :: attrs) config
