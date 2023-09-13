module UI.Source exposing (view)

import Core
import Dict
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Elm.Interface exposing (Exposed(..))
import Elm.Syntax.Node as Node
import Elm.Syntax.Range exposing (Location, Range)
import Html exposing (Html, pre, span, text)
import Html.Attributes exposing (style)
import Set exposing (Set)
import UI.Theme as Theme


view : Maybe Range -> String -> Element msg
view maybeHighlight source =
    let
        highlight : Range
        highlight =
            Maybe.withDefault fakeRange maybeHighlight
    in
    source
        |> String.split "\n"
        |> List.indexedMap (viewRow highlight)
        |> List.intersperse (text "\n")
        |> pre
            [ style "line-height" "125%"
            , style "margin" "0"
            , style "padding" "0"
            ]
        |> Element.html
        |> Element.el
            [ Element.width Element.fill
            , Element.alignTop
            , Theme.padding
            , Border.width 1
            , Background.color <| Element.rgb 0.2 0.2 0.2
            , Font.color <| Element.rgb 1 1 1
            ]


fakeRange : Range
fakeRange =
    { start = fakeLocation
    , end = fakeLocation
    }


fakeLocation : Location
fakeLocation =
    { row = -1
    , column = -1
    }


viewRow : Range -> Int -> String -> Html msg
viewRow highlight rowIndex row =
    let
        slice : Int -> Int -> List (Html msg)
        slice from to =
            viewSyntax <| String.slice from to row

        toEnd : Int -> List (Html msg)
        toEnd from =
            viewSyntax <| String.dropLeft from row

        high : List (Html msg) -> List (Html msg)
        high child =
            [ span [ style "background" "#333", style "border" "1px dotted white" ] child ]

        pieces : List (List (Html msg))
        pieces =
            if rowIndex < highlight.start.row || rowIndex > highlight.end.row then
                [ toEnd 0 ]

            else if rowIndex == highlight.start.row then
                if rowIndex == highlight.end.row then
                    [ slice 0 highlight.start.column
                    , high <| slice highlight.start.column highlight.end.column
                    , toEnd highlight.end.column
                    ]

                else
                    [ slice 0 highlight.start.column
                    , high <| toEnd highlight.start.column
                    ]

            else if rowIndex == highlight.end.row then
                [ high <| slice 0 highlight.end.column
                , toEnd highlight.end.column
                ]

            else
                [ high <| toEnd 0 ]
    in
    span [] <| List.concat pieces


keywords : Set String
keywords =
    let
        language : List String
        language =
            -- TODO: finish this.
            -- Or, even better, actually do syntax highlighting; possibly starting from the AST.
            [ "="
            , "as"
            , "else"
            , "exposing"
            , "if"
            , "import"
            , "in"
            , "let"
            , "module"
            , "then"
            ]

        core : List String
        core =
            Core.dependency.interfaces
                |> Dict.values
                |> List.concatMap
                    (\interface ->
                        interface
                            |> List.filterMap extractOperatorName
                    )
    in
    Set.fromList (language ++ core)


extractOperatorName : Exposed -> Maybe String
extractOperatorName exposed =
    case exposed of
        Operator { operator } ->
            Just <| Node.value operator

        _ ->
            Nothing


colors :
    { declaration : String
    , keyword : String
    , number : String
    , operator : String
    , string : String
    }
colors =
    { declaration = "#ffc"
    , keyword = "#88f"
    , number = "#cfc"
    , operator = "#cc4"
    , string = "#c44"
    }


colored : String -> String -> Html msg
colored color content =
    span
        [ style "color" color ]
        [ text content ]


viewSyntax : String -> List (Html msg)
viewSyntax fragment =
    fragment
        |> String.split " "
        |> List.indexedMap viewToken
        |> List.intersperse [ text " " ]
        |> List.concat


viewToken : Int -> String -> List (Html msg)
viewToken tokenIndex token =
    if Set.member token keywords then
        [ colored colors.keyword token ]

    else if String.startsWith "(" token then
        colored colors.operator "(" :: viewToken (tokenIndex + 1) (String.dropLeft 1 token)

    else if String.endsWith ")" token then
        viewToken tokenIndex (String.dropRight 1 token) ++ [ colored colors.operator ")" ]

    else if String.startsWith "\"" token && String.endsWith "\"" token then
        [ colored colors.string token ]

    else if String.toFloat token /= Nothing then
        [ colored colors.number token ]

    else if tokenIndex == 0 then
        [ colored colors.declaration token ]

    else
        [ text token ]
