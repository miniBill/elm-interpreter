module UI.Source exposing (Button, Config, view)

import Core
import Dict
import Element exposing (Attribute, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Elm.Interface exposing (Exposed(..))
import Elm.Syntax.Node as Node
import Elm.Syntax.Range exposing (Location, Range)
import Html exposing (Html, pre, span, text)
import Html.Attributes exposing (style, title)
import Html.Events exposing (onClick)
import List.Extra
import List.MyExtra
import Parser
import UI.Theme as Theme


type alias Button msg =
    { onPress : Maybe msg
    , range : Range
    , tooltip : Maybe String
    }


type alias Config msg =
    { highlight : Maybe Range
    , source : String
    , buttons : List (Button msg)
    }


view :
    List (Attribute msg)
    -> Config msg
    -> Element msg
view attrs config =
    config.source
        |> String.split "\n"
        |> List.indexedMap
            (\rowIndex row ->
                (String.toList row
                    |> List.indexedMap
                        (\colIndex char ->
                            ( char
                            , { row = rowIndex + 1
                              , column = colIndex + 1
                              }
                            )
                        )
                )
                    ++ [ ( '\n'
                         , { row = rowIndex + 1
                           , column = String.length row + 1
                           }
                         )
                       ]
            )
        |> List.concat
        |> parse config
        |> pre
            [ style "line-height" "125%"
            , style "margin" "0"
            , style "padding" "0"
            ]
        |> Element.html
        |> Element.el
            (Element.width Element.fill
                :: Element.alignTop
                :: Theme.padding
                :: Border.width 1
                :: (Background.color <| Element.rgb 0.2 0.2 0.2)
                :: (Font.color <| Element.rgb 1 1 1)
                :: attrs
            )


type State
    = Initial
    | GettingModuleName
    | WaitingModuleExposing
    | WaitingExposeList
    | ReadingExposeList
    | WaitingDeclaration
    | ReadingDeclaration
    | ReadingExpression
    | ReadingString State
    | ReadingStringEscape State
    | Comment State
    | Error


type alias Parsed msg =
    { color : Maybe String
    , background : Maybe String
    , button : Maybe (Button msg)
    , token : String
    }


parse : Config msg -> List ( Char, Location ) -> List (Html msg)
parse config chars =
    parseHelp Initial [] config.buttons chars


parseHelp : State -> List (Parsed msg) -> List (Button msg) -> List ( Char, Location ) -> List (Html msg)
parseHelp state acc buttons queue =
    case queue of
        [] ->
            List.reverse acc |> aggregate

        ( head, location ) :: tail ->
            let
                ( button, newButtons ) =
                    findButton buttons

                findButton : List (Button msg) -> ( Maybe (Button msg), List (Button msg) )
                findButton bqueue =
                    case bqueue of
                        [] ->
                            ( Nothing, bqueue )

                        bhead :: btail ->
                            case
                                compareLocationRange
                                    location
                                    bhead.range
                            of
                                LT ->
                                    ( Nothing, bqueue )

                                EQ ->
                                    ( Just bhead, buttons )

                                GT ->
                                    findButton btail

                default : Parsed msg
                default =
                    { background = Nothing
                    , button = button
                    , color = Nothing
                    , token = String.fromChar head
                    }

                normal :
                    State
                    ->
                        Parser.Step
                            ( State, Parsed msg, List ( Char, Location ) )
                            (List (Html msg))
                normal newState =
                    ( newState
                    , default
                    , tail
                    )
                        |> Parser.Loop

                operator :
                    State
                    ->
                        Parser.Step
                            ( State, Parsed msg, List ( Char, Location ) )
                            (List (Html msg))
                operator newState =
                    colored colors.operator newState

                colored :
                    String
                    -> State
                    ->
                        Parser.Step
                            ( State, Parsed msg, List ( Char, Location ) )
                            (List (Html msg))
                colored color newState =
                    ( newState
                    , { default | color = Just color }
                    , tail
                    )
                        |> Parser.Loop

                error :
                    ()
                    ->
                        Parser.Step
                            ( State, Parsed msg, List ( Char, Location ) )
                            (List (Html msg))
                error () =
                    colored "red" Error

                seek : String -> Maybe ( String, List ( Char, Location ) )
                seek keyword =
                    let
                        len : Int
                        len =
                            String.length keyword
                    in
                    if
                        (queue
                            |> List.take len
                            |> List.map Tuple.first
                        )
                            == String.toList keyword
                    then
                        Just ( keyword, List.drop len queue )

                    else
                        Nothing

                step :
                    Parser.Step
                        ( State, Parsed msg, List ( Char, Location ) )
                        (List (Html msg))
                step =
                    case ( state, queue ) of
                        ( _, [] ) ->
                            List.reverse acc
                                |> aggregate
                                |> Parser.Done

                        ( Error, _ ) ->
                            error ()

                        ( ReadingString previous, ( '\\', _ ) :: _ ) ->
                            colored colors.string (ReadingStringEscape previous)

                        ( ReadingStringEscape previous, _ ) ->
                            colored colors.string (ReadingString previous)

                        ( ReadingString previous, ( '"', _ ) :: _ ) ->
                            colored colors.string previous

                        ( ReadingString _, _ ) ->
                            colored colors.string state

                        ( _, ( '-', _ ) :: ( '-', _ ) :: _ ) ->
                            ( Comment state
                            , { default
                                | color = Just colors.comment
                                , token = "--"
                              }
                            , tail
                            )
                                |> Parser.Loop

                        ( Comment previous, ( '\n', _ ) :: _ ) ->
                            colored colors.comment previous

                        ( Comment _, _ ) ->
                            colored colors.comment state

                        ( Initial, _ ) ->
                            case seek "module " of
                                Just ( _, moduleTail ) ->
                                    ( GettingModuleName
                                    , { default
                                        | color = Just colors.keyword
                                        , token = "module "
                                      }
                                    , moduleTail
                                    )
                                        |> Parser.Loop

                                Nothing ->
                                    error ()

                        ( GettingModuleName, ( ' ', _ ) :: _ ) ->
                            normal WaitingModuleExposing

                        ( GettingModuleName, _ ) ->
                            normal GettingModuleName

                        ( WaitingModuleExposing, _ ) ->
                            case seek "exposing" of
                                Just ( _, exposingTail ) ->
                                    ( WaitingExposeList
                                    , { color = Just colors.keyword
                                      , background = Nothing
                                      , button = button
                                      , token = "exposing"
                                      }
                                    , exposingTail
                                    )
                                        |> Parser.Loop

                                Nothing ->
                                    error ()

                        ( WaitingExposeList, ( ' ', _ ) :: _ ) ->
                            normal WaitingExposeList

                        ( WaitingExposeList, ( '(', _ ) :: _ ) ->
                            operator ReadingExposeList

                        ( ReadingExposeList, ( ')', _ ) :: _ ) ->
                            operator WaitingDeclaration

                        ( ReadingExposeList, _ ) ->
                            normal ReadingExposeList

                        ( WaitingDeclaration, ( '\n', _ ) :: _ ) ->
                            normal WaitingDeclaration

                        ( WaitingDeclaration, ( ' ', _ ) :: _ ) ->
                            normal ReadingExpression

                        ( WaitingDeclaration, _ ) ->
                            colored colors.declaration ReadingDeclaration

                        ( ReadingDeclaration, ( ' ', _ ) :: _ ) ->
                            normal ReadingExpression

                        ( ReadingDeclaration, _ ) ->
                            colored colors.declaration ReadingDeclaration

                        ( ReadingExpression, ( '\n', _ ) :: _ ) ->
                            normal WaitingDeclaration

                        ( ReadingExpression, _ ) ->
                            case List.Extra.findMap seek keywords of
                                Just ( foundKeyword, foundTail ) ->
                                    ( ReadingExpression
                                    , { color = Just colors.keyword
                                      , background = Nothing
                                      , button = button
                                      , token = foundKeyword
                                      }
                                    , foundTail
                                    )
                                        |> Parser.Loop

                                Nothing ->
                                    normal ReadingExpression

                        _ ->
                            error ()
            in
            case step of
                Parser.Done r ->
                    r

                Parser.Loop ( newState, enqueue, effectiveTail ) ->
                    parseHelp newState
                        (enqueue :: acc)
                        newButtons
                        effectiveTail


{-| Returns `LT` if the location is before the range, `EQ` if it's inside the range, `GT` if it's after the range.
-}
compareLocationRange : Location -> Range -> Order
compareLocationRange { row, column } { start, end } =
    if row < start.row || (row == start.row && column < start.column) then
        LT

    else if row > end.row || (row == end.row && column >= end.column) then
        GT

    else
        EQ


aggregate : List (Parsed msg) -> List (Html msg)
aggregate queue =
    queue
        |> List.MyExtra.groupBy .button
        |> List.concatMap
            (\( button, group ) ->
                let
                    content : List (Html msg)
                    content =
                        group
                            |> List.MyExtra.groupBy
                                (\{ color, background } -> ( color, background ))
                            |> List.map
                                (\( ( color, background ), subgroup ) ->
                                    let
                                        subcontent : Html msg
                                        subcontent =
                                            text <| String.concat <| List.map .token subgroup
                                    in
                                    if color == Nothing && background == Nothing then
                                        subcontent

                                    else
                                        span
                                            (List.filterMap identity
                                                [ Maybe.map (\c -> style "color" c) color
                                                , Maybe.map (\c -> style "background" c) background
                                                ]
                                            )
                                            [ subcontent ]
                                )
                in
                case button of
                    Nothing ->
                        content

                    Just { onPress, tooltip } ->
                        [ span
                            ([ Just <| style "border" "1px dotted #fff"
                             , Just <| style "background" "#300"
                             , Just <| style "cursor" "pointer"
                             , Maybe.map onClick onPress
                             , Maybe.map title tooltip
                             ]
                                |> List.filterMap identity
                            )
                            content
                        ]
            )


keywords : List String
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
    language ++ core


extractOperatorName : Exposed -> Maybe String
extractOperatorName exposed =
    case exposed of
        Operator { operator } ->
            Just <| Node.value operator

        _ ->
            Nothing


colors :
    { comment : String
    , highlightBackground : String
    , declaration : String
    , keyword : String
    , number : String
    , operator : String
    , string : String
    }
colors =
    { comment = "#ccc"
    , highlightBackground = "#660"
    , declaration = "#ffc"
    , keyword = "#88f"
    , number = "#cfc"
    , operator = "#cc4"
    , string = "#c44"
    }
