module UI.Source exposing (Button, Config, view, viewExpression)

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
import Html.Events
import Json.Decode
import List.Extra
import List.MyExtra
import Maybe.Extra
import Parser
import UI.Theme as Theme
import Unicode


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


type alias InnerConfig msg =
    { highlight : Maybe Range
    , source : String
    , buttons : List (Button msg)
    , forExpression : Bool
    }


viewExpression : List (Attribute msg) -> Config msg -> Element msg
viewExpression attrs config =
    innerView attrs
        { source = config.source
        , highlight = config.highlight
        , buttons = config.buttons
        , forExpression = True
        }


view :
    List (Attribute msg)
    -> Config msg
    -> Element msg
view attrs config =
    innerView attrs
        { source = config.source
        , highlight = config.highlight
        , buttons = config.buttons
        , forExpression = False
        }


innerView : List (Attribute msg) -> InnerConfig msg -> Element msg
innerView attrs config =
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
    | ReadingVariable
    | ReadingNumber
    | ReadingString State
    | ReadingStringEscape State
    | Comment State
    | Error


type alias Parsed msg =
    { color : Maybe String
    , background : Maybe String
    , button : Maybe (Button msg)
    , token : String
    , highlight : Bool
    }


type alias Step msg =
    Parser.Step
        ( State, Parsed msg, Queue )
        (List (Html msg))


type alias Queue =
    List ( Char, Location )


parse : InnerConfig msg -> Queue -> List (Html msg)
parse config chars =
    parseHelp config
        (if config.forExpression then
            ReadingExpression

         else
            Initial
        )
        []
        (List.sortBy
            (\{ range } -> ( range.start.row, range.start.column ))
            config.buttons
        )
        chars


parseHelp : InnerConfig msg -> State -> List (Parsed msg) -> List (Button msg) -> Queue -> List (Html msg)
parseHelp config state acc buttons queue =
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
                    , highlight =
                        Maybe.map (compareLocationRange location) config.highlight == Just EQ
                    }

                normal : State -> Step msg
                normal newState =
                    ( newState
                    , default
                    , tail
                    )
                        |> Parser.Loop

                operator : State -> Step msg
                operator newState =
                    colored colors.operator newState

                colored : String -> State -> Step msg
                colored color newState =
                    ( newState
                    , { default | color = Just color }
                    , tail
                    )
                        |> Parser.Loop

                error : () -> Step msg
                error () =
                    colored "red" Error

                seek : String -> State -> String -> Maybe (Step msg)
                seek color newState keyword =
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
                        ( newState
                        , { default
                            | color = Just color
                            , token = keyword
                          }
                        , List.drop len queue
                        )
                            |> Parser.Loop
                            |> Just

                    else
                        Nothing

                seekOrError : String -> State -> String -> Step msg
                seekOrError color newState keyword =
                    case seek color newState keyword of
                        Just r ->
                            r

                        Nothing ->
                            error ()

                step :
                    Parser.Step
                        ( State, Parsed msg, Queue )
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

                        ( _, ( '-', _ ) :: ( '-', _ ) :: commentTail ) ->
                            ( Comment state
                            , { default
                                | color = Just colors.comment
                                , token = "--"
                              }
                            , commentTail
                            )
                                |> Parser.Loop

                        ( Comment previous, ( '\n', _ ) :: _ ) ->
                            colored colors.comment previous

                        ( Comment _, _ ) ->
                            colored colors.comment state

                        ( Initial, ( '\n', _ ) :: _ ) ->
                            normal Initial

                        ( Initial, ( ' ', _ ) :: _ ) ->
                            normal Initial

                        ( Initial, _ ) ->
                            seekOrError colors.keyword GettingModuleName "module "

                        ( GettingModuleName, ( ' ', _ ) :: _ ) ->
                            normal WaitingModuleExposing

                        ( GettingModuleName, _ ) ->
                            normal GettingModuleName

                        ( WaitingModuleExposing, _ ) ->
                            seekOrError colors.keyword WaitingExposeList "exposing"

                        ( WaitingExposeList, ( ' ', _ ) :: _ ) ->
                            normal WaitingExposeList

                        ( WaitingExposeList, ( '(', _ ) :: _ ) ->
                            operator ReadingExposeList

                        ( ReadingExposeList, ( ')', _ ) :: _ ) ->
                            operator WaitingDeclaration

                        ( ReadingExposeList, _ ) ->
                            normal ReadingExposeList

                        ( _, ( '\n', _ ) :: _ ) ->
                            normal
                                (if config.forExpression then
                                    ReadingExpression

                                 else
                                    WaitingDeclaration
                                )

                        ( WaitingDeclaration, ( ' ', _ ) :: _ ) ->
                            normal ReadingExpression

                        ( WaitingDeclaration, _ ) ->
                            colored colors.declaration ReadingDeclaration

                        ( ReadingDeclaration, ( ' ', _ ) :: _ ) ->
                            normal ReadingExpression

                        ( ReadingDeclaration, _ ) ->
                            colored colors.declaration ReadingDeclaration

                        ( ReadingExpression, _ ) ->
                            keywords
                                |> List.Extra.findMap
                                    (seek colors.keyword ReadingExpression)
                                |> Maybe.Extra.withDefaultLazy
                                    (\_ ->
                                        if Unicode.isAlpha head then
                                            normal ReadingVariable

                                        else if Unicode.isDigit head then
                                            colored colors.number ReadingNumber

                                        else
                                            normal ReadingExpression
                                    )

                        ( ReadingVariable, ( ' ', _ ) :: _ ) ->
                            normal ReadingExpression

                        ( ReadingVariable, _ ) ->
                            normal ReadingVariable

                        ( ReadingNumber, ( ' ', _ ) :: _ ) ->
                            colored colors.number ReadingExpression

                        ( ReadingNumber, _ ) ->
                            colored colors.number ReadingNumber

                        _ ->
                            error ()
            in
            case step of
                Parser.Done r ->
                    r

                Parser.Loop ( newState, enqueue, effectiveTail ) ->
                    parseHelp config
                        newState
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
        |> List.MyExtra.groupBy .highlight
        |> List.concatMap
            (\( highlight, highlightGroup ) ->
                let
                    highlightContent : List (Html msg)
                    highlightContent =
                        highlightGroup
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
                                                                    , Maybe.map (\c -> style "outline" <| "1px solid " ++ c) background
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
                                                ([ Just <| style "outline" "1px solid #fff"
                                                 , Just <| style "background" "rgba(100 0 0 / 0.5)"
                                                 , Just <| style "cursor" "pointer"
                                                 , Maybe.map
                                                    (\msg ->
                                                        Html.Events.stopPropagationOn "click"
                                                            (Json.Decode.succeed ( msg, True ))
                                                    )
                                                    onPress
                                                 , Maybe.map title tooltip
                                                 ]
                                                    |> List.filterMap identity
                                                )
                                                content
                                            ]
                                )
                in
                if highlight then
                    [ span
                        [ style "background" colors.highlightBackground
                        , style "outline" <| "1px solid " ++ colors.highlightBackground
                        ]
                        highlightContent
                    ]

                else
                    highlightContent
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
            , "case"
            , "else"
            , "exposing"
            , "if"
            , "of"
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
    (language ++ core)
        |> List.sortBy (\keyword -> -(String.length keyword))


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
