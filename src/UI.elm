module UI exposing (Model, Msg, main)

import Browser
import Core
import Element exposing (Element, alignTop, centerX, centerY, column, el, fill, height, padding, paddingEach, paragraph, px, rgb, row, text, textColumn, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Lazy
import Elm.Parser
import Elm.Processing
import Elm.Syntax.Declaration as Declaration
import Elm.Syntax.Expression as Expression
import Elm.Syntax.File as File
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern
import Elm.Syntax.Range exposing (Location, Range)
import Elm.Writer
import Eval
import Eval.Module
import Eval.Types as Types
import Hex
import Html exposing (Html)
import Html.Attributes
import Json.Encode
import List.Extra
import Rope
import Set exposing (Set)
import Syntax exposing (fakeNode)
import Types exposing (CallTree(..), Error, Value)
import UI.Theme as Theme
import Value


type Msg
    = Input String
    | Eval Bool
    | Open String
    | Close String


type alias Model =
    { input : String
    , parsed : Maybe (Node Expression.Expression)
    , output : Result String String
    , callTrees : List CallTree
    , logLines : List String
    , open : Set String
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
    Theme.column
        [ Theme.padding
        , width fill
        ]
        [ Theme.row [ width fill ]
            [ Input.multiline
                [ alignTop
                , width fill
                , Font.family [ Font.monospace ]
                ]
                { spellcheck = False
                , text = model.input
                , onChange = Input
                , label = Input.labelAbove [] <| text "Input"
                , placeholder = Nothing
                }
            , let
                moduleSource : String
                moduleSource =
                    if String.startsWith "module " model.input then
                        model.input

                    else
                        Eval.toModule model.input
              in
              viewSource Nothing moduleSource
            ]
        , Element.Lazy.lazy viewParsed model.parsed
        , let
            toRun : String
            toRun =
                if String.startsWith "module " model.input then
                    let
                        name : String
                        name =
                            model.input
                                |> String.split "\n"
                                |> List.head
                                |> Maybe.withDefault ""
                                |> String.split " "
                                |> List.drop 1
                                |> List.head
                                |> Maybe.withDefault ""
                    in
                    name ++ ".main"

                else
                    ""
          in
          Theme.row []
            [ Theme.button []
                { onPress = Just (Eval False)
                , label = text <| "Eval " ++ toRun
                }
            , Theme.button []
                { onPress = Just (Eval True)
                , label = text <| "Trace " ++ toRun
                }
            ]
        , Element.Lazy.lazy viewOutput model.output
        , model.callTrees
            |> List.indexedMap (viewCallTreeTop model.open)
            |> Theme.column [ width fill ]
        , Element.Lazy.lazy viewLogLines model.logLines
        ]


viewCallTreeTop : Set String -> Int -> CallTree -> Element Msg
viewCallTreeTop =
    Element.Lazy.lazy3 <|
        \open i tree ->
            el [ Border.width 1, Theme.padding, width fill ] <|
                viewCallTree [ i ] open tree


viewSource : Maybe Range -> String -> Element Msg
viewSource maybeHighlight source =
    let
        highlight : Range
        highlight =
            Maybe.withDefault fakeRange maybeHighlight

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

        viewRow : Int -> String -> Html msg
        viewRow rowIndex row =
            let
                slice : Int -> Int -> List (Html msg)
                slice from to =
                    syntax <| String.slice from to row

                toEnd : Int -> List (Html msg)
                toEnd from =
                    syntax <| String.dropLeft from row

                syntax : String -> List (Html msg)
                syntax fragment =
                    fragment
                        |> String.split " "
                        |> List.map viewToken
                        |> List.intersperse [ Html.text " " ]
                        |> List.concat

                keywords : Set String
                keywords =
                    -- TODO: finish this.
                    -- Or, even better, actually do syntax highlighting; possibly starting from the AST.
                    Set.fromList [ "module", "exposing", "=", "|>", "<|" ]

                colored : String -> String -> Html msg
                colored color content =
                    Html.span
                        [ Html.Attributes.style "color" color ]
                        [ Html.text content ]

                viewOperator : String -> Html msg
                viewOperator content =
                    colored "#cc4" content

                viewToken : String -> List (Html msg)
                viewToken token =
                    if Set.member token keywords then
                        [ colored "#88f" token ]

                    else if String.startsWith "(" token then
                        viewOperator "(" :: viewToken (String.dropLeft 1 token)

                    else if String.endsWith ")" token then
                        viewToken (String.dropRight 1 token) ++ [ viewOperator ")" ]

                    else if String.startsWith "\"" token && String.endsWith "\"" token then
                        [ colored "#c44" token ]

                    else if String.toFloat token /= Nothing then
                        [ colored "#cfc" token ]

                    else
                        [ Html.text token ]

                high : List (Html msg) -> List (Html msg)
                high child =
                    [ Html.span [ Html.Attributes.style "background" "#4c4" ] child ]

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
            Html.span [] <| List.concat pieces
    in
    source
        |> String.split "\n"
        |> List.indexedMap viewRow
        |> List.intersperse (Html.text "\n")
        |> Html.pre [ Html.Attributes.style "line-height" "125%" ]
        |> Element.html
        |> el
            [ width fill
            , alignTop
            , Theme.padding
            , Border.width 1
            , Background.color <| rgb 0.2 0.2 0.2
            , Font.color <| rgb 1 1 1
            ]


viewParsed : Maybe (Node Expression.Expression) -> Element Msg
viewParsed maybeExpr =
    case maybeExpr of
        Nothing ->
            Element.none

        Just expr ->
            el
                [ Font.family
                    [ Font.typeface "Fira Code"
                    , Font.monospace
                    ]
                ]
                (viewExpression expr)


viewExpression : Node Expression.Expression -> Element msg
viewExpression (Node _ expr) =
    case expr of
        Expression.OperatorApplication name _ l r ->
            boxxxy name [ viewExpressions [ l, r ] ]

        Expression.FunctionOrValue moduleName name ->
            boxxxy0 <| String.join "." (moduleName ++ [ name ])

        Expression.Application children ->
            boxxxy "Application" [ viewExpressions children ]

        Expression.Literal s ->
            boxxxy0 <| Json.Encode.encode 0 <| Json.Encode.string s

        Expression.Integer i ->
            boxxxy0 <| String.fromInt i

        Expression.Floatable f ->
            boxxxy0 <| String.fromFloat f

        Expression.CharLiteral c ->
            boxxxy0 <| "'" ++ String.fromChar c ++ "'"

        Expression.Hex i ->
            boxxxy0 <| "0x" ++ Hex.toString i

        Expression.LetExpression { declarations, expression } ->
            boxxxy "let/in"
                [ row [] <| List.map viewLetDeclaration declarations
                , viewExpression expression
                ]

        Expression.UnitExpr ->
            boxxxy0 "()"

        Expression.Negation expression ->
            boxxxy "-" [ viewExpression expression ]

        Expression.PrefixOperator name ->
            boxxxy0 <| "(" ++ name ++ ")"

        Expression.Operator name ->
            boxxxy0 <| "(" ++ name ++ ")"

        Expression.ParenthesizedExpression expression ->
            boxxxy "()" [ viewExpression expression ]

        Expression.IfBlock c t f ->
            boxxxy_ [ text "if ", viewExpression c, text " then" ]
                [ row []
                    [ viewExpression t
                    , el [ alignTop ] <| text " else "
                    , viewExpression f
                    ]
                ]

        Expression.TupledExpression children ->
            boxxxy
                (if List.length children == 2 then
                    "(,)"

                 else
                    "(,,)"
                )
                [ viewExpressions children ]

        Expression.ListExpr children ->
            boxxxy "[]" [ viewExpressions children ]

        Expression.RecordAccess chil (Node _ name) ->
            boxxxy "." [ row [] [ viewExpression chil, text <| " " ++ name ] ]

        Expression.RecordUpdateExpression (Node _ name) setters ->
            boxxxy ("{ " ++ name ++ " | }") [ viewSetters setters ]

        Expression.RecordExpr setters ->
            boxxxy "{}" [ viewSetters setters ]

        Expression.RecordAccessFunction name ->
            boxxxy0 name

        Expression.GLSLExpression code ->
            boxxxy "glsl" [ boxxxy0 code ]

        Expression.CaseExpression _ ->
            boxxxy0 "branch 'CaseExpression _' not implemented"

        Expression.LambdaExpression _ ->
            boxxxy0 "branch 'LambdaExpression _' not implemented"


viewSetters : List (Node Expression.RecordSetter) -> Element msg
viewSetters setters =
    row [] (List.map viewSetter setters)


viewSetter : Node Expression.RecordSetter -> Element msg
viewSetter (Node _ ( Node _ name, value )) =
    boxxxy (name ++ " =") [ viewExpression value ]


boxxxy0 : String -> Element msg
boxxxy0 name =
    boxxxy name []


boxxxy : String -> List (Element msg) -> Element msg
boxxxy name children =
    boxxxy_ [ text name ] children


boxxxy_ : List (Element msg) -> List (Element msg) -> Element msg
boxxxy_ name children =
    Theme.column
        [ alignTop
        , Border.width 1
        , padding Theme.rythm
        ]
        (row [] name :: children)


viewLetDeclaration : Node Expression.LetDeclaration -> Element msg
viewLetDeclaration (Node _ letDeclaration) =
    case letDeclaration of
        Expression.LetFunction function ->
            viewFunction function

        Expression.LetDestructuring pattern expression ->
            column []
                [ viewPattern pattern
                , viewExpression expression
                ]


viewFunction : Expression.Function -> Element msg
viewFunction function =
    let
        declaration : Expression.FunctionImplementation
        declaration =
            Node.value function.declaration
    in
    boxxxy_
        (text (Node.value declaration.name)
            :: List.map viewPattern declaration.arguments
        )
        [ viewExpression declaration.expression ]


viewPattern : Node Pattern.Pattern -> Element msg
viewPattern (Node _ pattern) =
    case pattern of
        Pattern.AllPattern ->
            boxxxy0 "_"

        Pattern.UnitPattern ->
            boxxxy0 "branch 'UnitPattern' not implemented"

        Pattern.CharPattern _ ->
            boxxxy0 "branch 'CharPattern _' not implemented"

        Pattern.StringPattern _ ->
            boxxxy0 "branch 'StringPattern _' not implemented"

        Pattern.IntPattern _ ->
            boxxxy0 "branch 'IntPattern _' not implemented"

        Pattern.HexPattern _ ->
            boxxxy0 "branch 'HexPattern _' not implemented"

        Pattern.FloatPattern _ ->
            boxxxy0 "branch 'FloatPattern _' not implemented"

        Pattern.TuplePattern _ ->
            boxxxy0 "branch 'TuplePattern _' not implemented"

        Pattern.RecordPattern _ ->
            boxxxy0 "branch 'RecordPattern _' not implemented"

        Pattern.UnConsPattern _ _ ->
            boxxxy0 "branch 'UnConsPattern _ _' not implemented"

        Pattern.ListPattern _ ->
            boxxxy0 "branch 'ListPattern _' not implemented"

        Pattern.VarPattern _ ->
            boxxxy0 "branch 'VarPattern _' not implemented"

        Pattern.NamedPattern _ _ ->
            boxxxy0 "branch 'NamedPattern _ _' not implemented"

        Pattern.AsPattern _ _ ->
            boxxxy0 "branch 'AsPattern _ _' not implemented"

        Pattern.ParenthesizedPattern _ ->
            boxxxy0 "branch 'ParenthesizedPattern _' not implemented"


viewExpressions : List (Node Expression.Expression) -> Element msg
viewExpressions expressions =
    row [] <| List.map viewExpression expressions


viewOutput : Result String String -> Element Msg
viewOutput output =
    case output of
        Ok o ->
            paragraph
                [ Font.family [ Font.monospace ]
                , Theme.style "max-width"
                    ("calc(100vw - " ++ String.fromInt (2 * Theme.rythm) ++ "px)")
                , Border.width 1
                , Theme.padding
                ]
                [ text <| "Result: " ++ o ]

        Err e ->
            e
                |> String.split "\n"
                |> List.map (\line -> paragraph [] [ text line ])
                |> (::) (text "Error:")
                |> textColumn
                    [ Font.family [ Font.monospace ]
                    , Border.width 1
                    , Theme.padding
                    ]


viewCallTree : List Int -> Set String -> CallTree -> Element Msg
viewCallTree currentList open (CallNode { expression, children, result }) =
    let
        current : String
        current =
            String.join "." (List.map String.fromInt currentList)

        expressionString : String
        expressionString =
            expression
                |> fakeNode
                |> Elm.Writer.writeExpression
                |> Elm.Writer.write

        nameRow : Element msg
        nameRow =
            text
                (String.trim expressionString
                    ++ (if String.contains "\n" expressionString then
                            "\n= "

                        else
                            " = "
                       )
                    ++ resultString
                )

        resultString : String
        resultString =
            case result of
                Ok v ->
                    Value.toString v

                Err e ->
                    Types.evalErrorToString e

        toggleButton : Element Msg
        toggleButton =
            if Rope.length children < 2 then
                Element.none

            else
                Theme.button
                    [ height <| px <| 4 * Theme.rythm
                    , width <| px <| 4 * Theme.rythm
                    , centerX
                    ]
                    { label =
                        el [ centerX, centerY ] <|
                            text <|
                                if Set.member current open then
                                    "∧"

                                else
                                    "∨"
                    , onPress =
                        Just <|
                            if Set.member current open then
                                Close current

                            else
                                Open current
                    }

        shownChildren : List (Element Msg)
        shownChildren =
            if Set.member current open || Rope.length children == 1 then
                Rope.toList children
                    |> List.sortBy (\node -> -(nodeSize node))
                    |> List.indexedMap
                        (\i -> viewCallTree (i :: currentList) open)

            else
                []
    in
    Theme.column [ alignTop, width fill ]
        [ nameRow
        , toggleButton
        , shownChildren
            |> List.intersperse
                (el
                    [ height fill
                    , Border.widthEach { left = 1, top = 0, bottom = 0, right = 0 }
                    , width <| px 1
                    ]
                    Element.none
                )
            |> (\l ->
                    l
                        ++ [ el
                                [ Background.color <| rgb 1 1 1
                                , width fill
                                , height <| px 1
                                ]
                             <|
                                text " "
                           ]
               )
            |> Theme.row
                [ Border.widthEach
                    { top = 1
                    , left = 0
                    , right = 0
                    , bottom = 0
                    }
                , paddingEach
                    { top = Theme.rythm
                    , bottom = 0
                    , left = 0
                    , right = 0
                    }
                , width fill
                ]
        ]


nodeSize : CallTree -> number
nodeSize (CallNode { children }) =
    List.foldl (\child acc -> acc + nodeSize child) 1 (Rope.toList children)


viewLogLines : List String -> Element msg
viewLogLines logLines =
    if List.isEmpty logLines then
        Element.none

    else
        Element.html <| Html.pre [] [ Html.text <| String.join "\n" logLines ]


init : Model
init =
    let
        input : String
        input =
            """List.sum (List.range 0 3)"""
    in
    { input = input
    , parsed = tryParse input
    , output = Ok ""
    , callTrees = []
    , logLines = []
    , open = Set.empty
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Input input ->
            { model
                | input = input
                , parsed = tryParse input
            }

        Eval tracing ->
            let
                moduleSource : String
                moduleSource =
                    if String.startsWith "module " model.input then
                        model.input

                    else
                        Eval.toModule model.input

                ( result, callTree, logLines ) =
                    Eval.Module.traceOrEvalModule
                        { trace = tracing }
                        moduleSource
                        (Expression.FunctionOrValue [] "main")
            in
            { model
                | output = resultToString result
                , callTrees = Rope.toList callTree
                , logLines = Rope.toList logLines
            }

        Open path ->
            { model | open = Set.insert path model.open }

        Close path ->
            { model | open = Set.remove path model.open }


tryParse : String -> Maybe (Node Expression.Expression)
tryParse input =
    let
        fixedInput : String
        fixedInput =
            if String.startsWith "module" input then
                input

            else
                Eval.toModule input
    in
    fixedInput
        |> Elm.Parser.parse
        |> Result.toMaybe
        |> Maybe.andThen
            (\rawFile ->
                let
                    context : Elm.Processing.ProcessContext
                    context =
                        Elm.Processing.init
                            |> Elm.Processing.addDependency Core.dependency

                    file : File.File
                    file =
                        Elm.Processing.process context rawFile
                in
                file.declarations
                    |> List.Extra.findMap (Node.value >> findMain)
            )


findMain : Declaration.Declaration -> Maybe (Node Expression.Expression)
findMain declaration =
    case declaration of
        Declaration.FunctionDeclaration function ->
            let
                implementation : Expression.FunctionImplementation
                implementation =
                    Node.value function.declaration
            in
            if Node.value implementation.name == "main" then
                Just implementation.expression

            else
                Nothing

        _ ->
            Nothing


resultToString : Result Error Value -> Result String String
resultToString result =
    case result of
        Err e ->
            Err <| Types.errorToString e

        Ok value ->
            Ok <| Value.toString value
