module UI exposing (CallTreeZipper, Model, Msg, main)

import Browser
import Core
import Element exposing (Attribute, Element, alignRight, alignTop, column, el, fill, height, paragraph, px, row, shrink, text, width)
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
import Elm.Writer
import Eval
import Eval.Module
import Eval.Types as Types
import FastDict
import Hex
import Html
import Json.Encode
import List.Extra
import Rope
import Types exposing (CallTree(..), Error, Value)
import UI.Source as Source
import UI.Theme as Theme
import Value


type Msg
    = Input String
    | Eval Bool
    | Focus
        { parent : Maybe CallTreeZipper
        , current : CallTree
        }


type alias Model =
    { input : String
    , parsed : Maybe (Node Expression.Expression)
    , output : Result String String
    , callTrees : List CallTree
    , logLines : List String
    , focus : Maybe CallTreeZipper
    }


type CallTreeZipper
    = CallTreeZipper
        { parent : Maybe CallTreeZipper
        , current : CallTree
        }


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = \model -> Element.layout [] (innerView model)
        , update = update
        }


init : Model
init =
    reinit """List.sum (List.range 0 3)"""


reinit : String -> Model
reinit input =
    { input = input
    , parsed = tryParse input
    , output = Ok ""
    , callTrees = []
    , logLines = []
    , focus = Nothing
    }


innerView : Model -> Element Msg
innerView model =
    let
        moduleSource : String
        moduleSource =
            if String.startsWith "module " model.input then
                model.input

            else
                Eval.toModule model.input
    in
    Theme.column
        [ Theme.padding
        , width fill
        ]
        [ Theme.box "Input:"
            [ width fill ]
            [ Input.multiline
                [ width fill
                , monospace
                ]
                { spellcheck = False
                , text = model.input
                , onChange = Input
                , label = Input.labelHidden "Input"
                , placeholder = Nothing
                }
            ]
        , Theme.wrappedRow [ width fill ]
            [ Element.Lazy.lazy viewParsed model.parsed
            , if moduleSource == model.input then
                Element.none

              else
                Theme.box "Full source:"
                    [ width fill ]
                    [ Source.view []
                        { highlight = Nothing
                        , buttons = []
                        , source = moduleSource
                        }
                    ]
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
              Theme.box "Commands:" [] <|
                [ Theme.button []
                    { onPress = Just (Eval False)
                    , label = text <| "Eval " ++ toRun
                    }
                , Theme.button []
                    { onPress = Just (Eval True)
                    , label = text <| "Trace " ++ toRun
                    }
                ]
            , Theme.column [ alignTop ]
                [ Element.Lazy.lazy viewOutput model.output
                , viewCallTrees model.callTrees
                ]
            ]
        , model.focus
            |> Maybe.map (viewCallTree moduleSource)
            |> Maybe.withDefault Element.none
        , Element.Lazy.lazy viewLogLines model.logLines
        ]


viewCallTrees : List CallTree -> Element Msg
viewCallTrees =
    Element.Lazy.lazy <|
        \callTrees ->
            if List.isEmpty callTrees then
                Element.none

            else
                Theme.box "Call trees:"
                    []
                    [ callTrees
                        |> List.map
                            (\callTree ->
                                focusButton []
                                    { parent = Nothing
                                    , current = callTree
                                    }
                            )
                        |> Theme.wrappedRow [ width fill ]
                    ]


focusButton :
    List (Attribute Msg)
    ->
        { parent : Maybe CallTreeZipper
        , current : CallTree
        }
    -> Element Msg
focusButton attrs zipper =
    Theme.button (monospace :: attrs)
        { label = viewNode zipper.current
        , onPress = Just <| Focus zipper
        }


viewParsed : Maybe (Node Expression.Expression) -> Element Msg
viewParsed maybeExpr =
    case maybeExpr of
        Nothing ->
            Element.none

        Just expr ->
            Theme.box "Parsed as:" [] <|
                [ el [ monospace ] <|
                    viewExpression expr
                ]


monospace : Attribute msg
monospace =
    Font.family
        [ Font.typeface "Fira Code"
        , Font.monospace
        ]


viewExpression : Node Expression.Expression -> Element msg
viewExpression (Node _ expr) =
    case expr of
        Expression.OperatorApplication name _ l r ->
            boxxxy_ name [ viewExpressions [ l, r ] ]

        Expression.FunctionOrValue moduleName name ->
            boxxxy0 <| String.join "." (moduleName ++ [ name ])

        Expression.Application children ->
            boxxxy_ "Application" [ viewExpressions children ]

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
            boxxxy_ "let/in"
                [ row [] <| List.map viewLetDeclaration declarations
                , viewExpression expression
                ]

        Expression.UnitExpr ->
            boxxxy0 "()"

        Expression.Negation expression ->
            boxxxy_ "-" [ viewExpression expression ]

        Expression.PrefixOperator name ->
            boxxxy0 <| "(" ++ name ++ ")"

        Expression.Operator name ->
            boxxxy0 <| "(" ++ name ++ ")"

        Expression.ParenthesizedExpression expression ->
            boxxxy_ "()" [ viewExpression expression ]

        Expression.IfBlock c t f ->
            boxxxy [ text "if ", viewExpression c, text " then" ]
                [ row []
                    [ viewExpression t
                    , el [ alignTop ] <| text " else "
                    , viewExpression f
                    ]
                ]

        Expression.TupledExpression children ->
            boxxxy_
                (if List.length children == 2 then
                    "(,)"

                 else
                    "(,,)"
                )
                [ viewExpressions children ]

        Expression.ListExpr children ->
            boxxxy_ "[]" [ viewExpressions children ]

        Expression.RecordAccess chil (Node _ name) ->
            boxxxy_ "." [ row [] [ viewExpression chil, text <| " " ++ name ] ]

        Expression.RecordUpdateExpression (Node _ name) setters ->
            boxxxy_ ("{ " ++ name ++ " | }") [ viewSetters setters ]

        Expression.RecordExpr setters ->
            boxxxy_ "{}" [ viewSetters setters ]

        Expression.RecordAccessFunction name ->
            boxxxy0 name

        Expression.GLSLExpression code ->
            boxxxy_ "glsl" [ boxxxy0 code ]

        Expression.CaseExpression _ ->
            boxxxy0 "branch 'CaseExpression _' not implemented"

        Expression.LambdaExpression _ ->
            boxxxy0 "branch 'LambdaExpression _' not implemented"


viewSetters : List (Node Expression.RecordSetter) -> Element msg
viewSetters setters =
    row [] (List.map viewSetter setters)


viewSetter : Node Expression.RecordSetter -> Element msg
viewSetter (Node _ ( Node _ name, value )) =
    boxxxy_ (name ++ " =") [ viewExpression value ]


boxxxy0 : String -> Element msg
boxxxy0 name =
    el
        [ alignTop
        , Border.width 1
        , Theme.padding
        ]
        (text name)


boxxxy_ : String -> List (Element msg) -> Element msg
boxxxy_ name children =
    boxxxy [ text name ] children


boxxxy : List (Element msg) -> List (Element msg) -> Element msg
boxxxy name children =
    Theme.column
        [ alignTop
        , Border.width 1
        , Theme.padding
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
    boxxxy
        (text (Node.value declaration.name)
            :: List.map viewPattern declaration.arguments
        )
        [ viewExpression declaration.expression ]


viewPattern : Node Pattern.Pattern -> Element msg
viewPattern (Node _ pattern) =
    let
        commaJoined : String -> (a -> Element msg) -> List a -> String -> Element msg
        commaJoined before viewChild children after =
            if List.isEmpty children then
                boxxxy0 <| before ++ after

            else
                boxxxy
                    (text (before ++ " ")
                        :: List.intersperse
                            (text ", ")
                            (List.map viewChild children)
                        ++ [ text <| " " ++ after ]
                    )
                    []
    in
    case pattern of
        Pattern.AllPattern ->
            boxxxy0 "_"

        Pattern.UnitPattern ->
            boxxxy0 "()"

        Pattern.CharPattern '\'' ->
            boxxxy0 "'\\''"

        Pattern.CharPattern c ->
            boxxxy0 <| "'" ++ String.fromChar c ++ "'"

        Pattern.StringPattern _ ->
            boxxxy0 "branch 'StringPattern _' not implemented"

        Pattern.IntPattern i ->
            boxxxy0 <| String.fromInt i

        Pattern.HexPattern h ->
            boxxxy0 <| "0x" ++ Hex.toString h

        Pattern.FloatPattern f ->
            boxxxy0 <| String.fromFloat f

        Pattern.ParenthesizedPattern child ->
            commaJoined "(" viewPattern [ child ] ")"

        Pattern.TuplePattern children ->
            commaJoined "(" viewPattern children ")"

        Pattern.ListPattern children ->
            commaJoined "[" viewPattern children "]"

        Pattern.RecordPattern children ->
            commaJoined "{" (Node.value >> text) children "}"

        Pattern.UnConsPattern _ _ ->
            boxxxy0 "branch 'UnConsPattern _ _' not implemented"

        Pattern.VarPattern v ->
            boxxxy0 v

        Pattern.NamedPattern _ _ ->
            boxxxy0 "branch 'NamedPattern _ _' not implemented"

        Pattern.AsPattern _ _ ->
            boxxxy0 "branch 'AsPattern _ _' not implemented"


viewExpressions : List (Node Expression.Expression) -> Element msg
viewExpressions expressions =
    row [] <| List.map viewExpression expressions


viewOutput : Result String String -> Element Msg
viewOutput output =
    case output of
        Ok "" ->
            Element.none

        Ok o ->
            paragraph
                [ Theme.style "max-width"
                    ("calc(100vw - " ++ String.fromInt (2 * Theme.rythm) ++ "px)")
                , Border.width 1
                , Theme.padding
                , alignTop
                ]
                [ el [ Font.bold ] <| text "Result: "
                , el [ monospace ] <| text o
                ]

        Err e ->
            e
                |> String.split "\n"
                |> List.map (\line -> paragraph [] [ text line ])
                |> Theme.box "Error:" []


viewCallTree : String -> CallTreeZipper -> Element Msg
viewCallTree source ((CallTreeZipper { current, parent }) as zipper) =
    let
        (CallNode { expression, children, env }) =
            current

        sourceViewConfig : Source.Config Msg
        sourceViewConfig =
            if env.currentModule == [ "Main" ] then
                { highlight = Just <| Node.range expression
                , buttons =
                    Rope.toList children
                        |> List.map
                            (\((CallNode childData) as child) ->
                                { range = Node.range childData.expression
                                , onPress =
                                    Focus
                                        { current = child
                                        , parent = Just zipper
                                        }
                                        |> Just
                                , tooltip =
                                    case childData.result of
                                        Err _ ->
                                            Nothing

                                        Ok value ->
                                            Just (Value.toString value)
                                }
                            )
                , source = source
                }

            else
                { highlight =
                    Just
                        { start = { row = 6, column = 3 }
                        , end =
                            { row = round (1 / 0)
                            , column = 0
                            }
                        }
                , buttons = []
                , source =
                    "-- No full source available\n\nmodule "
                        ++ String.join "." env.currentModule
                        ++ " exposing (..)\n\n"
                        ++ "currentExpr =\n"
                        ++ Eval.indent 2 (String.trim <| expressionToString expression)
                }

        parentButtons : List (Element Msg)
        parentButtons =
            let
                go : Maybe CallTreeZipper -> List (Element Msg)
                go node =
                    case node of
                        Nothing ->
                            []

                        Just (CallTreeZipper z) ->
                            focusButton [ alignTop ] z :: go z.parent
            in
            go parent
                |> List.reverse

        childrenButtons : List (Element Msg)
        childrenButtons =
            Rope.toList children
                |> List.sortBy (\node -> -(nodeSize node))
                |> List.map
                    (\child ->
                        focusButton [ alignTop ]
                            { current = child
                            , parent = Just zipper
                            }
                    )
    in
    Theme.box "Call tree:"
        [ width fill ]
        [ Theme.box "Parents:"
            [ width fill ]
            [ Theme.wrappedRow [ width fill ] parentButtons ]
        , Theme.row [ width fill ]
            [ Theme.box "Source"
                [ width fill ]
                [ Source.view [ alignTop ] sourceViewConfig ]
            , Theme.box "Environment"
                [ alignTop
                , alignRight
                ]
                [ viewEnv env ]
            ]
        , Theme.box "Children:"
            [ width fill ]
          <|
            [ Theme.wrappedRow
                [ width fill ]
                childrenButtons
            ]
        ]


viewEnv : Types.Env -> Element msg
viewEnv { values } =
    if FastDict.isEmpty values then
        Element.none

    else
        let
            cell : String -> Element msg
            cell value =
                el [ monospace ] <| text value
        in
        Element.table
            [ Theme.spacing
            , alignTop
            , alignRight
            , width shrink
            ]
            { columns =
                [ { header = text "Name"
                  , view = \( name, _ ) -> cell name
                  , width = shrink
                  }
                , { header = text "Value"
                  , view = \( _, value ) -> cell <| Value.toString value
                  , width = shrink
                  }
                ]
            , data = FastDict.toList values
            }


viewNode : CallTree -> Element msg
viewNode (CallNode { expression, result }) =
    let
        expressionString : String
        expressionString =
            expressionToString expression

        resultString : String
        resultString =
            case result of
                Ok v ->
                    Value.toString v

                Err e ->
                    Types.evalErrorToString e
    in
    Theme.row []
        [ text <| String.trim expressionString
        , el
            [ width <| px Theme.rythm
            , height fill
            , Border.width 1
            , Border.roundEach
                { topLeft = 0
                , bottomLeft = 0
                , topRight = 999
                , bottomRight = 999
                }
            ]
            Element.none
        , text resultString
        ]


expressionToString : Node Expression.Expression -> String
expressionToString expression =
    expression
        |> Elm.Writer.writeExpression
        |> Elm.Writer.write


nodeSize : CallTree -> number
nodeSize (CallNode { children }) =
    List.foldl (\child acc -> acc + nodeSize child) 1 (Rope.toList children)


viewLogLines : List String -> Element msg
viewLogLines logLines =
    if List.isEmpty logLines then
        Element.none

    else
        Element.html <| Html.pre [] [ Html.text <| String.join "\n" logLines ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        Input input ->
            reinit input

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

                callTrees : List CallTree
                callTrees =
                    Rope.toList callTree
            in
            { model
                | output = resultToString result
                , callTrees = callTrees
                , logLines = Rope.toList logLines
                , focus =
                    case callTrees of
                        [ tree ] ->
                            CallTreeZipper
                                { parent = Nothing
                                , current = tree
                                }
                                |> Just

                        _ ->
                            Nothing
            }

        Focus focus ->
            { model | focus = Just (CallTreeZipper focus) }


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
