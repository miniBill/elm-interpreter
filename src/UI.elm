module UI exposing (CallTreeZipper, Model, Msg, main)

import Browser
import Core
import Element exposing (Attribute, Element, alignRight, alignTop, column, el, fill, padding, paragraph, row, shrink, text, width)
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
        [ Theme.wrappedRow [ width fill ]
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
            , Element.Lazy.lazy viewParsed model.parsed
            , Theme.box "Source:"
                [ width fill ]
                [ Source.view [] Nothing moduleSource ]
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
            , Element.Lazy.lazy viewOutput model.output
            , viewCallTrees model.callTrees
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

        sourceView : Element msg
        sourceView =
            if env.currentModule == [ "Main" ] then
                Source.view [ alignTop ] (Just <| Node.range expression) source

            else
                Source.view [ alignTop ] Nothing "-- No source available"

        parentButtons : List (Element Msg)
        parentButtons =
            let
                go : Maybe CallTreeZipper -> List (Element Msg)
                go node =
                    case node of
                        Nothing ->
                            []

                        Just (CallTreeZipper z) ->
                            focusButton [] z :: go z.parent
            in
            go parent
                |> List.reverse

        shownChildren : List (Element Msg)
        shownChildren =
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
            parentButtons
        , Theme.row [ width fill ]
            [ Theme.box "Source"
                [ width fill ]
                [ sourceView ]
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
                shownChildren
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
                  , view = \( _, value ) -> cell <| viewValue value
                  , width = shrink
                  }
                ]
            , data = FastDict.toList values
            }


viewValue : Value -> String
viewValue val =
    val
        |> Value.toExpression
        |> expressionToString


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
    text
        (String.trim expressionString
            ++ (if String.contains "\n" expressionString then
                    "\n--> "

                else
                    " --> "
               )
            ++ resultString
        )


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
    , focus = Nothing
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
