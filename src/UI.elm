module UI exposing (Model, Msg, main)

import Browser
import Core
import Element exposing (Element, alignTop, column, el, fill, height, htmlAttribute, padding, paddingEach, paragraph, px, row, text, textColumn, width)
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
import Hex
import Html
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
        [ Theme.padding ]
        [ Theme.row []
            [ Input.multiline
                [ width fill
                , Font.family [ Font.monospace ]
                ]
                { spellcheck = False
                , text = model.input
                , onChange = Input
                , label = Input.labelAbove [] <| text "Input"
                , placeholder = Nothing
                }
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
            |> List.indexedMap (\i tree -> Element.Lazy.lazy3 viewCallTree [ i ] model.open tree)
            |> Theme.column []
        , Element.Lazy.lazy viewLogLines model.logLines
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
                , htmlAttribute <|
                    Html.Attributes.style "max-width"
                        ("calc(100vw - " ++ String.fromInt (2 * Theme.rythm) ++ "px)")
                ]
                [ text o ]

        Err e ->
            e
                |> String.split "\n"
                |> List.map (\line -> paragraph [] [ text line ])
                |> textColumn [ Font.family [ Font.monospace ] ]


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
            Theme.button
                [ height <| px <| 4 * Theme.rythm
                , width <| px <| 4 * Theme.rythm
                ]
                { label = text " "
                , onPress =
                    Just <|
                        if Set.member current open then
                            Close current

                        else
                            Open current
                }
    in
    nameRow
        :: (if Set.member current open then
                List.indexedMap (\i -> viewCallTree (i :: currentList) open) <| Rope.toList children

            else
                []
           )
        |> Theme.column
            [ Border.widthEach
                { top = 0
                , left = 1
                , right = 0
                , bottom = 0
                }
            , paddingEach
                { top = 0
                , bottom = 0
                , left = Theme.rythm
                , right = 0
                }
            ]
        |> List.singleton
        |> (::) toggleButton
        |> Theme.row []


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
