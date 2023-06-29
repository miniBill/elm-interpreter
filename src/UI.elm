module UI exposing (Model, Msg, main)

import Browser
import Element exposing (Element, alignTop, column, el, fill, htmlAttribute, padding, paddingEach, paragraph, row, spacing, text, textColumn, width)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Lazy
import Elm.Parser
import Elm.Processing
import Elm.Syntax.Declaration as Declaration
import Elm.Syntax.Expression as Expression
import Elm.Syntax.File as File
import Elm.Syntax.Node as Node
import Elm.Syntax.Pattern as Pattern
import Eval
import Eval.Module
import Eval.Types as Types exposing (CallTree(..), Error)
import Hex
import Html
import Html.Attributes
import Json.Encode
import List.Extra
import Rope
import Syntax exposing (fakeNode)
import Value


type Msg
    = Input String
    | Eval Bool


type alias Model =
    { input : String
    , parsed : Maybe (Node.Node Expression.Expression)
    , output : Result String String
    , callTree : List CallTree
    , logLines : List String
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
        , Element.Lazy.lazy viewParsed model.parsed
        , let
            toRun : String
            toRun =
                if String.startsWith "module " model.input then
                    let
                        moduleName : Maybe String
                        moduleName =
                            model.input
                                |> String.split "\n"
                                |> List.head
                                |> Maybe.withDefault ""
                                |> String.split " "
                                |> List.drop 1
                                |> List.head
                    in
                    case moduleName of
                        Nothing ->
                            "main"

                        Just name ->
                            name ++ ".main"

                else
                    ""
          in
          row [ spacing 10 ]
            [ Input.button
                [ padding 10
                , Border.width 1
                ]
                { onPress = Just (Eval False)
                , label = text <| "Eval " ++ toRun
                }
            , Input.button
                [ padding 10
                , Border.width 1
                ]
                { onPress = Just (Eval True)
                , label = text <| "Trace " ++ toRun
                }
            ]
        , Element.Lazy.lazy viewOutput model.output
        , Element.Lazy.lazy viewCallTrees model.callTree
        , Element.Lazy.lazy viewLogLines model.logLines
        ]


viewParsed : Maybe (Node.Node Expression.Expression) -> Element Msg
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


viewExpression : Node.Node Expression.Expression -> Element msg
viewExpression (Node.Node _ expr) =
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

        Expression.RecordAccess chil (Node.Node _ name) ->
            boxxxy "." [ row [] [ viewExpression chil, text <| " " ++ name ] ]

        Expression.RecordUpdateExpression (Node.Node _ name) setters ->
            boxxxy ("{ " ++ name ++ " | }") [ viewSetters setters ]

        Expression.RecordExpr setters ->
            boxxxy "{}" [ viewSetters setters ]

        Expression.RecordAccessFunction name ->
            boxxxy0 name

        Expression.GLSLExpression code ->
            boxxxy "glsl" [ boxxxy0 code ]

        -- Expression.CaseExpression _ ->
        --     Debug.todo "branch 'CaseExpression _' not implemented"
        -- Expression.LambdaExpression _ ->
        --     Debug.todo "branch 'LambdaExpression _' not implemented"
        _ ->
            boxxxy0 <| Debug.toString expr


viewSetters : List (Node.Node Expression.RecordSetter) -> Element msg
viewSetters setters =
    row [] (List.map viewSetter setters)


viewSetter : Node.Node Expression.RecordSetter -> Element msg
viewSetter (Node.Node _ ( Node.Node _ name, value )) =
    boxxxy (name ++ " =") [ viewExpression value ]


boxxxy0 : String -> Element msg
boxxxy0 name =
    boxxxy name []


boxxxy : String -> List (Element msg) -> Element msg
boxxxy name children =
    boxxxy_ [ text name ] children


boxxxy_ : List (Element msg) -> List (Element msg) -> Element msg
boxxxy_ name children =
    column
        [ alignTop
        , Border.width 1
        , padding 10
        , spacing 10
        ]
        (row [] name :: children)


viewLetDeclaration : Node.Node Expression.LetDeclaration -> Element msg
viewLetDeclaration (Node.Node _ letDeclaration) =
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


viewPattern : Node.Node Pattern.Pattern -> Element msg
viewPattern (Node.Node _ pattern) =
    text <| Debug.toString pattern


viewExpressions : List (Node.Node Expression.Expression) -> Element msg
viewExpressions expressions =
    row [] <| List.map viewExpression expressions


viewOutput : Result String String -> Element Msg
viewOutput output =
    case output of
        Ok o ->
            paragraph
                [ Font.family [ Font.monospace ]
                , htmlAttribute <| Html.Attributes.style "max-width" "calc(100vw - 20px)"
                ]
                [ text o ]

        Err e ->
            e
                |> String.split "\n"
                |> List.map (\line -> paragraph [] [ text line ])
                |> textColumn [ Font.family [ Font.monospace ] ]


viewCallTrees : List CallTree -> Element Msg
viewCallTrees callTree =
    if List.isEmpty callTree then
        Element.none

    else
        column
            [ Font.family [ Font.monospace ]
            , spacing 10
            ]
            (List.map (viewCallTree 4) callTree)


viewCallTree : Int -> CallTree -> Element msg
viewCallTree budget (CallNode { expression, children, result }) =
    if budget <= 0 then
        Element.none

    else
        let
            nameRow : Element msg
            nameRow =
                row []
                    [ viewExpression <| fakeNode <| expression
                    , text (" = " ++ resultString)
                    ]

            resultString : String
            resultString =
                case result of
                    Ok v ->
                        Value.toString v

                    Err e ->
                        Types.evalErrorToString e

            childrenRows : List (Element msg)
            childrenRows =
                List.map (viewCallTree (budget - 1)) <| Rope.toList children
        in
        (nameRow :: childrenRows)
            |> column
                [ Border.widthEach
                    { top = 0
                    , left = 1
                    , right = 0
                    , bottom = 0
                    }
                , paddingEach
                    { top = 0
                    , bottom = 0
                    , left = 10
                    , right = 0
                    }
                , spacing 10
                ]


viewLogLines : List String -> Element msg
viewLogLines logLines =
    if List.isEmpty logLines then
        Element.none

    else
        Element.html <| Html.pre [] [ Html.text <| String.join "\n" logLines ]


init : Model
init =
    { input = """List.sum (List.range 0 3)"""
    , parsed = Nothing
    , output = Ok ""
    , callTree = []
    , logLines = []
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
                ( result, callTree, logLines ) =
                    if tracing then
                        if String.startsWith "module " model.input then
                            Eval.Module.trace model.input (Expression.FunctionOrValue [] "main")

                        else
                            Eval.trace model.input

                    else
                        ( if String.startsWith "module " model.input then
                            Eval.Module.eval model.input (Expression.FunctionOrValue [] "main")

                          else
                            Eval.eval model.input
                        , Rope.empty
                        , Rope.empty
                        )
            in
            { model
                | output = resultToString result
                , callTree = Rope.toList callTree
                , logLines = Rope.toList logLines
            }


tryParse : String -> Maybe (Node.Node Expression.Expression)
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
                    file : File.File
                    file =
                        Elm.Processing.process Elm.Processing.init rawFile
                in
                file.declarations
                    |> List.Extra.findMap (Node.value >> findMain)
            )


findMain : Declaration.Declaration -> Maybe (Node.Node Expression.Expression)
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


resultToString : Result Error Value.Value -> Result String String
resultToString result =
    case result of
        Err e ->
            Err <| Types.errorToString e

        Ok value ->
            Ok <| Value.toString value
