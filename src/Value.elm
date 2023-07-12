module Value exposing (fromOrder, nameError, toArray, toOrder, toString, todo, typeError, unsupported)

import Array exposing (Array)
import Elm.CodeGen
import Elm.Pretty
import Elm.RawFile exposing (moduleName)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))
import FastDict as Dict
import Pretty
import Syntax
import Types exposing (Env, EvalErrorData, EvalErrorKind(..), Value(..))


typeError : Env -> String -> EvalErrorData
typeError env msg =
    error env (TypeError msg)


nameError : Env -> String -> EvalErrorData
nameError env msg =
    error env (NameError msg)


unsupported : Env -> String -> EvalErrorData
unsupported env msg =
    error env (Unsupported msg)


todo : Env -> String -> EvalErrorData
todo env msg =
    error env (Todo msg)


error : Env -> EvalErrorKind -> EvalErrorData
error env msg =
    { callStack = env.callStack
    , error = msg
    }


toArray : Value -> Maybe (List Value)
toArray value =
    case value of
        Custom name [ _, _, JsArray tree, JsArray tailArray ] ->
            case ( name.moduleName, name.name ) of
                ( [ "Array" ], "Array_elm_builtin" ) ->
                    let
                        treeToArray : Array Value -> List Value
                        treeToArray arr =
                            List.concatMap nodeToList (Array.toList arr)

                        nodeToList : Value -> List Value
                        nodeToList node =
                            case node of
                                Custom qualifiedName [ JsArray arr ] ->
                                    case qualifiedName.name of
                                        "SubTree" ->
                                            treeToArray arr

                                        "Leaf" ->
                                            Array.toList arr

                                        _ ->
                                            []

                                _ ->
                                    []
                    in
                    Just (treeToArray tree ++ Array.toList tailArray)

                _ ->
                    Nothing

        _ ->
            Nothing


toString : Value -> String
toString value =
    toPretty value
        |> Elm.Pretty.prettyExpression
        |> Pretty.pretty 120


toPretty : Value -> Elm.CodeGen.Expression
toPretty expr =
    case expr of
        Int i ->
            Elm.CodeGen.int i

        String s ->
            Elm.CodeGen.string s

        Float f ->
            Elm.CodeGen.float f

        Char c ->
            Elm.CodeGen.char c

        Bool b ->
            Elm.CodeGen.val (boolToString b)

        Unit ->
            Elm.CodeGen.unit

        Tuple children ->
            Elm.CodeGen.tuple (List.map toPretty children)

        Record fields ->
            Elm.CodeGen.record (List.map (Tuple.mapSecond toPretty) (Dict.toList fields))

        Custom name args ->
            Elm.CodeGen.fqConstruct name.moduleName name.name (List.map toPretty args)

        List children ->
            Elm.CodeGen.list (List.map toPretty children)

        JsArray arr ->
            Elm.CodeGen.apply
                [ Elm.CodeGen.fqFun [ "Array" ] "fromList"
                , Elm.CodeGen.list <|
                    List.map toPretty <|
                        Array.toList arr
                ]

        Lambda _ arg implementation ->
            Elm.CodeGen.lambda
                [ patternToPretty (Syntax.fakeNode arg) ]
                (expressionToPretty implementation)


expressionToPretty : Node Expression -> Elm.CodeGen.Expression
expressionToPretty (Node _ expr) =
    case expr of
        Expression.UnitExpr ->
            Elm.CodeGen.unit

        Expression.Application cs ->
            Elm.CodeGen.apply (List.map expressionToPretty cs)

        Expression.OperatorApplication name _ l r ->
            Elm.CodeGen.applyBinOp (expressionToPretty l) (nameToOperator name) (expressionToPretty r)

        Expression.FunctionOrValue moduleName name ->
            Elm.CodeGen.fqVal moduleName name

        Expression.IfBlock c t f ->
            Elm.CodeGen.ifExpr
                (expressionToPretty c)
                (expressionToPretty t)
                (expressionToPretty f)

        Expression.PrefixOperator c ->
            Elm.CodeGen.binOp (nameToOperator c)

        Expression.Operator c ->
            Elm.CodeGen.binOp (nameToOperator c)

        Expression.Integer c ->
            Elm.CodeGen.int c

        Expression.Hex c ->
            Elm.CodeGen.hex c

        Expression.Floatable c ->
            Elm.CodeGen.float c

        Expression.Negation c ->
            Elm.CodeGen.negate (expressionToPretty c)

        Expression.Literal c ->
            Elm.CodeGen.string c

        Expression.CharLiteral c ->
            Elm.CodeGen.char c

        Expression.TupledExpression cs ->
            Elm.CodeGen.tuple (List.map expressionToPretty cs)

        Expression.ParenthesizedExpression c ->
            Elm.CodeGen.parens (expressionToPretty c)

        Expression.LetExpression letBlock ->
            letBlockToPretty letBlock

        Expression.CaseExpression caseBlock ->
            caseBlockToPretty caseBlock

        Expression.LambdaExpression lambdaExpression ->
            lambdaExpressionToPretty lambdaExpression

        Expression.RecordExpr record ->
            Elm.CodeGen.record (List.map recordSetterToPretty record)

        Expression.ListExpr cs ->
            Elm.CodeGen.list (List.map expressionToPretty cs)

        Expression.RecordAccess c (Node _ field) ->
            Elm.CodeGen.access (expressionToPretty c) field

        Expression.RecordAccessFunction _ ->
            Debug.todo "branch 'RecordAccessFunction _' not implemented"

        Expression.RecordUpdateExpression _ _ ->
            Debug.todo "branch 'RecordUpdateExpression _ _' not implemented"

        Expression.GLSLExpression code ->
            Elm.CodeGen.glsl code


recordSetterToPretty : Node Expression.RecordSetter -> ( String, Expression )
recordSetterToPretty (Node _ ( Node _ name, expr )) =
    ( name, expressionToPretty expr )


lambdaExpressionToPretty : Expression.Lambda -> Expression
lambdaExpressionToPretty _ =
    Debug.todo "TODO"


caseBlockToPretty : Expression.CaseBlock -> Expression
caseBlockToPretty _ =
    Debug.todo "TODO"


letBlockToPretty : Expression.LetBlock -> Expression
letBlockToPretty _ =
    Debug.todo "TODO"


nameToOperator : String -> Elm.CodeGen.BinOp
nameToOperator name =
    case name of
        ">>" ->
            Elm.CodeGen.composer

        "<<" ->
            Elm.CodeGen.composel

        "^" ->
            Elm.CodeGen.power

        "*" ->
            Elm.CodeGen.mult

        "/" ->
            Elm.CodeGen.div

        "//" ->
            Elm.CodeGen.intDiv

        "%" ->
            Elm.CodeGen.modulo

        "+" ->
            Elm.CodeGen.plus

        "-" ->
            Elm.CodeGen.minus

        "++" ->
            Elm.CodeGen.append

        "::" ->
            Elm.CodeGen.cons

        "==" ->
            Elm.CodeGen.equals

        "/=" ->
            Elm.CodeGen.notEqual

        "<" ->
            Elm.CodeGen.lt

        ">" ->
            Elm.CodeGen.gt

        "<=" ->
            Elm.CodeGen.lte

        ">=" ->
            Elm.CodeGen.gte

        "&&" ->
            Elm.CodeGen.and

        "||" ->
            Elm.CodeGen.or

        "|>" ->
            Elm.CodeGen.piper

        "<|" ->
            Elm.CodeGen.pipel

        _ ->
            Debug.todo "handle errors"


patternToPretty : Node Pattern -> Elm.CodeGen.Pattern
patternToPretty (Node _ pattern) =
    case pattern of
        AllPattern ->
            Elm.CodeGen.allPattern

        UnitPattern ->
            Elm.CodeGen.unitPattern

        CharPattern c ->
            Elm.CodeGen.charPattern c

        StringPattern c ->
            Elm.CodeGen.stringPattern c

        IntPattern c ->
            Elm.CodeGen.intPattern c

        HexPattern c ->
            Elm.CodeGen.hexPattern c

        FloatPattern c ->
            Elm.CodeGen.floatPattern c

        TuplePattern c ->
            Elm.CodeGen.tuplePattern (List.map patternToPretty c)

        RecordPattern c ->
            Elm.CodeGen.recordPattern (List.map Node.value c)

        UnConsPattern h t ->
            Elm.CodeGen.unConsPattern (patternToPretty h) (patternToPretty t)

        ListPattern c ->
            Elm.CodeGen.listPattern (List.map patternToPretty c)

        VarPattern c ->
            Elm.CodeGen.varPattern c

        NamedPattern name children ->
            Elm.CodeGen.fqNamedPattern name.moduleName name.name (List.map patternToPretty children)

        AsPattern child (Node _ name) ->
            Elm.CodeGen.asPattern (patternToPretty child) name

        ParenthesizedPattern c ->
            Elm.CodeGen.parensPattern (patternToPretty c)


boolToString : Bool -> String
boolToString b =
    if b then
        "True"

    else
        "False"


fromOrder : Order -> Value
fromOrder order =
    case order of
        LT ->
            Custom { moduleName = [ "Basics" ], name = "LT" } []

        EQ ->
            Custom { moduleName = [ "Basics" ], name = "EQ" } []

        GT ->
            Custom { moduleName = [ "Basics" ], name = "GT" } []


toOrder : Value -> Maybe Order
toOrder value =
    case value of
        Custom { moduleName, name } [] ->
            case ( moduleName, name ) of
                ( [ "Basics" ], "LT" ) ->
                    Just LT

                ( [ "Basics" ], "EQ" ) ->
                    Just EQ

                ( [ "Basics" ], "GT" ) ->
                    Just GT

                _ ->
                    Nothing

        _ ->
            Nothing
