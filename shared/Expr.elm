module Expr exposing
    ( val, case_, letIn, lambda
    , toString
    , fromOrder, toOrder
    , list, record, recordUpdate, toArray, tuple, valueToString
    )

{-|

@docs val, case_, letIn, lambda

@docs toString

@docs fromOrder, toOrder

-}

import Array exposing (Array)
import Elm.CodeGen as Elm exposing (Expression)
import Elm.Pretty
import Environment
import FastDict as Dict
import Pretty
import Types exposing (Expr(..), Pattern(..), Value(..))


val : String -> Expr
val name =
    Variable
        { moduleName = []
        , name = name
        }


lambda : Pattern -> Expr -> Expr
lambda arg expr =
    Lambda Environment.empty arg expr


case_ : Expr -> List ( Pattern, Expr ) -> Expr
case_ expr cases =
    Case expr cases


letIn : List ( Pattern, Expr ) -> Expr -> Expr
letIn bindings expr =
    LetIn bindings expr


tuple : List Expr -> Expr
tuple children =
    Tuple children


list : List Expr -> Expr
list children =
    List children


recordUpdate : String -> List ( String, Expr ) -> Expr
recordUpdate recordName updates =
    RecordUpdate recordName updates


record : List ( String, Expr ) -> Expr
record fields =
    Record <| Dict.fromList fields


valueToString : Value -> String
valueToString value =
    valueToExpr value
        |> toString


valueToExpr : Value -> Expr
valueToExpr value =
    case value of
        VInt i ->
            Int i

        VString c ->
            String c

        VFloat c ->
            Float c

        VChar c ->
            Char c

        VBool c ->
            Bool c

        VUnit ->
            Unit

        VTuple c ->
            Tuple (List.map valueToExpr c)

        VRecord c ->
            Record <| Dict.map (\_ -> valueToExpr) c

        VCustom name args ->
            Custom name <| List.map valueToExpr args

        VList c ->
            List <| List.map valueToExpr c

        VJsArray c ->
            JsArray <| Array.map valueToExpr c


toExpression : Expr -> Expression
toExpression value =
    case value of
        String s ->
            Elm.string s

        Int i ->
            Elm.int i

        Float f ->
            Elm.float f

        Char c ->
            Elm.char c

        Bool b ->
            Elm.val (boolToString b)

        Unit ->
            Elm.unit

        Tuple children ->
            Elm.tuple <| List.map toExpression children

        Record fields ->
            fields
                |> Dict.toList
                |> List.map
                    (\( fieldName, fieldValue ) ->
                        ( fieldName, toExpression fieldValue )
                    )
                |> Elm.record

        List children ->
            children
                |> List.map toExpression
                |> Elm.list

        Custom name args ->
            case toArray value of
                Just array ->
                    arrayToExpression "Array" array

                Nothing ->
                    (Elm.fqVal name.moduleName name.name
                        :: List.map toExpression args
                    )
                        |> Elm.apply

        JsArray array ->
            arrayToExpression "JsArray" (Array.toList array)

        Variable v ->
            Elm.fqVal v.moduleName v.name

        Negate child ->
            Elm.negate (toExpression child)

        Case _ _ ->
            Debug.todo "branch 'Case _ _' not implemented"

        Lambda _ pattern child ->
            Elm.lambda [ patternToCodegen pattern ] (toExpression child)

        Apply _ _ ->
            Debug.todo "branch 'Apply _ _' not implemented"

        BinOp l op r ->
            Elm.applyBinOp (toExpression l) op (toExpression r)

        LetIn bindings child ->
            Elm.letExpr (List.map letToExpression bindings) (toExpression child)

        RecordAccess child field ->
            Elm.access (toExpression child) field

        RecordAccessFunction field ->
            Elm.accessFun field

        IfThenElse c t f ->
            Elm.ifExpr (toExpression c) (toExpression t) (toExpression f)

        Negation child ->
            Elm.negate (toExpression child)

        RecordUpdate recordName updates ->
            Elm.update recordName (List.map (Tuple.mapSecond toExpression) updates)

        GLSLExpression content ->
            Elm.glsl content


patternToCodegen : Pattern -> Elm.Pattern
patternToCodegen pattern =
    case pattern of
        AllPattern ->
            Elm.allPattern

        UnitPattern ->
            Elm.unitPattern

        CharPattern v ->
            Elm.charPattern v

        StringPattern v ->
            Elm.stringPattern v

        IntPattern v ->
            Elm.intPattern v

        HexPattern v ->
            Elm.hexPattern v

        FloatPattern v ->
            Elm.floatPattern v

        TuplePattern childPatterns ->
            Elm.tuplePattern (List.map patternToCodegen childPatterns)

        RecordPattern fields ->
            Elm.recordPattern fields

        UnConsPattern headPattern tailPattern ->
            Elm.unConsPattern
                (patternToCodegen headPattern)
                (patternToCodegen tailPattern)

        ListPattern childPatterns ->
            Elm.listPattern (List.map patternToCodegen childPatterns)

        VarPattern v ->
            Elm.varPattern v

        NamedPattern name args ->
            Elm.fqNamedPattern
                name.moduleName
                name.name
                (List.map patternToCodegen args)

        AsPattern childPattern name ->
            Elm.asPattern (patternToCodegen childPattern) name

        ParenthesizedPattern childPattern ->
            Elm.parensPattern (patternToCodegen childPattern)


letToExpression : ( Pattern, Expr ) -> Elm.LetDeclaration
letToExpression _ =
    Debug.todo "letToExpression"


arrayToExpression : String -> List Expr -> Expression
arrayToExpression name array =
    Elm.apply
        [ Elm.fqVal
            [ name ]
            "fromList"
        , array
            |> List
            |> toExpression
        ]


toArray : Expr -> Maybe (List Expr)
toArray value =
    case value of
        Custom name [ _, _, JsArray tree, JsArray tailArray ] ->
            case ( name.moduleName, name.name ) of
                ( [ "Array" ], "Array_elm_builtin" ) ->
                    let
                        treeToArray : Array Expr -> List Expr
                        treeToArray arr =
                            List.concatMap nodeToList (Array.toList arr)

                        nodeToList : Expr -> List Expr
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


boolToString : Bool -> String
boolToString b =
    if b then
        "True"

    else
        "False"


toString : Expr -> String
toString value =
    -- TODO: This is inefficient and subtly different from Debug.toString
    toExpression value
        |> Elm.Pretty.prettyExpression
        |> Pretty.pretty 120


fromOrder : Order -> Expr
fromOrder order =
    case order of
        LT ->
            Custom { moduleName = [ "Basics" ], name = "LT" } []

        EQ ->
            Custom { moduleName = [ "Basics" ], name = "EQ" } []

        GT ->
            Custom { moduleName = [ "Basics" ], name = "GT" } []


toOrder : Expr -> Maybe Order
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
