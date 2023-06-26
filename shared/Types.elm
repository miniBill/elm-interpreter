module Types exposing (BinOp, Env, Error(..), Eval, EvalConfig, EvalErrorData, EvalErrorKind(..), EvalResult, Expr(..), ExprOrValue(..), LogContinuation(..), LogLine, ModuleName, Pattern(..), QualifiedNameRef, Value(..))

{-| Qualified name reference such as `Maybe.Just`.
-}

import Array exposing (Array)
import Elm.CodeGen
import FastDict exposing (Dict)
import Parser exposing (DeadEnd)
import Rope exposing (Rope)


type alias BinOp =
    Elm.CodeGen.BinOp


type alias ModuleName =
    List String


type alias QualifiedNameRef =
    -- Copied from elm-syntax
    { moduleName : ModuleName
    , name : String
    }


type ExprOrValue
    = Expr Env Expr
    | Value Value


type Expr
    = -- Leaves
      String String
    | Int Int
    | Float Float
    | Char Char
    | Bool Bool
    | Unit
      -- Containers
    | Tuple (List Expr)
    | Record (Dict String Expr)
    | Custom QualifiedNameRef (List Expr)
    | List (List Expr)
    | JsArray (Array Expr)
      -- Blocks
    | Lambda Env Pattern Expr
    | RecordAccess Expr String
    | RecordAccessFunction String
    | RecordUpdate String (List ( String, Expr ))
    | Variable QualifiedNameRef
    | Case Expr (List ( Pattern, Expr ))
    | Negate Expr
    | Apply Expr Expr
    | BinOp Expr BinOp Expr
    | LetIn (List ( Pattern, Expr )) Expr
    | IfThenElse Expr Expr Expr
    | Negation Expr
    | GLSLExpression String


type Value
    = VString String
    | VInt Int
    | VFloat Float
    | VChar Char
    | VBool Bool
    | VUnit
    | VLambda Env Pattern Expr
      -- Containers
    | VTuple (List Value)
    | VRecord (Dict String Value)
    | VCustom QualifiedNameRef (List Value)
    | VList (List Value)
    | VJsArray (Array Value)


{-| Pattern, taken from `Elm.Syntax.Pattern` but de-noded.
-}
type Pattern
    = AllPattern
    | UnitPattern
    | CharPattern Char
    | StringPattern String
    | IntPattern Int
    | HexPattern Int
    | FloatPattern Float
    | TuplePattern (List Pattern)
    | RecordPattern (List String)
    | UnConsPattern Pattern Pattern
    | ListPattern (List Pattern)
    | VarPattern String
    | NamedPattern QualifiedNameRef (List Pattern)
    | AsPattern Pattern String
    | ParenthesizedPattern Pattern


type alias Env =
    { callStack : List QualifiedNameRef
    , values : Dict String ExprOrValue
    }


type alias Eval out =
    EvalConfig -> Env -> EvalResult out


type alias EvalConfig =
    { trace : Bool
    , logContinuation : LogContinuation
    }


type LogContinuation
    = LogDone
    | LogAppendTo (Rope LogLine) LogContinuation
    | LogPrepend LogLine LogContinuation
    | LogAppend LogLine LogContinuation


type Error
    = ParsingError (List DeadEnd)
    | EvalError EvalErrorData


type alias EvalResult out =
    ( Result EvalErrorData out
    , Rope LogLine
    )


type alias EvalErrorData =
    { callStack : List QualifiedNameRef
    , error : EvalErrorKind
    }


type EvalErrorKind
    = TypeError String
    | Unsupported String
    | NameError String
    | Todo String


type alias LogLine =
    { message : String
    , env : Env
    }
