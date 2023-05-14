module Core.Basics exposing (env)

{-
   import Basics exposing (..)
   import List exposing (List, (::))
   import Maybe exposing (Maybe(..))
   import Result exposing (Result(..))
   import String exposing (String)
   import Char exposing (Char)
   import Tuple

   import Debug

   import Platform exposing ( Program )
   import Platform.Cmd as Cmd exposing ( Cmd )
   import Platform.Sub as Sub exposing ( Sub )
-}

import Elm.Syntax.Expression exposing (FunctionImplementation)
import Env exposing (Env)
import FastDict as Dict


env : Env
env =
    { values = Dict.empty
    , functions = Dict.fromList functions
    }


functions : List ( String, FunctionImplementation )
functions =
    []
