module Compiler.Function exposing (Function)

import Compiler.Expression exposing (Expression)


type alias Function =
    { name : String
    , arguments : List String
    , expressions : List Expression
    , lastExpression : Expression
    }
