module Compiler.Function exposing (Function)

import Compiler.Expression exposing (LocatedExpression)


type alias Function =
    { name : String
    , arguments : List String
    , expressions : List LocatedExpression
    , lastExpression : LocatedExpression
    }
