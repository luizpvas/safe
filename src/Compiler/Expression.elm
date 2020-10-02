module Compiler.Expression exposing (Expression(..))

import Compiler.Type exposing (Type)


type Variable
    = WholeAssignment String Type


type Expression
    = LiteralString String
    | LiteralInt Int
