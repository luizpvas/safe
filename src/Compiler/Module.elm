module Compiler.Module exposing (Import, Module)

import Compiler.Function exposing (Function)


type alias Module =
    { name : String
    , imports : List Import
    , functions : List Function
    }


type alias Import =
    { qualifiedName : String
    }
