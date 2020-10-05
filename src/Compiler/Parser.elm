module Compiler.Parser exposing (parse)

import Compiler.Expression exposing (LocatedExpression)
import Compiler.Function exposing (Function)
import Compiler.Module exposing (Import, Module)
import Parser exposing (..)
import Set


parse : String -> Result (List Parser.DeadEnd) Module
parse str =
    Parser.run mod str


mod : Parser Module
mod =
    succeed (\name imps -> Module name imps [])
        |. spaces
        |. keyword "module"
        |. spaces
        |= moduleName
        |. spaces
        |= loop [] imports


imports : List Import -> Parser (Step (List Import) (List Import))
imports list =
    oneOf
        [ succeed (\parsedImport -> Loop (parsedImport :: list))
            |= importParser
            |. spaces
        , succeed ()
            |> map (\_ -> Done (List.reverse list))
        ]


importParser : Parser Import
importParser =
    succeed Import
        |. keyword "import"
        |. spaces
        |= qualifiedImport


moduleName : Parser String
moduleName =
    variable
        { start = Char.isUpper
        , inner = \c -> Char.isAlphaNum c
        , reserved = Set.fromList []
        }


qualifiedImport : Parser String
qualifiedImport =
    variable
        { start = Char.isUpper
        , inner = \c -> Char.isAlphaNum c || c == '.'
        , reserved = Set.fromList []
        }
