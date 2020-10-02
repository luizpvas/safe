module Compiler.Parser exposing (parse)

import Compiler.Expression exposing (Expression(..))
import Compiler.Function exposing (Function)
import Compiler.Module exposing (Import, Module)
import Parser exposing ((|.), (|=), Parser, Step(..), float, int, keyword, loop, map, oneOf, problem, spaces, succeed, symbol, variable)
import Set exposing (Set)


parse : String -> Result (List Parser.DeadEnd) Module
parse str =
    Parser.run mod str


mod : Parser Module
mod =
    succeed Module
        |. spaces
        |. keyword "module"
        |. spaces
        |= moduleName
        |. spaces
        |= loop [] imports
        |. spaces
        |= loop [] functions


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


functions : List Function -> Parser (Step (List Function) (List Function))
functions list =
    oneOf
        [ succeed (\parsed -> Loop (parsed :: list))
            |= functionParser
            |. spaces
        , succeed ()
            |> map (\_ -> Done (List.reverse list))
        ]


functionParser : Parser Function
functionParser =
    succeed (\name lastExpr -> Function name [] [] lastExpr)
        |= variableName
        |. spaces
        |. keyword "="
        |. spaces
        |= expressionParser


expressions : List Expression -> Parser (Step (List Expression) (List Expression))
expressions list =
    oneOf
        [ succeed (\parsed -> Loop (parsed :: list))
            |= expressionParser
            |. spaces
        , succeed ()
            |> map (\_ -> Done (List.reverse list))
        ]


expressionParser : Parser Expression
expressionParser =
    oneOf
        [ map LiteralInt int
        ]


variableName : Parser String
variableName =
    variable
        { start = \c -> Char.isAlpha c && Char.isLower c
        , inner = \c -> Char.isAlphaNum c
        , reserved = Set.fromList []
        }


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
