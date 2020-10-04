module Compiler.Expression exposing (Expression, ExpressionType(..), parser)

import Compiler.Located as Located exposing (Located)
import Compiler.Parser.DoubleQuotedString as DoubleQuotedString
import Parser exposing (..)
import Set


type Operator
    = OpPlus
    | OpMinus


type alias Expression =
    Located ExpressionType


type ExpressionType
    = LiteralString String
    | LiteralInt Int
    | LiteralFloat Float
    | Plus Expression Expression
    | Minus Expression Expression
    | FunctionCall String (List Expression)


parser : Parser Expression
parser =
    term |> andThen (expressionHelp [])


expressionHelp : List ( Expression, Operator ) -> Expression -> Parser Expression
expressionHelp revOps expr =
    oneOf
        [ succeed Tuple.pair
            |. spaces
            |= operator
            |. spaces
            |= term
            |> andThen (\( op, newExpr ) -> expressionHelp (( expr, op ) :: revOps) newExpr)
        , lazy (\_ -> succeed (finalize revOps expr))
        ]


term : Parser Expression
term =
    oneOf
        [ Located.map False digits
        , Located.map False stringLiteral
        , functionCall
        , succeed (\expression -> { expression | precedence = True })
            |. symbol "("
            |. spaces
            |= lazy (\_ -> parser)
            |. spaces
            |. symbol ")"
        ]


digits : Parser ExpressionType
digits =
    number
        { int = Just LiteralInt
        , hex = Just LiteralInt
        , octal = Nothing
        , binary = Nothing
        , float = Just LiteralFloat
        }


stringLiteral : Parser ExpressionType
stringLiteral =
    map LiteralString DoubleQuotedString.parser


operator : Parser Operator
operator =
    oneOf
        [ map (\_ -> OpPlus) (symbol "+")
        , map (\_ -> OpMinus) (symbol "-")
        ]


finalize : List ( Expression, Operator ) -> Expression -> Expression
finalize revOps finalExpr =
    case revOps of
        [] ->
            finalExpr

        ( expr, OpPlus ) :: otherRevOps ->
            finalize otherRevOps (Located.wrap expr finalExpr (Plus expr finalExpr))

        ( expr, OpMinus ) :: otherRevOps ->
            finalize otherRevOps (Located.wrap expr finalExpr (Minus expr finalExpr))


functionCall : Parser Expression
functionCall =
    functionCallIdentifier
        |> andThen (argumentsHelp [])
        |> map
            (\( name, args ) ->
                { precedence = False
                , start = name.start
                , value = FunctionCall name.value (List.reverse args)
                , end = name.end
                }
            )


argumentsHelp : List Expression -> Located String -> Parser ( Located String, List Expression )
argumentsHelp args functionName =
    oneOf
        [ succeed identity
            |. spaces
            |= term
            |> andThen (\expr -> argumentsHelp (expr :: args) functionName)
        , lazy (\_ -> succeed ( functionName, args ))
        ]


functionCallIdentifier : Parser (Located String)
functionCallIdentifier =
    oneOf
        [ Located.map False variableName
        , Located.map False moduleName
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
        { start = \c -> Char.isAlpha c && Char.isUpper c
        , inner = \c -> Char.isAlphaNum c || c == '.'
        , reserved = Set.fromList []
        }
        |> andThen checkQualifiedModuleName


checkQualifiedModuleName : String -> Parser String
checkQualifiedModuleName str =
    case String.split "." str of
        [] ->
            succeed str

        parts ->
            let
                allOk =
                    List.map (\word -> True) parts |> List.all identity
            in
            if allOk then
                succeed str

            else
                problem "modules must be in the form of `Module.Submodule.method`"
