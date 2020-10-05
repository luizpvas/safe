module Compiler.Expression exposing (Expression(..), LocatedExpression, parser)

import Compiler.Located as Located exposing (Located)
import Compiler.Parser.DoubleQuotedString as DoubleQuotedString
import Parser exposing (..)
import Set


type Operator
    = OpPlus
    | OpMinus


type alias LocatedExpression =
    Located Expression


type Expression
    = LiteralString String
    | LiteralInt Int
    | LiteralFloat Float
    | LiteralList (List LocatedExpression)
    | Plus LocatedExpression LocatedExpression
    | Minus LocatedExpression LocatedExpression
    | FunctionCall String (List LocatedExpression)


parser : Parser LocatedExpression
parser =
    term |> andThen (expressionHelp [])


expressionHelp : List ( LocatedExpression, Operator ) -> LocatedExpression -> Parser LocatedExpression
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


term : Parser LocatedExpression
term =
    oneOf
        [ Located.locate digits
        , Located.locate stringLiteral
        , functionCall
        , Located.locate (map LiteralList literalList)
        , succeed (\expression -> { expression | precedence = True })
            |. symbol "("
            |. spaces
            |= lazy (\_ -> parser)
            |. spaces
            |. symbol ")"
        ]


literalList : Parser (List LocatedExpression)
literalList =
    sequence
        { start = "["
        , separator = ","
        , end = "]"
        , spaces = spaces
        , item = lazy (\_ -> parser)
        , trailing = Parser.Forbidden
        }


digits : Parser Expression
digits =
    number
        { int = Just LiteralInt
        , hex = Just LiteralInt
        , octal = Nothing
        , binary = Nothing
        , float = Just LiteralFloat
        }


stringLiteral : Parser Expression
stringLiteral =
    map LiteralString DoubleQuotedString.parser


operator : Parser Operator
operator =
    oneOf
        [ map (\_ -> OpPlus) (symbol "+")
        , map (\_ -> OpMinus) (symbol "-")
        ]


finalize : List ( LocatedExpression, Operator ) -> LocatedExpression -> LocatedExpression
finalize revOps finalExpr =
    case revOps of
        [] ->
            finalExpr

        ( expr, OpPlus ) :: otherRevOps ->
            finalize otherRevOps (Located.wrap expr finalExpr (Plus expr finalExpr))

        ( expr, OpMinus ) :: otherRevOps ->
            finalize otherRevOps (Located.wrap expr finalExpr (Minus expr finalExpr))


functionCall : Parser LocatedExpression
functionCall =
    functionCallIdentifier
        |> andThen (functionArguments [])
        |> map
            (\( name, args ) ->
                { precedence = False
                , start = name.start
                , value = FunctionCall name.value (List.reverse args)
                , end = name.end
                }
            )


functionArguments : List LocatedExpression -> Located String -> Parser ( Located String, List LocatedExpression )
functionArguments args fname =
    oneOf
        [ succeed identity
            |. spaces
            |= term
            |> andThen (\expr -> functionArguments (expr :: args) fname)
        , lazy (\_ -> succeed ( fname, args ))
        ]


functionCallIdentifier : Parser (Located String)
functionCallIdentifier =
    oneOf
        [ Located.locate functionName
        , Located.locate qualifiedModuleNameEndingInFunctionName
        ]


functionName : Parser String
functionName =
    variable
        { start = \c -> Char.isAlpha c && Char.isLower c
        , inner = \c -> Char.isAlphaNum c
        , reserved = Set.fromList []
        }


qualifiedModuleNameEndingInFunctionName : Parser String
qualifiedModuleNameEndingInFunctionName =
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
