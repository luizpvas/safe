module Compiler.Html exposing (Children(..), Element(..), LocatedElement, Tag, parser)

import Compiler.Located as Located exposing (Located)
import Compiler.Parser.DoubleQuotedString as DoubleQuotedString
import Parser exposing (..)
import Set


type alias LocatedElement =
    Located Element


type Element
    = TextNode (Located String)
    | TagNode Tag


type alias Tag =
    { name : Located String
    , attributes : List (Located HtmlAttribute)
    , children : Children
    }


type Children
    = SelfClosing
    | Elements (List LocatedElement)


type alias HtmlAttribute =
    { key : String
    , value : String
    }


parser : Parser LocatedElement
parser =
    tag


tag : Parser LocatedElement
tag =
    Located.locate <|
        map TagNode <|
            succeed Tag
                |. symbol "<"
                |= tagName
                |. spaces
                |= attributes []
                |= oneOf
                    [ succeed SelfClosing
                        |. symbol "/>"
                    , succeed identity
                        |. symbol ">"
                        |. spaces
                        |= lazy (\_ -> map Elements (children []))
                        |. spaces
                        |. symbol "</"
                        |. tagName
                        |. symbol ">"
                    ]


children : List LocatedElement -> Parser (List LocatedElement)
children tags =
    oneOf
        [ htmlText
            |> andThen (\locatedText -> children (Located.copy locatedText (TextNode locatedText) :: tags))
        , backtrackable tag
            |> andThen (\locatedTag -> children (locatedTag :: tags))
        , lazy (\_ -> succeed (List.reverse tags))
        ]


attributes : List (Located HtmlAttribute) -> Parser (List (Located HtmlAttribute))
attributes attrs =
    oneOf
        [ Located.locate
            (succeed Tuple.pair
                |= tagName
                |. symbol "="
                |= DoubleQuotedString.parser
                |. spaces
            )
            |> andThen
                (\locatedKeyValue ->
                    let
                        locatedKey =
                            Tuple.first locatedKeyValue.value

                        value =
                            Tuple.second locatedKeyValue.value
                    in
                    attributes
                        ({ precedence = False
                         , start = locatedKeyValue.start
                         , value = HtmlAttribute locatedKey.value value
                         , end = locatedKeyValue.end
                         }
                            :: attrs
                        )
                )
        , lazy (\_ -> succeed (List.reverse attrs))
        ]


htmlText : Parser (Located String)
htmlText =
    Located.locate <|
        variable
            { start = \c -> c /= '<' && c /= '>'
            , inner = \c -> c /= '<' && c /= '>'
            , reserved = Set.fromList []
            }


tagName : Parser (Located String)
tagName =
    Located.locate <|
        variable
            { start = \c -> Char.isAlpha c && Char.isLower c
            , inner = \c -> Char.isAlpha c || c == '-'
            , reserved = Set.fromList []
            }
