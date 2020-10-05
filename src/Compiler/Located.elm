module Compiler.Located exposing (Located, copy, locate, locateWithPrecedence, map, wrap)

import Parser exposing (..)


{-| <https://package.elm-lang.org/packages/elm/parser/latest/Parser#getPosition>

Code editors treat code like a grid, with rows and columns.
The start is row=1 and col=1. As you chomp characters, the col increments.
When you run into a \\n character, the row increments and col goes back to 1.

In the Elm compiler, I track the start and end position of every expression like this:

So if there is a problem during type inference, I use this saved position information
to underline the exact problem!

-}
type alias Located a =
    { precedence : Bool
    , start : ( Int, Int )
    , value : a
    , end : ( Int, Int )
    }


locate : Parser a -> Parser (Located a)
locate parser =
    succeed (Located False)
        |= getPosition
        |= parser
        |= getPosition


locateWithPrecedence : Parser a -> Parser (Located a)
locateWithPrecedence parser =
    succeed (Located True)
        |= getPosition
        |= parser
        |= getPosition


copy : Located a -> b -> Located b
copy located newValue =
    { precedence = located.precedence
    , start = located.start
    , value = newValue
    , end = located.end
    }


map : (a -> b) -> Located a -> Located b
map function located =
    { precedence = located.precedence
    , start = located.start
    , value = function located.value
    , end = located.end
    }


wrap : Located a -> Located a -> a -> Located a
wrap start end value =
    { precedence = start.precedence && end.precedence, start = start.start, value = value, end = end.end }
