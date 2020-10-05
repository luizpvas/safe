module Compiler.HtmlTest exposing (expectParseAndFormat, suite)

import Compiler.Formatter as Formatter exposing (formatHtml)
import Compiler.Html exposing (parser)
import Expect exposing (Expectation)
import Parser
import Test exposing (..)


suite : Test
suite =
    describe "Html parser"
        [ test "tag with no attributes" <|
            \_ ->
                expectParseAndFormat "<div></div>"
        , test "self closing tag with no attributes" <|
            \_ ->
                expectParseAndFormat "<input />"
        , test "self closing tag with a single attribute" <|
            \_ ->
                expectParseAndFormat "<input name=\"query\" />"
        , test "self closing tag with a multiple attributes" <|
            \_ ->
                expectParseAndFormat "<input name=\"query\" placeholder=\"Search...\" />"
        , test "self closing wag with multiple attributes that overflows 80ch" <|
            \_ ->
                expectParseAndFormat """
<input
    name="query"
    class="border rounded-sm p-2 bg-white focus:border-blue"
    placeholder="Search..."
/>
"""
        , test "tag with text inside" <|
            \_ ->
                expectParseAndFormat "<div>Hello world</div>"
        , test "tag with nested tag in the same line" <|
            \_ ->
                expectParseAndFormat "<div>Hello <b>world</b></div>"
        ]


expectParseAndFormat : String -> Expectation
expectParseAndFormat code =
    let
        parsed =
            Parser.run parser (String.trim code)

        formatted =
            case parsed of
                Ok data ->
                    Just (formatHtml Formatter.defaultSettings 0 data)

                Err err ->
                    let
                        _ =
                            Debug.log "err" err
                    in
                    Nothing
    in
    Expect.equal formatted (Just (String.trim code))
