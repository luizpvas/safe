module Compiler.ExpressionTest exposing (expectParseAndFormat, suite)

import Compiler.Expression exposing (parser)
import Compiler.Formatter exposing (formatExpression)
import Expect exposing (Expectation)
import Parser
import Test exposing (..)


suite : Test
suite =
    describe "expressions"
        [ test "positive literal integer" <|
            \_ ->
                expectParseAndFormat "35"
        , test "literal floating number" <|
            \_ ->
                expectParseAndFormat "5.4"
        , test "plus two numbers" <|
            \_ ->
                expectParseAndFormat "1 + 2"
        , test "minus two numbers" <|
            \_ ->
                expectParseAndFormat "1 - 2"
        , test "plus chain with three numbers" <|
            \_ ->
                expectParseAndFormat "1 + 2 + 3"
        , test "string concatenation" <|
            \_ ->
                expectParseAndFormat "\"Hello \" + \"world\""
        , test "plus followed by another expression with precedence" <|
            \_ ->
                expectParseAndFormat "1 + (2 + 3)"
        , test "plus with precedence follow by another plus" <|
            \_ ->
                expectParseAndFormat "(1 + 2) + 3"
        , test "nested precedence in math expression" <|
            \_ ->
                expectParseAndFormat "(1 + (2 + 3)) + 4"
        , test "function call with no arguments and no module" <|
            \_ ->
                expectParseAndFormat "myFunction"
        , test "function call with no arguments inside a module" <|
            \_ ->
                expectParseAndFormat "MyModule.myFunction"
        , test "function call with a literal argument" <|
            \_ ->
                expectParseAndFormat "myFunction 10"
        , test "function call with two literal arguments" <|
            \_ ->
                expectParseAndFormat "myFunction 10 \"foo\""
        , test "function call with nested call" <|
            \_ ->
                expectParseAndFormat "myFunction (myOtherFunction 10)"
        , test "function call with qualified name with partially applied function as arg" <|
            \_ ->
                expectParseAndFormat "List.map (add 10) myList"
        , test "function call with literal list" <|
            \_ ->
                expectParseAndFormat "List.map (add 10) [1, 2, 3]"
        ]


expectParseAndFormat : String -> Expectation
expectParseAndFormat code =
    let
        parsed =
            Parser.run parser code

        formatted =
            case parsed of
                Ok data ->
                    Just (formatExpression data)

                Err err ->
                    let
                        _ =
                            Debug.log "err" err
                    in
                    Nothing
    in
    Expect.equal formatted (Just (String.trim code))
