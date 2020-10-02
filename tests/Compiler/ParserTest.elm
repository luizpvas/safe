module Compiler.ParserTest exposing (expectParseAndFormat, suite)

import Compiler.Formatter exposing (formatModule)
import Compiler.Parser exposing (parse)
import Expect exposing (Expectation)
import Test exposing (..)


suite : Test
suite =
    describe "parser test"
        [ test "empty module" <|
            \_ ->
                expectParseAndFormat """
module MyApp
"""
        , test "module with imports" <|
            \_ ->
                expectParseAndFormat """
module MyApp

import Http
import Encoding.Json
"""
        , test "module + imports + a single expression" <|
            \_ ->
                expectParseAndFormat """
module MyApp

import Http


a =
    10
"""
        ]


expectParseAndFormat : String -> Expectation
expectParseAndFormat code =
    let
        parsed =
            parse code

        formatted =
            case parsed of
                Ok data ->
                    let
                        _ =
                            Debug.log "data" data
                    in
                    Just (formatModule data)

                Err err ->
                    let
                        _ =
                            Debug.log "err" err
                    in
                    Nothing
    in
    Expect.equal (Just (String.trim code)) formatted
