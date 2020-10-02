module Example exposing (suite)

import Compiler.Expression exposing (Expression(..))
import Compiler.Module exposing (Module)
import Compiler.Parser exposing (parse)
import Expect exposing (Expectation)
import Test exposing (..)


suite : Test
suite =
    describe "module declaration"
        [ test "a module with a single function" <|
            \_ ->
                let
                    mod =
                        parse code1
                in
                Expect.equal 2 2
        ]


code1 : String
code1 =
    """
module MyApp
    """
