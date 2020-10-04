module Compiler.Formatter exposing (formatExpression, formatImport, formatModule)

import Compiler.Expression exposing (Expression, ExpressionType(..))
import Compiler.Function exposing (Function)
import Compiler.Module exposing (Import, Module)


formatModule : Module -> String
formatModule module_ =
    let
        moduleDeclaration =
            "module " ++ module_.name

        imports =
            List.map formatImport module_.imports

        functions =
            List.map formatFunction module_.functions

        hasImports =
            not (List.isEmpty imports)

        hasFunctions =
            not (List.isEmpty functions)
    in
    String.join ""
        [ moduleDeclaration
        , breakIf hasImports "\n\n"
        , imports |> String.join "\n"
        , breakIf hasFunctions "\n\n\n"
        , functions |> String.join "\n\n\n"
        ]


breakIf : Bool -> String -> String
breakIf condition break =
    if condition then
        break

    else
        ""


formatImport : Import -> String
formatImport import_ =
    "import " ++ import_.qualifiedName


formatFunction : Function -> String
formatFunction function =
    function.name ++ " =\n    " ++ formatExpression function.lastExpression


formatExpression : Expression -> String
formatExpression expr =
    let
        ( before, after ) =
            if expr.precedence then
                ( "(", ")" )

            else
                ( "", "" )

        formatted =
            case expr.value of
                LiteralInt n ->
                    String.fromInt n

                LiteralFloat n ->
                    String.fromFloat n

                LiteralString str ->
                    "\"" ++ str ++ "\""

                Plus expr1 expr2 ->
                    formatExpression expr1 ++ " + " ++ formatExpression expr2

                Minus expr1 expr2 ->
                    formatExpression expr1 ++ " - " ++ formatExpression expr2

                FunctionCall name args ->
                    if List.isEmpty args then
                        name

                    else
                        name ++ " " ++ String.join " " (List.map formatExpression args)
    in
    before ++ formatted ++ after
