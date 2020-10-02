module Compiler.Formatter exposing (formatImport, formatModule)

import Compiler.Expression exposing (Expression(..))
import Compiler.Function exposing (Function)
import Compiler.Module exposing (Import, Module)


formatModule : Module -> String
formatModule module_ =
    let
        moduleDeclaration =
            "module " ++ module_.name

        imports =
            List.map formatImport module_.imports |> String.join "\n"

        functions =
            List.map formatFunction module_.functions |> String.join "\n\n"
    in
    case imports of
        "" ->
            moduleDeclaration

        _ ->
            String.join "\n\n" [ moduleDeclaration, imports, functions ]


formatImport : Import -> String
formatImport import_ =
    "import " ++ import_.qualifiedName


formatFunction : Function -> String
formatFunction function =
    function.name ++ " =\n    " ++ formatExpression function.lastExpression


formatExpression : Expression -> String
formatExpression expr =
    case expr of
        LiteralInt n ->
            String.fromInt n

        LiteralString str ->
            "\"" ++ str ++ "\""
