module Compiler.Formatter exposing (defaultSettings, formatExpression, formatHtml, formatImport, formatModule)

import Compiler.Expression exposing (Expression(..), LocatedExpression)
import Compiler.Function exposing (Function)
import Compiler.Html as Html exposing (LocatedElement)
import Compiler.Module exposing (Import, Module)


type alias FormatterSettings =
    { maxLineLength : Int
    , indentSize : Int
    }


defaultSettings : FormatterSettings
defaultSettings =
    { maxLineLength = 80
    , indentSize = 4
    }


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


formatExpression : LocatedExpression -> String
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

                LiteralList expressions ->
                    "[" ++ String.join ", " (List.map formatExpression expressions) ++ "]"

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


formatHtml : FormatterSettings -> Int -> LocatedElement -> String
formatHtml settings indentLevel element =
    case element.value of
        Html.TextNode node ->
            node.value

        Html.TagNode node ->
            formatHtmlTag settings indentLevel node


formatHtmlTag : FormatterSettings -> Int -> Html.Tag -> String
formatHtmlTag settings indentLevel tag =
    let
        openingTagStart =
            "<" ++ tag.name.value

        isSelfClosing =
            case tag.children of
                Html.SelfClosing ->
                    True

                Html.Elements _ ->
                    False

        openingTagEnd =
            case tag.children of
                Html.SelfClosing ->
                    "/>"

                Html.Elements _ ->
                    ">"

        closingTag =
            case tag.children of
                Html.SelfClosing ->
                    ""

                Html.Elements _ ->
                    "</" ++ tag.name.value ++ ">"

        formattedAttributes =
            List.map (\attr -> attr.value.key ++ "=" ++ "\"" ++ attr.value.value ++ "\"") tag.attributes

        hasAttributes =
            not <| List.isEmpty tag.attributes

        formattedAttributesLength =
            formattedAttributes |> List.map String.length |> List.sum

        canFitOpeningTagInOneLine =
            settings.maxLineLength
                > List.sum
                    [ String.length openingTagStart
                    , 1 -- space between tag and first attribute
                    , formattedAttributesLength
                    , List.length formattedAttributes - 1 -- space between each attribute
                    , String.length openingTagEnd
                    ]

        openingTag =
            if canFitOpeningTagInOneLine then
                String.join ""
                    [ openingTagStart
                    , if hasAttributes then
                        " "

                      else
                        ""
                    , String.join " " formattedAttributes
                    , if hasAttributes || isSelfClosing then
                        " "

                      else
                        ""
                    , openingTagEnd
                    ]

            else
                let
                    oneAttributePerLine =
                        formattedAttributes
                            |> List.map (\str -> indentToSpaces settings (indentLevel + 1) ++ str)
                            |> String.join "\n"
                in
                openingTagStart ++ "\n" ++ oneAttributePerLine ++ "\n" ++ openingTagEnd

        children =
            case tag.children of
                Html.SelfClosing ->
                    ""

                Html.Elements tags ->
                    case tags of
                        [] ->
                            ""

                        nonEmptyTags ->
                            List.map (formatHtml settings indentLevel) nonEmptyTags
                                |> String.join ""
    in
    openingTag ++ children ++ closingTag


indentToSpaces : FormatterSettings -> Int -> String
indentToSpaces settings indentLevel =
    String.repeat (indentLevel * settings.indentSize) " "
