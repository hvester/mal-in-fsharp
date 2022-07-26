namespace Interpreter

open System
open FParsec

module Parser =

    let specialChars = """[](){}'"`,;""".ToCharArray() |> Set.ofArray

    let isSpecialChar c = Set.contains c specialChars

    let pNonSpecialString: Parser<_, unit> =
        many1Satisfy (fun c -> not (Char.IsWhiteSpace c) && not (isSpecialChar c))

    let pNil: Parser<_, unit> = stringReturn "nil" Ast.Nil

    let pBoolean: Parser<_, unit> =
        stringReturn "true" (Ast.Boolean true)
        <|> stringReturn "false" (Ast.Boolean false)

    let pInteger: Parser<_, unit> = pint32 |>> Ast.Integer

    let pSymbol = pNonSpecialString |>> Ast.Symbol

    let pEscapeChar: Parser<_, unit> =
        skipChar '\\' >>. anyOf [ '\\'; '"'; 'n' ]
        |>> function
            | 'n' -> '\n'
            | c -> c

    let pString: Parser<_, unit> =
        skipChar '"'
        >>. manyTill (pEscapeChar <|> anyChar) (skipChar '"')
        |>> (List.toArray >> String)

    let pStr = pString |>> Ast.String

    let pKeywordString: Parser<_, unit> =
        skipChar ':' >>. pNonSpecialString
        |>> KeywordString

    let pKeyword = pKeywordString |>> Ast.Keyword

    let pAst, pAstRef = createParserForwardedToRef ()

    let pComment: Parser<unit, unit> = skipChar ';' .>> skipRestOfLine true

    let ws1: Parser<unit, unit> =
        skipMany1Satisfy (fun c -> Char.IsWhiteSpace c || c = ',')

    let pTrivia = skipMany (ws1 <|> pComment)

    let pAstList = pTrivia >>. (sepEndBy pAst pTrivia)

    let pList =
        between (skipChar '(') (skipChar ')') pAstList
        |>> Ast.List

    let pVector =
        between (skipChar '[') (skipChar ']') pAstList
        |>> Ast.Vector

    let pHashMap =
        let pHashMapKey =
            (pString |>> StringKey)
            <|> (pKeywordString |>> KeywordKey)

        let pKeyValuePair = pHashMapKey .>> pTrivia .>>. pAst
        let pKeyValuePairList = pTrivia >>. (sepEndBy pKeyValuePair pTrivia)

        between (skipChar '{') (skipChar '}') pKeyValuePairList
        |>> (Map.ofList >> HashMap >> Ast.HashMap)

    let pReaderMacroSymbol: Parser<_, unit> =
        stringReturn "'" "quote"
        <|> stringReturn "`" "quasiquote"
        <|> stringReturn "~@" "splice-unquote"
        <|> stringReturn "~" "unquote"
        <|> stringReturn "@" "deref"

    let pReaderMacro =
        pReaderMacroSymbol .>>. pAst
        |>> (fun (symbolName, ast) -> Ast.List [ Ast.Symbol symbolName; ast ])

    do
        pAstRef
        := choice [ pNil
                    pBoolean
                    pInteger
                    pStr
                    pKeyword
                    pList
                    pVector
                    pHashMap
                    pReaderMacro
                    pSymbol ]


    let read (str: string) =
        match run pAstList str with
        | Success (astList, _, _) -> astList
        | Failure (errorMessage, _, _) -> raise (ParsingError(errorMessage))
