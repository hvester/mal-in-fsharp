namespace Interpreter

[<AutoOpen>]
module rec AstTypes =


    type KeywordString = KeywordString of string

    type HashMapKey =
        | StringKey of string
        | KeywordKey of KeywordString

    type HashMap = HashMap of Map<HashMapKey, Ast>

    [<RequireQualifiedAccess>]
    type Ast =
        | Nil
        | Boolean of bool
        | Integer of int
        | Symbol of string
        | String of string
        | Keyword of KeywordString
        | Vector of Ast list
        | HashMap of HashMap
        | List of Ast list
        | Function of (Ast list -> Ast)
        | Quote of Ast
        | Quasiquote of Ast
        | Unquote of Ast
        | SpliceUnquote of Ast
        | Deref of Ast

    exception ParsingError of message: string

    exception SymbolResolutionError of symbolName: string

    exception ArgumentError of message: string

    exception EvaluationError of message: string * invalidExpression: Ast


module Ast =

    let unwrapInteger ast =
        match ast with
        | Ast.Integer i -> i
        | _ -> raise (EvaluationError("Expected an integer.", ast))

    let unwrapSymbol ast =
        match ast with
        | Ast.Symbol symbolName -> symbolName
        | _ -> raise (EvaluationError("Expected a symbol.", ast))

    let unwrapList ast =
        match ast with
        | Ast.List xs -> xs
        | _ -> raise (EvaluationError("Expected a list.", ast))
