namespace Interpreter

open System.Collections.Generic

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
        | CoreFunction of (Ast list -> Ast)
        | UserDefinedFunction of env: Env * argumentNames: string list * body: Ast
        | Quote of Ast
        | Quasiquote of Ast
        | Unquote of Ast
        | SpliceUnquote of Ast
        | Deref of Ast

    type Env(outer: Env option) =

        let bindings = Dictionary<string, Ast>()

        member _.Set(symbolName, ast) = bindings[symbolName] <- ast

        member _.Resolve(symbolName) =
            match bindings.TryGetValue(symbolName) with
            | true, value -> value
            | false, _ ->
                match outer with
                | Some o -> o.Resolve(symbolName)
                | None -> raise (SymbolResolutionError symbolName)


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

    let unwrapString ast =
        match ast with
        | Ast.String str -> str
        | _ -> raise (EvaluationError("Expected a string.", ast))

    let unwrapList ast =
        match ast with
        | Ast.List xs -> xs
        | _ -> raise (EvaluationError("Expected a list.", ast))

    let unwrapCollection ast =
        match ast with
        | Ast.List xs
        | Ast.Vector xs -> xs
        | _ -> raise (EvaluationError("Expected a list or a vector.", ast))
