namespace Interpreter

open System.Collections.Generic

module Evaluator =

    exception EvaluationError of string

    type BindingValue =
        | ValueBinding of Ast
        | FunctionBinding of (Ast -> Ast -> Ast)

    type Env =
        { Outer: Env option
          Bindings: Dictionary<string, BindingValue> }

        member this.Set(symbolName, value) = this.Bindings[ symbolName ] <- value

        member this.Find(symbolName) =
            if this.Bindings.ContainsKey(symbolName) then
                this
            else
                match this.Outer with
                | Some outer -> outer.Find(symbolName)
                | None ->
                    $"Cannot find environment with symbol {symbolName}"
                    |> EvaluationError
                    |> raise

        member this.Get(symbolName) =
            this.Find(symbolName).Bindings[symbolName]

        member this.CreateInnerEnv() =
            { Outer = Some this; Bindings = Dictionary() }

    let integerOperation f ast1 ast2 =
        match ast1, ast2 with
        | Ast.Integer x, Ast.Integer y -> Ast.Integer(f x y)
        | _ ->
            sprintf "Invalid types: %s, %s" (string ast1) (string ast2)
            |> EvaluationError
            |> raise

    let add = integerOperation (fun x y -> x + y)
    let substract = integerOperation (fun x y -> x - y)
    let multiply = integerOperation (fun x y -> x * y)
    let divide = integerOperation (fun x y -> x / y)

    let createInitialEnv () =
        let d = Dictionary<_, _>()
        d.Add("+", FunctionBinding add)
        d.Add("-", FunctionBinding substract)
        d.Add("*", FunctionBinding multiply)
        d.Add("/", FunctionBinding divide)
        { Outer = None; Bindings = d }

    let evalError msg = raise (EvaluationError(msg))

    let rec evalAst (env: Env) (ast: Ast) =
        match ast with
        | Ast.Symbol symbolName ->
            match env.Get(symbolName) with
            | ValueBinding value -> value
            | FunctionBinding _ -> evalError "Expected a value binding but got function binding"

        | Ast.List asts ->
            match asts with
            | [] -> ast

            | [ Ast.Symbol "def!"; ast1; ast2 ] ->
                match ast1 with
                | Ast.Symbol symbolName ->
                    let value = evalAst env ast2
                    env.Set(symbolName, ValueBinding value)
                    value
                | _ -> evalError $"Expected a symbol but got: {string ast1}"

            | [ Ast.Symbol "let*"; ast1; ast2 ] ->
                match ast1 with
                | Ast.List bindingAstList
                | Ast.Vector bindingAstList ->
                    let innerEnv = env.CreateInnerEnv()
                    for bindingAsts in List.chunkBySize 2 bindingAstList do
                        match bindingAsts with
                        | [ Ast.Symbol name; valueAst ] ->
                            innerEnv.Set(name, ValueBinding(evalAst innerEnv valueAst))
                        | _ ->
                            evalError $"Invalid let* binding: {string ast}"

                    evalAst innerEnv ast2

                | _ -> evalError $"Expected a list but got: {string ast1}"

            | [ Ast.Symbol symbolName; ast1; ast2 ] ->
                match env.Get(symbolName) with
                | FunctionBinding operation -> operation (evalAst env ast1) (evalAst env ast2)
                | ValueBinding x -> evalError $"Expected a function but got: {string x}"

            | _ -> evalError $"Cannot evaluate list: {string ast}"

        | Ast.Vector asts -> asts |> List.map (evalAst env) |> Ast.Vector

        | Ast.HashMap (HashMap m) ->
            Map.map (fun _ v -> evalAst env v) m
            |> HashMap
            |> Ast.HashMap

        | _ -> ast


    let eval env asts =
        let rec loop remaining =
            match remaining with
            | [] -> None
            | [ ast ] -> evalAst env ast |> Some
            | ast :: tail ->
                evalAst env ast |> ignore
                loop tail

        loop asts
