namespace Interpreter

open System.Collections.Generic

module Evaluator =

    let evalError msg = raise (EvaluationError(msg))

    type Env =
        { Outer: Env option
          Bindings: Dictionary<string, Ast> }

        member this.Set(symbolName, value) = this.Bindings[ symbolName ] <- value

        member this.Find(symbolName) =
            if this.Bindings.ContainsKey(symbolName) then
                this
            else
                match this.Outer with
                | Some outer -> outer.Find(symbolName)
                | None -> evalError $"{symbolName} not found"

        member this.Get(symbolName) =
            this.Find(symbolName).Bindings[symbolName]

        member this.CreateInnerEnv() =
            { Outer = Some this
              Bindings = Dictionary() }


    let createInitialEnv () =
        let d = Dictionary<_, _>()
        d.Add("+", Ast.Function Core.add)
        d.Add("-", Ast.Function Core.substract)
        d.Add("*", Ast.Function Core.multiply)
        d.Add("/", Ast.Function Core.divide)
        d.Add("list", Ast.Function Core.list)
        d.Add("list?", Ast.Function Core.isList)
        d.Add("empty?", Ast.Function Core.isEmpty)
        d.Add("count", Ast.Function Core.count)
        d.Add("=", Ast.Function Core.areEqual)
        d.Add("<", Ast.Function Core.lessThan)
        d.Add("<=", Ast.Function Core.lessThanOrEqual)
        d.Add(">", Ast.Function Core.greaterThan)
        d.Add(">=", Ast.Function Core.greaterThanOrEqual)
        d.Add("prn", Ast.Function Core.prn)
        { Outer = None; Bindings = d }

    let rec evalAst (env: Env) (ast: Ast) =
        match ast with
        | Ast.Symbol symbolName -> env.Get(symbolName)

        | Ast.List asts ->
            match asts with
            | [] -> ast

            | Ast.Symbol "def!" :: asts ->
                match asts with
                | Ast.Symbol symbolName :: valueAst :: _ ->
                    let value = evalAst env valueAst
                    env.Set(symbolName, value)
                    value
                | _ -> evalError $"Invalid def!: {string ast}"

            | Ast.Symbol "let*" :: asts ->
                match asts with
                | Ast.List bindingAstList :: bodyAst :: _
                | Ast.Vector bindingAstList :: bodyAst :: _ ->
                    let innerEnv = env.CreateInnerEnv()

                    for bindingAsts in List.chunkBySize 2 bindingAstList do
                        match bindingAsts with
                        | [ Ast.Symbol name; valueAst ] -> innerEnv.Set(name, evalAst innerEnv valueAst)
                        | _ -> evalError $"Invalid binding list in let*: {string ast}"

                    evalAst innerEnv bodyAst

                | _ -> evalError $"Invalid let*: {string ast}"

            | Ast.Symbol "do" :: asts ->
                (Ast.Nil, asts)
                ||> List.fold (fun _ ast -> evalAst env ast)

            | Ast.Symbol "if" :: asts ->
                match asts with
                | []
                | [ _ ] -> evalError $"Expected at least condition and true branch expression."
                | conditionAst :: trueBranchAst :: other ->
                    match evalAst env conditionAst with
                    | Ast.Nil
                    | Ast.Boolean false ->
                        match other with
                        | [] -> Ast.Nil
                        | falseBranchAst :: _ -> evalAst env falseBranchAst
                    | _ -> evalAst env trueBranchAst

            | Ast.Symbol "fn*" :: asts ->
                match asts with
                | Ast.List argumentNameAsts :: bodyAst :: _ ->
                    let argumentNames = argumentNameAsts |> List.map Ast.getSymbolName

                    let func argumentAsts =
                        if List.length argumentAsts
                           <> List.length argumentNames then
                            evalError $"Wrong number of arguments given to: {string ast}"
                        else
                            let innerEnv = env.CreateInnerEnv()

                            for argumentName, argumentAst in List.zip argumentNames argumentAsts do
                                innerEnv.Set(argumentName, argumentAst)

                            evalAst innerEnv bodyAst

                    Ast.Function func

                | _ -> evalError $"Invalid let*: {string ast}"

            | operationAst :: argumentAsts ->
                match evalAst env operationAst with
                | Ast.Function func -> func (List.map (evalAst env) argumentAsts)
                | _ -> evalError $"First list element should be a function: {string ast}"

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
