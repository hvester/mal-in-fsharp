namespace Interpreter

module Evaluator =

    let evalError msg ast = raise (EvaluationError(msg, ast))

    let createInnerEnv outerEnv symbolNames values =
        let innerEnv = Env(Some outerEnv)

        let rec loop =
            function
            | [], _
            | "&" :: [], _ -> ()

            | "&" :: symbolName :: _, restValues ->
                innerEnv.Set(symbolName, Ast.List restValues)

            | symbolName :: remainingSymbolNames, value :: remainingValues ->
                innerEnv.Set(symbolName, value)
                loop (remainingSymbolNames, remainingValues)

            | symbolName :: remainingSymbols, [] ->
                innerEnv.Set(symbolName, Ast.Nil)
                loop (remainingSymbols, [])

        loop (symbolNames, values)
        innerEnv


    let rec evalAst (env: Env) (ast: Ast) : Ast =
        match ast with
        | Ast.Symbol symbolName -> env.Resolve(symbolName)

        | Ast.List asts ->
            match asts with
            | [] -> ast

            | Ast.Symbol "def!" :: asts ->
                match asts with
                | symbol :: valueAst :: _ ->
                    let value = evalAst env valueAst
                    env.Set(Ast.unwrapSymbol symbol, value)
                    value
                | _ -> evalError $"Invalid def!" ast

            | Ast.Symbol "let*" :: asts ->
                match asts with
                | Ast.List bindingAstList :: bodyAst :: _
                | Ast.Vector bindingAstList :: bodyAst :: _ ->

                    let innerEnv = Env(Some env)

                    for bindingAsts in List.chunkBySize 2 bindingAstList do
                        match bindingAsts with
                        | [ Ast.Symbol symbolName; valueAst ] ->
                            innerEnv.Set(symbolName, evalAst innerEnv valueAst)
                        | _ -> evalError "Invalid binding list in let*" ast

                    evalAst innerEnv bodyAst

                | _ -> evalError $"Invalid let*" ast

            | Ast.Symbol "do" :: asts ->
                if List.isEmpty asts then
                    Ast.Nil
                else
                    let otherAsts, lastAst = List.splitAt (asts.Length - 1) asts
                    otherAsts |> List.iter (evalAst env >> ignore)
                    evalAst env lastAst[0]

            | Ast.Symbol "if" :: asts ->
                match asts with
                | []
                | [ _ ] -> evalError "Expected at least condition and true branch expression" ast
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
                | Ast.Vector argumentNameAsts :: bodyAst :: _
                | Ast.List argumentNameAsts :: bodyAst :: _ ->
                    let argumentNames = List.map Ast.unwrapSymbol argumentNameAsts
                    Ast.UserDefinedFunction(env, argumentNames, bodyAst)

                | _ -> evalError "Invalid let*" ast

            | Ast.Symbol "quote" :: asts ->
                List.tryHead asts
                |> Option.defaultWith (fun () -> evalError "Invalid quote" ast)

            | operationAst :: argumentAsts ->
                match evalAst env operationAst with
                | Ast.CoreFunction func ->
                    argumentAsts
                    |> List.map (evalAst env)
                    |> func

                | Ast.UserDefinedFunction(outerEnv, argumentNames, body) ->
                    let functionEnv =
                        argumentAsts
                        |> List.map (evalAst env)
                        |> createInnerEnv outerEnv argumentNames

                    evalAst functionEnv body

                | _ -> evalError "First list element should be a function" ast

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
