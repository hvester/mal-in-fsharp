namespace Interpreter

module Evaluator =

    let evalError msg ast = raise (EvaluationError(msg, ast))

    let rec evalAst (env: Env) (ast: Ast) =
        match ast with
        | Ast.Symbol symbolName -> env.Resolve(symbolName)

        | Ast.List asts ->
            match asts with
            | [] ->
             ast

            | Ast.Symbol "def!" :: asts ->
                match asts with
                | symbol :: valueAst :: _ ->
                    let value = evalAst env valueAst
                    env.Set(symbol, value)
                    value
                | _ -> evalError $"Invalid def!" ast

            | Ast.Symbol "let*" :: asts ->
                match asts with
                | Ast.List bindingAstList :: bodyAst :: _
                | Ast.Vector bindingAstList :: bodyAst :: _ ->

                    let innerEnv = env.CreateInner([], [])

                    for bindingAsts in List.chunkBySize 2 bindingAstList do
                        match bindingAsts with
                        | [ symbol; valueAst ] -> innerEnv.Set(symbol, evalAst innerEnv valueAst)
                        | _ -> evalError "Invalid binding list in let*" ast

                    evalAst innerEnv bodyAst

                | _ -> evalError $"Invalid let*" ast

            | Ast.Symbol "do" :: asts ->
                (Ast.Nil, asts)
                ||> List.fold (fun _ ast -> evalAst env ast)

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
                | (Ast.Vector argumentNameAsts | Ast.List argumentNameAsts) :: bodyAst :: _ ->
                    let func argumentAsts =
                        let innerEnv = env.CreateInner(argumentNameAsts, argumentAsts)
                        evalAst innerEnv bodyAst

                    Ast.Function func

                | _ -> evalError "Invalid let*" ast

            | operationAst :: argumentAsts ->
                match evalAst env operationAst with
                | Ast.Function func -> func (List.map (evalAst env) argumentAsts)
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
