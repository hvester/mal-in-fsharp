namespace Interpreter

module Evaluator =

    exception EvaluationError of string

    type Env = Map<string, Ast -> Ast -> Ast>

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

    let initialEnv =
        [ "+", add
          "-", substract
          "*", multiply
          "/", divide ]
        |> Map.ofList

    let rec evalAst (env: Env) (ast: Ast) =
        match ast with
        | Ast.List asts ->
            match asts with
            | [] -> (ast, env)
            | [ Ast.Symbol symbolName; ast1; ast2 ] ->
                match Map.tryFind symbolName env with
                | None ->
                    sprintf "Symbol %s cannot be resolved" symbolName
                    |> EvaluationError
                    |> raise
                | Some operation ->
                    let arg1, _ = evalAst env ast1
                    let arg2, _ = evalAst env ast2
                    let result = operation arg1 arg2
                    (result, env)

            | _ ->
                sprintf "Cannot evaluate list: %s" (string ast)
                |> EvaluationError
                |> raise

        | Ast.Vector asts ->
            let result =
                asts
                |> List.map (evalAst env >> fst)
                |> Ast.Vector

            (result, env)

        | Ast.HashMap (HashMap m) ->
            let result =
                Map.map (fun _ v -> evalAst env v |> fst) m
                |> HashMap
                |> Ast.HashMap

            (result, env)

        | _ -> (ast, env)

    let eval asts =
        List.mapFold evalAst initialEnv asts |> fst
