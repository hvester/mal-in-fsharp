namespace Interpreter

module Core =

    let private integerOperation f asts =
        match asts with
        | []
        | _ :: [] -> raise (EvaluationError $"Expected at least two arguments.")
        | firstAst :: otherAsts ->
            (Ast.getIntegerValue firstAst, otherAsts)
            ||> List.fold (fun acc ast -> f acc (Ast.getIntegerValue ast))
            |> Ast.Integer

    let add = integerOperation (fun x y -> x + y)
    let substract = integerOperation (fun x y -> x - y)
    let multiply = integerOperation (fun x y -> x * y)
    let divide = integerOperation (fun x y -> x / y)

    let list asts = Ast.List asts

    let isList asts =
        match asts with
        | Ast.List _ :: _ -> Ast.Boolean true
        | _ -> Ast.Boolean false

    let isEmpty asts =
        match asts with
        | [] -> raise (EvaluationError $"Expected at least one argument.")
        | ast :: _ ->
            List.isEmpty (Ast.getListElements ast)
            |> Ast.Boolean

    let count asts =
        match asts with
        | Ast.List xs :: _ -> List.length xs |> Ast.Integer
        | _ -> Ast.Integer 0

    let areEqual asts =
        let rec twoAreEqual ast1 ast2 =
            match ast1, ast2 with
            | Ast.Nil, Ast.Nil -> true
            | Ast.Boolean x, Ast.Boolean y -> x = y
            | Ast.Integer x, Ast.Integer y -> x = y
            | Ast.String x, Ast.String y -> x = y
            | Ast.Keyword x, Ast.Keyword y -> x = y
            | Ast.Vector xs, Ast.Vector ys
            | Ast.List xs, Ast.List ys ->
                List.length xs = List.length ys
                && List.forall2 twoAreEqual xs ys

            | Ast.HashMap (HashMap x), Ast.HashMap (HashMap y) ->
                Map.count x = Map.count y
                && (x
                    |> Seq.forall (fun (KeyValue (k, v1)) ->
                        match Map.tryFind k y with
                        | None -> false
                        | Some v2 -> twoAreEqual v1 v2))

            | _ -> false

        match asts with
        | []
        | [ _ ] -> raise (EvaluationError $"Expected at least two arguments.")
        | ast1 :: ast2 :: _ -> twoAreEqual ast1 ast2 |> Ast.Boolean

    let private integerComparison compare asts =
        match asts with
        | []
        | [ _ ] -> raise (EvaluationError $"Expected at least two arguments.")
        | ast1 :: ast2 :: _ ->
            compare (Ast.getIntegerValue ast1) (Ast.getIntegerValue ast2)
            |> Ast.Boolean

    let lessThan = integerComparison (<)
    let lessThanOrEqual = integerComparison (<=)
    let greaterThan = integerComparison (>)
    let greaterThanOrEqual = integerComparison (>=)

    let prn (asts: Ast list) =
        match asts with
        | [] -> raise (EvaluationError $"Expected one argument.")
        | ast :: _ ->
            printfn "%s" (string ast)
            Ast.Nil