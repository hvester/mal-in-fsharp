namespace Interpreter

type Interpreter() =

    let env = Env.CreateInitial()

    let print (astOpt: Ast option) =
        match astOpt with
        | None -> ""
        | Some ast -> string ast

    do
        "(def! not (fn* (x) (if x false true)))"
        |> Parser.read
        |> Evaluator.eval env
        |> ignore

    member _.Rep(input: string) =
        try
            Parser.read input |> Evaluator.eval env |> print
        with
        | ParsingError msg -> $"Parsing error: {msg}"
        | EvaluationError msg -> $"Evaluation error: {msg}"
