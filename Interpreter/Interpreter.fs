namespace Interpreter

type Interpreter() =

    let env = Evaluator.createInitialEnv ()

    let print (astOpt: Ast option) =
        match astOpt with
        | None -> ""
        | Some ast -> string ast

    member _.Rep(input: string) =
        try
            Parser.read input |> Evaluator.eval env |> print
        with
        | ParsingError msg -> $"Parsing error: {msg}"
        | EvaluationError msg -> $"Evaluation error: {msg}"
