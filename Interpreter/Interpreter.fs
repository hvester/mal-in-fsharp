namespace Interpreter

type Interpreter() =

    let env = Env.CreateInitial()

    do
        "(def! not (fn* (x) (if x false true)))"
        |> Parser.read
        |> Evaluator.eval env
        |> ignore

    member _.Rep(input: string) =
        try
            Parser.read input
            |> Evaluator.eval env
            |> function
                | None -> ""
                | Some ast -> Printer.printAst true ast
        with
        | ParsingError msg -> $"Parsing error: {msg}"
        | ArgumentError msg -> $"Argument error: {msg}"
        | SymbolResolutionError symbolName -> $"{symbolName} not found."
        | EvaluationError (msg, ast) -> $"Evaluation error: {msg} {Printer.printAst true ast}"
