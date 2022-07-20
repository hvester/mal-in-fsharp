namespace Interpreter

type Interpreter(?writeLine: string -> unit) =
    let writeLine = defaultArg writeLine (printfn "%s")
    let env = Core.createRootEnv writeLine

    do
        """
        (def! not (fn* (x) (if x false true)))
        (def! load-file (fn* (f) (eval (read-string (str "(do " (slurp f) "\nnil)")))))
        """
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
