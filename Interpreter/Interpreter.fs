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

    let rep input =
        try
            Parser.read input
            |> Evaluator.eval env
            |> Option.map (Printer.printAst true)
        with
        | ParsingError msg -> Some $"Parsing error: {msg}"
        | ArgumentError msg -> Some $"Argument error: {msg}"
        | SymbolResolutionError symbolName -> Some $"{symbolName} not found."
        | EvaluationError (msg, ast) -> Some $"Evaluation error: {msg} {Printer.printAst true ast}"

    member _.RunRepl() =
        let rec loop () =
            printf "user>"

            match System.Console.ReadLine() with
            | "#quit" -> ()
            | input ->
                rep input |> Option.iter (printfn "%s")
                loop ()

        loop ()

    member _.RunScript(scriptFilePath, args) =
        rep $"""(load-file "{scriptFilePath}")""" |> ignore
