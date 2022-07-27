namespace Interpreter

type Interpreter(commandLineArguments, ?writeLine: string -> unit) =

    let writeLine = defaultArg writeLine (printfn "%s")
    let env = Core.createRootEnv writeLine

    let argsString = commandLineArguments |> List.map (sprintf "\"%s\"") |> String.concat " "

    do
        $"""
        (def! not (fn* (x) (if x false true)))
        (def! load-file (fn* (f) (eval (read-string (str "(do " (slurp f) "\nnil)")))))
        (def! *ARGV* (list {argsString}))
        (defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw "odd number of forms to cond")) (cons 'cond (rest (rest xs)))))))
        """
        |> Parser.read
        |> Evaluator.eval env
        |> ignore

    member _.Rep(input) =
        try
            Parser.read input
            |> Evaluator.eval env
            |> Option.map (Printer.printAst true)
        with
        | ParsingError msg -> Some $"Parsing error: {msg}"
        | ArgumentError msg -> Some $"Argument error: {msg}"
        | SymbolResolutionError symbolName -> Some $"{symbolName} not found."
        | EvaluationError (msg, ast) -> Some $"Evaluation error: {msg} {Printer.printAst true ast}"

    member this.RunRepl() =
        let rec loop () =
            printf "user>"

            match System.Console.ReadLine() with
            | "#quit" -> ()
            | input ->
                this.Rep(input) |> Option.iter (printfn "%s")
                loop ()

        loop ()

    member this.RunScript(scriptFilePath) =
        this.Rep($"""(load-file "{scriptFilePath}")""")
        |> ignore
