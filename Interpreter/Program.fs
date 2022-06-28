namespace Interpreter

module Program =

    let print (asts: Ast list) =
        String.concat "\n" (asts |> List.map string)

    let rep input =
        try
            Parser.read input |> Evaluator.eval |> print
        with
        | Parser.ParsingError msg -> $"Parsing error: {msg}"
        | Evaluator.EvaluationError msg -> $"Evaluation error: {msg}"

    [<EntryPoint>]
    let main (args: string []) =
        printf "user>"
        System.Console.ReadLine() |> rep |> printfn "%s"
        0
