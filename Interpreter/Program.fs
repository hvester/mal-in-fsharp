namespace Interpreter

module Program =

    let eval x = x

    let print (asts : Ast list) =
        String.concat "\n" (asts |> List.map string)

    let rep input =
        match Parser.read input with
        | Ok asts -> asts |> eval |> print |> Ok
        | Error e -> Error e 

    let outputResultToString result =
        match result with
        | Ok output -> output
        | Error error -> sprintf "ERROR: %s" error

    [<EntryPoint>]
    let main (args: string[]) =
        printf "user>"
        System.Console.ReadLine()
        |> rep
        |> outputResultToString
        |> printfn "%s"
        0