namespace Interpreter

module Program =

    [<EntryPoint>]
    let main (args: string []) =
        let interpreter = Interpreter()

        let rec loop () =
            printf "user>"

            match System.Console.ReadLine() with
            | "quit" -> 0
            | input ->
                interpreter.Rep(input) |> printfn "%s"
                loop ()

        loop ()
