namespace Interpreter

module Program =

    [<EntryPoint>]
    let main (args: string []) =
        let interpreter = Interpreter()

        match Array.toList args with
        | [] -> interpreter.RunRepl()
        | fileName :: scriptArgs -> interpreter.RunScript(fileName, scriptArgs)

        0
