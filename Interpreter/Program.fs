namespace Interpreter

module Program =

    [<EntryPoint>]
    let main (args: string []) =
        match Array.toList args with
        | [] -> Interpreter([]).RunRepl()
        | fileName :: scriptArgs -> Interpreter(scriptArgs).RunScript(fileName, scriptArgs)

        0
