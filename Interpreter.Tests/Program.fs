namespace Interpreter.Tests

open System.IO
open TestCaseRunner

module Program =


    let generateTestsForFile filePath =
        let testSet =
            Path.Combine(__SOURCE_DIRECTORY__, filePath)
            |> getTestSet        
        for tc in testSet.Value do
            (*
            printfn $"{tc.Header}"

            for x in tc.TestCases do
                printfn "Inputs:"
                for inp in x.Inputs do
                    printfn "%s" inp
                printfn "Expected output: %A" x.ExpectedOutput
            *)

            printfn $"""
    [<Fact>]
    let ``{tc.Header}`` () =
        runTestSection testSet "{tc.Header}" """


    let [<EntryPoint>] main arg =
        generateTestsForFile arg[0]
        0
