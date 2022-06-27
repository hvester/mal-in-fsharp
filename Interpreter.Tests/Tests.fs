namespace Interpreter.Tests

open System
open System.IO
open Xunit
open TestCaseParser
open Interpreter

module ReadPrintTests =

    let private testFilePath =
        Path.Combine(__SOURCE_DIRECTORY__, "TestSets", "step1_read_print.mal")

    let testSet = lazy (parseTestCases testFilePath)

    let getTestSection header =
        testSet.Value
        |> List.find (fun x -> x.Header = header)

    let runTestSection sectionHeader =
        for testCase in (getTestSection sectionHeader).TestCases do
            printfn "Test input>%s" testCase.Input

            match Program.rep testCase.Input, testCase.ExpectedOutput with
            | Error _, Error _ -> printfn "Got expected error"
            | output, expectedOutput ->
                let consolePrint = Program.outputResultToString output
                printfn ";=>%s" consolePrint
                Assert.Equal(Program.outputResultToString expectedOutput, consolePrint)

    (*
    [<Fact>]
    let printTest () =
        for tc in testSet.Value do
            printfn $"""
    [<Fact>]
    let ``{tc.Header}`` () =
        runTestSection "{tc.Header}"
"""
*)

    [<Fact>]
    let ``Testing read of numbers`` () =
        runTestSection "Testing read of numbers"

    [<Fact>]
    let ``Testing read of symbols`` () =
        runTestSection "Testing read of symbols"

    [<Fact>]
    let ``Testing non-numbers starting with a dash.`` () =
        runTestSection "Testing non-numbers starting with a dash."

    [<Fact>]
    let ``Testing read of lists`` () = runTestSection "Testing read of lists"

    [<Fact>]
    let ``Test commas as whitespace`` () =
        runTestSection "Test commas as whitespace"

    [<Fact>]
    let ``Testing read of nil/true/false`` () =
        runTestSection "Testing read of nil/true/false"

    [<Fact>]
    let ``Testing read of strings`` () =
        runTestSection "Testing read of strings"

    [<Fact>]
    let ``Testing reader errors`` () = runTestSection "Testing reader errors"

    [<Fact>]
    let ``These should throw some error with no return value`` () =
        runTestSection "These should throw some error with no return value"

    [<Fact>]
    let ``Testing read of quoting`` () =
        runTestSection "Testing read of quoting"

    [<Fact>]
    let ``Testing keywords`` () = runTestSection "Testing keywords"

    [<Fact>]
    let ``Testing read of vectors`` () =
        runTestSection "Testing read of vectors"

    [<Fact>]
    let ``Testing read of hash maps`` () =
        runTestSection "Testing read of hash maps"

    [<Fact>]
    let ``Testing read of comments`` () =
        runTestSection "Testing read of comments"

    [<Fact>]
    let ``Testing read of deref`` () =
        runTestSection "Testing read of @/deref"

(*
    [<Fact>]
    let ``Testing read of ^/metadata`` () =
        runTestSection "Testing read of ^/metadata"
*)

    [<Fact>]
    let ``Non alphanumerice characters in strings`` () =
        runTestSection "Non alphanumerice characters in strings"

    [<Fact>]
    let ``Non alphanumeric characters in comments`` () =
        runTestSection "Non alphanumeric characters in comments"
