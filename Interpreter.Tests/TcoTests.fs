namespace Interpreter.Tests

open System.IO
open Xunit
open TestCaseRunner

module TcoTests =

    let testSet =
        Path.Combine(__SOURCE_DIRECTORY__, "TestSets", "step5_tco.mal")
        |> getTestSet

    [<Fact>]
    let ``Testing recursive tail-call function`` () =
        runTestSection testSet "Testing recursive tail-call function" 

    [<Fact>]
    let ``Test mutually recursive tail-call functions`` () =
        runTestSection testSet "Test mutually recursive tail-call functions" 