namespace Interpreter.Tests

open System.IO
open Xunit
open TestCaseRunner

module EvalTests =

    let testSet =
        Path.Combine(__SOURCE_DIRECTORY__, "TestSets", "step2_eval.mal")
        |> getTestSet

    [<Fact>]
    let ``Testing evaluation of arithmetic operations`` () =
        runTestSection testSet "Testing evaluation of arithmetic operations"

    [<Fact>]
    let ``Testing empty list`` () =
        runTestSection testSet "Testing empty list"

    [<Fact>]
    let ``Testing evaluation within collection literals`` () =
        runTestSection testSet "Testing evaluation within collection literals"

    [<Fact>]
    let ``Check that evaluation hasn't broken empty collections`` () =
        runTestSection testSet "Check that evaluation hasn't broken empty collections"
