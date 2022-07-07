namespace Interpreter.Tests

open System.IO
open Xunit
open TestCaseRunner

module EnvTests =

    let testSet =
        Path.Combine(__SOURCE_DIRECTORY__, "TestSets", "step3_env.mal")
        |> getTestSet

    [<Fact>]
    let ``Testing REPL_ENV`` () =
        runTestSection testSet "Testing REPL_ENV" 

    [<Fact>]
    let ``Testing def!`` () =
        runTestSection testSet "Testing def!" 

    [<Fact>]
    let ``Verifying symbols are case-sensitive`` () =
        runTestSection testSet "Verifying symbols are case-sensitive" 

    [<Fact>]
    let ``Check env lookup non-fatal error`` () =
        runTestSection testSet "Check env lookup non-fatal error" 

    [<Fact>]
    let ``Check that error aborts def!`` () =
        runTestSection testSet "Check that error aborts def!" 

    [<Fact>]
    let ``Testing let*`` () =
        runTestSection testSet "Testing let*" 

    [<Fact>]
    let ``Testing outer environment`` () =
        runTestSection testSet "Testing outer environment" 

    [<Fact>]
    let ``Testing let* with vector bindings`` () =
        runTestSection testSet "Testing let* with vector bindings" 

    [<Fact>]
    let ``Testing vector evaluation`` () =
        runTestSection testSet "Testing vector evaluation" 

    [<Fact>]
    let ``Check that last assignment takes priority`` () =
        runTestSection testSet "Check that last assignment takes priority" 