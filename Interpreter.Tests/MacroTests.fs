namespace Interpreter.Tests

open System.IO
open Xunit
open TestCaseRunner

module MacroTests =

    let testSet =
        Path.Combine(__SOURCE_DIRECTORY__, "TestSets", "step8_macros.mal")
        |> getTestSet

    [<Fact>]
    let ``Testing trivial macros`` () =
        runTestSection testSet "Testing trivial macros" 

    [<Fact>]
    let ``Testing unless macros`` () =
        runTestSection testSet "Testing unless macros" 

    [<Fact>]
    let ``Testing macroexpand`` () =
        runTestSection testSet "Testing macroexpand" 

    [<Fact>]
    let ``Testing evaluation of macro result`` () =
        runTestSection testSet "Testing evaluation of macro result" 

    [<Fact>]
    let ``Test that macros do not break empty list`` () =
        runTestSection testSet "Test that macros do not break empty list" 

    [<Fact>]
    let ``Test that macros do not break quasiquote`` () =
        runTestSection testSet "Test that macros do not break quasiquote" 

    [<Fact>]
    let ``Testing non-macro function`` () =
        runTestSection testSet "Testing non-macro function" 

    [<Fact>]
    let ``Testing nth, first and rest functions`` () =
        runTestSection testSet "Testing nth, first and rest functions" 

    [<Fact>]
    let ``Testing cond macro`` () =
        runTestSection testSet "Testing cond macro" 

    [<Fact>]
    let ``Testing EVAL in let*`` () =
        runTestSection testSet "Testing EVAL in let*" 

    [<Fact>]
    let ``Testing nth, first, rest with vectors`` () =
        runTestSection testSet "Testing nth, first, rest with vectors" 

    [<Fact>]
    let ``Testing EVAL in vector let*`` () =
        runTestSection testSet "Testing EVAL in vector let*" 

    [<Fact>]
    let ``Test that macros use closures`` () =
        runTestSection testSet "Test that macros use closures" 