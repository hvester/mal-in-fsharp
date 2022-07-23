namespace Interpreter.Tests

open System.IO
open Xunit
open TestCaseRunner

module QuoteTests =

    let testSet =
        Path.Combine(__SOURCE_DIRECTORY__, "TestSets", "step7_quote.mal")
        |> getTestSet

    [<Fact>]
    let ``Testing cons function`` () =
        runTestSection testSet "Testing cons function" 

    [<Fact>]
    let ``Testing concat function`` () =
        runTestSection testSet "Testing concat function" 


    [<Fact>]
    let ``Testing regular quote`` () =
        runTestSection testSet "Testing regular quote" 

    [<Fact>]
    let ``Testing simple quasiquote`` () =
        runTestSection testSet "Testing simple quasiquote" 

    [<Fact>]
    let ``Testing quasiquote with lists`` () =
        runTestSection testSet "Testing quasiquote with lists" 

    [<Fact>]
    let ``Testing unquote`` () =
        runTestSection testSet "Testing unquote" 

    [<Fact>]
    let ``Quasiquote and environments`` () =
        runTestSection testSet "Quasiquote and environments" 

    [<Fact>]
    let ``Testing splice-unquote`` () =
        runTestSection testSet "Testing splice-unquote" 

    [<Fact>]
    let ``Testing symbol equality`` () =
        runTestSection testSet "Testing symbol equality" 

    [<Fact>]
    let ``Testing quote reader macro`` () =
        runTestSection testSet "Testing ' (quote) reader macro" 

    [<Fact>]
    let ``Testing cons and concat with vectors`` () =
        runTestSection testSet "Testing cons and concat with vectors" 

    [<Fact>]
    let ``Testing quasiquote reader macro`` () =
        runTestSection testSet "Testing ` (quasiquote) reader macro" 

    [<Fact>]
    let ``Testing unquote reader macro`` () =
        runTestSection testSet "Testing ~ (unquote) reader macro" 

    [<Fact>]
    let ``Testing splice-unquote reader macro`` () =
        runTestSection testSet "Testing ~@ (splice-unquote) reader macro" 

    [<Fact>]
    let ``Testing vec function`` () =
        runTestSection testSet "Testing vec function" 

    [<Fact>]
    let ``Testing that vec does not mutate the original list`` () =
        runTestSection testSet "Testing that vec does not mutate the original list" 

    [<Fact>]
    let ``Test quine`` () =
        runTestSection testSet "Test quine" 

    [<Fact>]
    let ``Testing quasiquote with vectors`` () =
        runTestSection testSet "Testing quasiquote with vectors" 

    [<Fact>]
    let ``Testing unquote with vectors`` () =
        runTestSection testSet "Testing unquote with vectors" 

    [<Fact>]
    let ``Testing splice-unquote with vectors`` () =
        runTestSection testSet "Testing splice-unquote with vectors" 

    [<Fact>]
    let ``Misplaced unquote or splice-unquote`` () =
        runTestSection testSet "Misplaced unquote or splice-unquote" 

    [<Fact>]
    let ``Debugging quasiquote`` () =
        runTestSection testSet "Debugging quasiquote" 
