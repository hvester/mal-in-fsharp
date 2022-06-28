namespace Interpreter.Tests

open System.IO
open Xunit
open TestCaseRunner

module ReadPrintTests =


    let testSet =
        Path.Combine(__SOURCE_DIRECTORY__, "TestSets", "step1_read_print.mal")
        |> getTestSet

    [<Fact>]
    let ``Testing read of numbers`` () =
        runParseTestSection testSet "Testing read of numbers"

    [<Fact>]
    let ``Testing read of symbols`` () =
        runParseTestSection testSet "Testing read of symbols"

    [<Fact>]
    let ``Testing non-numbers starting with a dash.`` () =
        runParseTestSection testSet "Testing non-numbers starting with a dash."

    [<Fact>]
    let ``Testing read of lists`` () =
        runParseTestSection testSet "Testing read of lists"

    [<Fact>]
    let ``Test commas as whitespace`` () =
        runParseTestSection testSet "Test commas as whitespace"

    [<Fact>]
    let ``Testing read of nil/true/false`` () =
        runParseTestSection testSet "Testing read of nil/true/false"

    [<Fact>]
    let ``Testing read of strings`` () =
        runParseTestSection testSet "Testing read of strings"

    [<Fact>]
    let ``Testing reader errors`` () =
        runParseTestSection testSet "Testing reader errors"

    [<Fact>]
    let ``These should throw some error with no return value`` () =
        runParseTestSection testSet "These should throw some error with no return value"

    [<Fact>]
    let ``Testing read of quoting`` () =
        runParseTestSection testSet "Testing read of quoting"

    [<Fact>]
    let ``Testing keywords`` () =
        runParseTestSection testSet "Testing keywords"

    [<Fact>]
    let ``Testing read of vectors`` () =
        runParseTestSection testSet "Testing read of vectors"

    [<Fact>]
    let ``Testing read of hash maps`` () =
        runParseTestSection testSet "Testing read of hash maps"

    [<Fact>]
    let ``Testing read of comments`` () =
        runParseTestSection testSet "Testing read of comments"

    [<Fact>]
    let ``Testing read of deref`` () =
        runParseTestSection testSet "Testing read of @/deref"

    [<Fact(Skip = "Metadata not implemented")>]
    let ``Testing read of ^/metadata`` () =
        runParseTestSection testSet "Testing read of ^/metadata"

    [<Fact>]
    let ``Non alphanumerice characters in strings`` () =
        runParseTestSection testSet "Non alphanumerice characters in strings"

    [<Fact>]
    let ``Non alphanumeric characters in comments`` () =
        runParseTestSection testSet "Non alphanumeric characters in comments"
