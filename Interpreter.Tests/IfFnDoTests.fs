namespace Interpreter.Tests

open System.IO
open Xunit
open TestCaseRunner

module IfFnDoTests =

    let testSet =
        Path.Combine(__SOURCE_DIRECTORY__, "TestSets", "step4_if_fn_do.mal")
        |> getTestSet

    [<Fact>]
    let ``Testing list functions`` () =
        runTestSection testSet "Testing list functions"

    [<Fact>]
    let ``Testing if form`` () =
        runTestSection testSet "Testing if form"

    [<Fact>]
    let ``Testing 1-way if form`` () =
        runTestSection testSet "Testing 1-way if form"

    [<Fact>]
    let ``Testing basic conditionals`` () =
        runTestSection testSet "Testing basic conditionals"

    [<Fact>]
    let ``Testing equality`` () =
        runTestSection testSet "Testing equality"

    [<Fact>]
    let ``Testing builtin and user defined functions`` () =
        runTestSection testSet "Testing builtin and user defined functions"

    [<Fact>]
    let ``Testing closures`` () =
        runTestSection testSet "Testing closures"

    [<Fact>]
    let ``Testing do form`` () =
        runTestSection testSet "Testing do form"

    [<Fact>]
    let ``Testing special form case-sensitivity`` () =
        runTestSection testSet "Testing special form case-sensitivity"

    [<Fact>]
    let ``Testing recursive sumdown function`` () =
        runTestSection testSet "Testing recursive sumdown function"

    [<Fact>]
    let ``Testing recursive fibonacci function`` () =
        runTestSection testSet "Testing recursive fibonacci function"

    [<Fact>]
    let ``Testing recursive function in environment.`` () =
        runTestSection testSet "Testing recursive function in environment."

    [<Fact>]
    let ``Testing if on strings`` () =
        runTestSection testSet "Testing if on strings"

    [<Fact>]
    let ``Testing string equality`` () =
        runTestSection testSet "Testing string equality"

    [<Fact>]
    let ``Testing variable length arguments`` () =
        runTestSection testSet "Testing variable length arguments"

    [<Fact>]
    let ``Testing language defined not function`` () =
        runTestSection testSet "Testing language defined not function"

    [<Fact>]
    let ``Testing string quoting`` () =
        runTestSection testSet "Testing string quoting"

    [<Fact>]
    let ``Testing pr-str`` () = runTestSection testSet "Testing pr-str"

    [<Fact>]
    let ``Testing str`` () = runTestSection testSet "Testing str"

    [<Fact>]
    let ``Testing prn`` () = runTestSection testSet "Testing prn"

    [<Fact>]
    let ``Testing println`` () =
        runTestSection testSet "Testing println"

    [<Fact>]
    let ``Testing keywords`` () =
        runTestSection testSet "Testing keywords"

    [<Fact>]
    let ``Testing vector truthiness`` () =
        runTestSection testSet "Testing vector truthiness"

    [<Fact>]
    let ``Testing vector printing`` () =
        runTestSection testSet "Testing vector printing"

    [<Fact>]
    let ``Testing vector functions`` () =
        runTestSection testSet "Testing vector functions"

    [<Fact>]
    let ``Testing vector equality`` () =
        runTestSection testSet "Testing vector equality"

    [<Fact>]
    let ``Testing vector parameter lists`` () =
        runTestSection testSet "Testing vector parameter lists"

    [<Fact>]
    let ``Nested vector/list equality`` () =
        runTestSection testSet "Nested vector/list equality"
