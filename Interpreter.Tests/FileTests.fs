namespace Interpreter.Tests

open System.IO
open Xunit
open TestCaseRunner

module FileTests =

    let testSet =
        Path.Combine(__SOURCE_DIRECTORY__, "TestSets", "step6_file.mal")
        |> getTestSet

    [<Fact>]
    let ``Testing that (do (do)) not broken by TCO`` () =
        runTestSection testSet "Testing that (do (do)) not broken by TCO" 


    [<Fact>]
    let ``Testing read-string, eval and slurp`` () =
        runTestSection testSet "Testing read-string, eval and slurp" 

    [<Fact>]
    let ``Testing load-file`` () =
        runTestSection testSet "Testing load-file" 

    [<Fact>]
    let ``Testing atoms`` () =
        runTestSection testSet "Testing atoms" 

    [<Fact>]
    let ``Testing swap!/closure interaction`` () =
        runTestSection testSet "Testing swap!/closure interaction" 

    [<Fact>]
    let ``Testing whether closures can retain atoms`` () =
        runTestSection testSet "Testing whether closures can retain atoms" 

    [<Fact>]
    let ``Testing reading of large files`` () =
        runTestSection testSet "Testing reading of large files" 

    [<Fact>]
    let ``Testing reader macro (short for `deref`)`` () =
        runTestSection testSet "Testing `@` reader macro (short for `deref`)" 

    [<Fact>]
    let ``Testing that vector params not broken by TCO`` () =
        runTestSection testSet "Testing that vector params not broken by TCO" 

    [<Fact>]
    let ``Testing that *ARGV* exists and is an empty list`` () =
        runTestSection testSet "Testing that *ARGV* exists and is an empty list" 

    [<Fact>]
    let ``Testing that eval sets aa in root scope, and that it is found in nested scope`` () =
        runTestSection testSet "Testing that eval sets aa in root scope, and that it is found in nested scope" 

    [<Fact>]
    let ``Testing comments in a file`` () =
        runTestSection testSet "Testing comments in a file" 

    [<Fact>]
    let ``Testing map literal across multiple lines in a file`` () =
        runTestSection testSet "Testing map literal across multiple lines in a file" 

    [<Fact>]
    let ``Checking that eval does not use local environments.`` () =
        runTestSection testSet "Checking that eval does not use local environments." 

    [<Fact>]
    let ``Non alphanumeric characters in comments in read-string`` () =
        runTestSection testSet "Non alphanumeric characters in comments in read-string"
