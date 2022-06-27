namespace Interpreter.Tests

open System
open System.IO
open FParsec

module TestCaseParser =

    type TestCase =
        { Input: string
          ExpectedOutput: Result<string, string> }

    type TestSection =
        { Header: string
          TestCases: TestCase list }

    let pTestSectionHeader: Parser<string, unit> = skipString ";;" >>. restOfLine true

    let pExpectedOutput =
        skipString ";=>" >>. restOfLine true
        |>> (fun str -> str.Replace(@"\n", "\n") |> Result.Ok)

    let pExpectedError =
        skipString ";/" >>. restOfLine true
        |>> Result.Error

    let pTestCase: Parser<TestCase, unit> =
        restOfLine true // PROBLEM IS THAT THIS ACCEPTS ALSO EMPTY LINES
        .>>. (pExpectedOutput <|> pExpectedError)
        |>> fun (input, expectedOutput) ->
                { Input = input
                  ExpectedOutput = expectedOutput }

    let pCommentLine: Parser<unit, unit> = skipString ";;;" .>> skipRestOfLine true

    let pTestCaseList =
        opt pCommentLine
        >>. (sepEndBy1 (attempt pTestCase) (many pCommentLine))

    let pTestSection =
        pTestSectionHeader .>>. pTestCaseList .>> spaces
        |>> fun (header, testCases) ->
                { Header = header.Trim()
                  TestCases = testCases }

    let pTestSet = many1 pTestSection .>> spaces

    let parseTestCases filePath =
        let fileContent = File.ReadAllText(filePath)

        match run pTestSet fileContent with
        | Success (testSet, _, _) -> testSet
        | Failure (_, error, _) -> failwith (string error)
