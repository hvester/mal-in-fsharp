namespace Interpreter.Tests

open System
open Interpreter
open Xunit

module TestCaseRunner =


    type TestCase =
        { Input: string
          ExpectedOutput: Result<string, string> }

    type TestSection =
        { Header: string
          TestCases: TestCase list }


    module TestCaseParser =
        open System.IO
        open FParsec

        let pTestSectionHeader: Parser<string, unit> = skipString ";;" >>. restOfLine true

        let pExpectedOutput =
            skipString ";=>" >>. restOfLine true
            |>> (fun str -> str.Replace(@"\n", "\n") |> Result.Ok)

        let pExpectedError =
            skipString ";/" >>. restOfLine true
            |>> Result.Error

        let pTestCase: Parser<TestCase, unit> =
            restOfLine true
            .>>. (pExpectedOutput <|> pExpectedError)
            |>> (fun (input, expectedOutput) ->
                { Input = input
                  ExpectedOutput = expectedOutput })

        let pCommentLine: Parser<unit, unit> = skipString ";;;" .>> skipRestOfLine true

        let pTestCaseList =
            opt pCommentLine
            >>. (sepEndBy1 (attempt pTestCase) (many pCommentLine))

        let pTestSection =
            pTestSectionHeader .>>. pTestCaseList .>> spaces
            |>> (fun (header, testCases) ->
                { Header = header.Trim()
                  TestCases = testCases })

        let pTestSet = many1 pTestSection .>> spaces

        let parseTestCases filePath =
            let fileContent = File.ReadAllText(filePath)

            match run pTestSet fileContent with
            | Success (testSet, _, _) -> testSet
            | Failure (_, error, _) -> failwith (string error)


    let getTestSet filePath =
        lazy (TestCaseParser.parseTestCases filePath)

    let private getTestSection (testSet: Lazy<TestSection list>) header =
        testSet.Value
        |> List.find (fun x -> x.Header = header)

    let runParseTestSection testSet sectionHeader =
        let testSection = getTestSection testSet sectionHeader

        for testCase in testSection.TestCases do
            try
                let output = Parser.read testCase.Input |> Program.print

                match testCase.ExpectedOutput with
                | Ok expectedOutput -> Assert.Equal(expectedOutput, string output)
                | Error _ -> failwithf "Expected an error, got %s" output
            with
            | Parser.ParsingError _ ->
                match testCase.ExpectedOutput with
                | Error _ -> ()
                | Ok _ -> failwith "Parsing should have succeeded"

    let runTestSection testSet sectionHeader =
        let testSection = getTestSection testSet sectionHeader

        for testCase in testSection.TestCases do
            try
                let output =
                    Parser.read testCase.Input
                    |> Evaluator.eval
                    |> Program.print

                match testCase.ExpectedOutput with
                | Ok expectedOutput -> Assert.Equal(expectedOutput, string output)
                | Error _ -> failwithf "Expected an error, got %s" output
            with
            | Evaluator.EvaluationError _ ->
                match testCase.ExpectedOutput with
                | Error _ -> ()
                | Ok _ -> failwith "Evaluation should have succeeded"
