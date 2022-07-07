namespace Interpreter.Tests

open System
open Interpreter
open Xunit

module TestCaseRunner =


    type TestCase =
        { Inputs: string list
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

        let pEmptyLine: Parser<unit, unit> =
            attempt (many (anyOf [ ' '; '\t' ]) >>. skipChar '\n')

        let pTestCase: Parser<TestCase, unit> =
            sepEndBy1
                (notFollowedBy (
                    skipString ";=>"
                    <|> skipString ";/"
                    <|> pEmptyLine
                 )
                 >>. restOfLine false)
                (pchar '\n')
            .>>. (pExpectedOutput <|> pExpectedError)
            |>> (fun (inputs, expectedOutput) ->
                { Inputs = inputs
                  ExpectedOutput = expectedOutput })

        let pCommentLine: Parser<unit, unit> = skipString ";;;" .>> skipRestOfLine true

        let pTestCaseList =
            opt pCommentLine
            >>. (sepEndBy1 (attempt pTestCase) (many pCommentLine))

        let pTestSection =
            pTestSectionHeader .>>. pTestCaseList
            |>> (fun (header, testCases) ->
                { Header = header.Trim()
                  TestCases = testCases })

        let pTestSet = sepEndBy1 pTestSection (many1 pEmptyLine)

        let parseTestCases filePath =
            let fileContent = File.ReadAllText(filePath)

            match run (pTestSet .>> spaces .>> eof) fileContent with
            | Success (testSet, _, _) -> testSet
            | Failure (_, error, _) -> failwith (string error)


    let getTestSet filePath =
        lazy (TestCaseParser.parseTestCases filePath)

    let private getTestSection (testSet: Lazy<TestSection list>) header =
        let sectionsBefore =
            testSet.Value
            |> List.takeWhile (fun x -> x.Header <> header)

        let testSection =
            testSet.Value
            |> List.find (fun x -> x.Header = header)

        (sectionsBefore, testSection)

    let runParseTestSection testSet sectionHeader =
        let _, testSection = getTestSection testSet sectionHeader

        for testCase in testSection.TestCases do
            try
                let output =
                    testCase.Inputs
                    |> String.concat "\n"
                    |> Parser.read
                    |> List.map string
                    |> String.concat " "

                match testCase.ExpectedOutput with
                | Ok expectedOutput -> Assert.Equal(expectedOutput, string output)
                | Error _ -> failwithf "Expected an error, got %s" output
            with
            | Parser.ParsingError _ ->
                match testCase.ExpectedOutput with
                | Error _ -> ()
                | Ok _ -> failwith "Parsing should have succeeded"

    let runTestSection testSet sectionHeader =
        let sectionsBefore, testSection = getTestSection testSet sectionHeader
        let interpreter = Interpreter()

        for section in sectionsBefore do
            for testCase in section.TestCases do
                interpreter.Rep(String.concat "\n" testCase.Inputs)
                |> ignore

        for testCase in testSection.TestCases do
            let mutable output = ""

            for input in testCase.Inputs do
                output <- interpreter.Rep(input)

            match testCase.ExpectedOutput with
            | Ok expectedOutput -> Assert.Equal(expectedOutput, output)
            | Error _ -> Assert.Contains("error", output)
