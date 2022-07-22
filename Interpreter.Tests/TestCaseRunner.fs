namespace Interpreter.Tests

open System
open Interpreter
open Xunit

module TestCaseRunner =

    type ExpectedOutput =
        | ExactOutput of string
        | ShouldMatchRegex of string

    type TestCase =
        { Inputs: string list
          ExpectedPrint: ExpectedOutput list
          ExpectedResult: ExpectedOutput }

    type TestSection =
        { Header: string
          TestCases: TestCase list }


    module TestCaseParser =
        open System.IO
        open FParsec

        let pTestSectionHeader: Parser<string, unit> = skipString ";;" >>. restOfLine true

        let pExactOutput =
            skipString ";=>" >>. restOfLine true
            |>> ExactOutput

        let pShouldMatchRegex =
            skipString ";/" >>. restOfLine true
            |>> ShouldMatchRegex

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
            .>>. (many1 (pExactOutput <|> pShouldMatchRegex))
            |>> (fun (inputs, outputs) ->
                { Inputs = inputs
                  ExpectedPrint = outputs[.. outputs.Length - 2]
                  ExpectedResult = List.last outputs })

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

    let assertCorrectOutput expectedOutput output =
        match expectedOutput with
        | ExactOutput expectedExactOutput -> Assert.Equal(expectedExactOutput, output)
        | ShouldMatchRegex regex -> Assert.Matches(regex, output)

    let runParseTestSection testSet sectionHeader =
        let _, testSection = getTestSection testSet sectionHeader

        for testCase in testSection.TestCases do
            let output =
                try
                    testCase.Inputs
                    |> List.exactlyOne
                    |> Parser.read
                    |> function
                        | [] -> ""
                        | [ ast ] -> Printer.printAst true ast
                        | _ -> failwith "Got more than one AST"
                with
                | ParsingError msg -> msg

            assertCorrectOutput testCase.ExpectedResult output


    let runTestSection testSet sectionHeader =
        let sectionsBefore, testSection = getTestSection testSet sectionHeader

        let output = ResizeArray<string>()
        let writeLine (line: string) =
            for s in line.Split('\n') do
                output.Add(s)

        let interpreter = Interpreter([], writeLine)

        for section in sectionsBefore do
            for testCase in section.TestCases do
                for input in testCase.Inputs do
                    interpreter.Rep(input) |> ignore

        for testCase in testSection.TestCases do
            output.Clear()

            let result =
                ("", testCase.Inputs)
                ||> List.fold (fun _ input -> 
                    interpreter.Rep(input)
                    |> Option.defaultValue "")

            Assert.True(
                testCase.ExpectedPrint.Length = output.Count,
                $"Wrong number of outputs. Expected: %A{testCase.ExpectedPrint}, actual: %A{output}"
            )

            for expectedOutput, output in Seq.zip testCase.ExpectedPrint output do
                assertCorrectOutput expectedOutput output

            assertCorrectOutput testCase.ExpectedResult result
