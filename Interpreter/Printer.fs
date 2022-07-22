namespace Interpreter

open System.Text

module rec Printer =

    let appendString (sb: StringBuilder) (printReadably: bool) (str: string) =
        if printReadably then
            sb.Append('"') |> ignore

            for c in str.ToCharArray() do
                match c with
                | '\\' -> sb.Append(@"\\") |> ignore
                | '"' -> sb.Append("\\\"") |> ignore
                | '\n' -> sb.Append(@"\n") |> ignore
                | c -> sb.Append(c) |> ignore

            sb.Append('"') |> ignore
        else
            sb.Append(str) |> ignore


    let appendKeywordString (sb: StringBuilder) (KeywordString s) =
        sb.Append(':') |> ignore
        sb.Append(s) |> ignore

    let appendHashMapKey (sb: StringBuilder) printReadably hashMapKey =
        match hashMapKey with
        | StringKey key -> appendString sb printReadably key
        | KeywordKey key -> appendKeywordString sb key

    let appendJoin appendValue appendSeparator values =
        (false, values)
        ||> Seq.fold (fun shouldAppendSeparator value ->
            if shouldAppendSeparator then
                appendSeparator ()

            appendValue value
            true)
        |> ignore

    let appendHashMap (sb: StringBuilder) printReadably (HashMap keyValuePairs) =
        sb.Append('{') |> ignore

        appendJoin
            (fun (KeyValue (key, value)) ->
                appendHashMapKey sb printReadably key
                sb.Append(' ') |> ignore
                appendAst sb printReadably value)
            (fun () -> sb.Append(' ') |> ignore)
            keyValuePairs

        sb.Append('}') |> ignore

    let appendAstList (sb: StringBuilder) printReadably asts =
        appendJoin (appendAst sb printReadably) (fun () -> sb.Append(' ') |> ignore) asts

    let appendAst (sb: StringBuilder) printReadably ast =
        match ast with
        | Ast.Nil -> sb.Append("nil") |> ignore
        | Ast.Boolean b -> sb.Append(if b then "true" else "false") |> ignore
        | Ast.Integer i -> sb.Append(i) |> ignore
        | Ast.Symbol s -> sb.Append(s) |> ignore
        | Ast.String s -> appendString sb printReadably s
        | Ast.Keyword keyword -> appendKeywordString sb keyword
        | Ast.Vector asts ->
            sb.Append('[') |> ignore
            appendAstList sb printReadably asts
            sb.Append(']') |> ignore

        | Ast.HashMap hashMap -> appendHashMap sb printReadably hashMap
        | Ast.List asts ->
            sb.Append('(') |> ignore
            appendAstList sb printReadably asts
            sb.Append(')') |> ignore

        | Ast.CoreFunction _
        | Ast.UserDefinedFunction _ -> sb.Append("#<function>") |> ignore

        | Ast.Atom atom ->
            sb.Append("(atom ") |> ignore
            appendAst sb printReadably atom.Value
            sb.Append(')') |> ignore


    let printAst printReadably (ast: Ast) =
        let sb = StringBuilder()
        appendAst sb printReadably ast
        sb.ToString()
