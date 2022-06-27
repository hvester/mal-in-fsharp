namespace Interpreter

[<AutoOpen>]
module rec AstTypes =

    let private formatString (str: string) =
        let escaped =
            str.ToCharArray()
            |> Array.map (function
                | '\\' -> @"\\"
                | '"' -> "\\\""
                | c -> string c)
            |> String.concat ""

        $"\"{escaped}\""

    type KeywordString =
        | KeywordString of string

        override this.ToString() =
            let (KeywordString s) = this
            $":{s}"

    type HashMapKey =
        | StringKey of string
        | KeywordKey of KeywordString

        override this.ToString() =
            match this with
            | StringKey key -> formatString key
            | KeywordKey key -> string key

    type HashMap =
        | HashMap of Map<HashMapKey, Ast>

        override this.ToString() =
            let (HashMap keyValuePairs) = this

            keyValuePairs
            |> Seq.map (fun (KeyValue (k, v)) -> $"{string k} {string v}")
            |> String.concat " "
            |> sprintf "{%s}"

    [<RequireQualifiedAccess>]
    type Ast =
        | Nil
        | Boolean of bool
        | Number of float
        | Symbol of string
        | String of string
        | Keyword of KeywordString
        | Vector of Ast list
        | HashMap of HashMap
        | List of Ast list
        | Quote of Ast
        | Quasiquote of Ast
        | Unquote of Ast
        | SpliceUnquote of Ast
        | Deref of Ast

        override this.ToString() =
            let formatAstList (asts: Ast list) =
                asts |> List.map string |> String.concat " "

            match this with
            | Nil -> "nil"
            | Boolean b -> if b then "true" else "false"
            | Number x -> string x
            | Symbol s -> s
            | String s -> formatString s
            | Keyword keyword -> string keyword
            | Vector asts -> $"[{formatAstList asts}]"
            | HashMap hashMap -> string hashMap
            | List asts -> $"({formatAstList asts})"
            | Quote ast -> $"(quote {string ast})"
            | Quasiquote ast -> $"(quasiquote {string ast})"
            | Unquote ast -> $"(unquote {string ast})"
            | SpliceUnquote ast -> $"(splice-unquote {string ast})"
            | Deref ast -> $"(deref {string ast})"
