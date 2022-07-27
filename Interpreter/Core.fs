namespace Interpreter

open System.IO

module Core =

    let private getOneArgument (argumentAsts: Ast list) =
        match argumentAsts with
        | [] -> raise (ArgumentError "Expected one argument.")
        | argumentAst :: _ -> argumentAst

    let private getTwoArguments (argumentAsts: Ast list) =
        match argumentAsts with
        | []
        | _ :: [] -> raise (ArgumentError "Expected two arguments.")
        | firstArgumentAst :: secondArgumentAst :: _ -> (firstArgumentAst, secondArgumentAst)

    let private integerOperation f asts =
        match asts with
        | []
        | _ :: [] -> raise (ArgumentError "Expected at least two arguments.")
        | firstAst :: otherAsts ->
            (Ast.unwrapInteger firstAst, otherAsts)
            ||> List.fold (fun acc ast -> f acc (Ast.unwrapInteger ast))
            |> Ast.Integer

    let add = integerOperation (fun x y -> x + y)
    let substract = integerOperation (fun x y -> x - y)
    let multiply = integerOperation (fun x y -> x * y)
    let divide = integerOperation (fun x y -> x / y)

    let list asts = Ast.List asts

    let isList asts =
        match asts with
        | Ast.List _ :: _ -> Ast.Boolean true
        | _ -> Ast.Boolean false

    let isEmpty asts =
        getOneArgument asts
        |> Ast.unwrapCollection
        |> List.isEmpty
        |> Ast.Boolean

    let count asts =
        match asts with
        | Ast.List xs :: _
        | Ast.Vector xs :: _ -> List.length xs |> Ast.Integer
        | _ -> Ast.Integer 0

    let areEqual asts =
        let rec twoAreEqual ast1 ast2 =
            match ast1, ast2 with
            | Ast.Nil, Ast.Nil -> true
            | Ast.Boolean x, Ast.Boolean y -> x = y
            | Ast.Integer x, Ast.Integer y -> x = y
            | Ast.Symbol x, Ast.Symbol y -> x = y
            | Ast.String x, Ast.String y -> x = y
            | Ast.Keyword x, Ast.Keyword y -> x = y
            | (Ast.Vector xs
              | Ast.List xs),
              (Ast.Vector ys
              | Ast.List ys) ->
                List.length xs = List.length ys
                && List.forall2 twoAreEqual xs ys

            | Ast.HashMap (HashMap x), Ast.HashMap (HashMap y) ->
                Map.count x = Map.count y
                && (x
                    |> Seq.forall (fun (KeyValue (k, v1)) ->
                        match Map.tryFind k y with
                        | None -> false
                        | Some v2 -> twoAreEqual v1 v2))

            | _ -> false

        let ast1, ast2 = getTwoArguments asts
        twoAreEqual ast1 ast2 |> Ast.Boolean


    let private integerComparison compare asts =
        let ast1, ast2 = getTwoArguments asts

        compare (Ast.unwrapInteger ast1) (Ast.unwrapInteger ast2)
        |> Ast.Boolean

    let lessThan = integerComparison (<)
    let lessThanOrEqual = integerComparison (<=)
    let greaterThan = integerComparison (>)
    let greaterThanOrEqual = integerComparison (>=)

    let private printAndConcat printReadably separator (asts: Ast list) =
        asts
        |> List.map (Printer.printAst printReadably)
        |> String.concat separator

    let prStr (asts: Ast list) =
        printAndConcat true " " asts |> Ast.String

    let str (asts: Ast list) =
        printAndConcat false "" asts |> Ast.String

    let prn writeLine (asts: Ast list) =
        printAndConcat true " " asts |> writeLine
        Ast.Nil

    let println writeLine (asts: Ast list) =
        printAndConcat false " " asts |> writeLine
        Ast.Nil

    let readString (asts: Ast list) =
        getOneArgument asts
        |> Ast.unwrapString
        |> Parser.read
        |> List.tryHead
        |> Option.defaultValue Ast.Nil

    let slurp (asts: Ast list) =
        let filePath = getOneArgument asts |> Ast.unwrapString

        try
            File.ReadAllText(filePath) |> Ast.String
        with
        | _ -> Ast.Nil

    let atom (asts: Ast list) =
        Ast.Atom { Value = getOneArgument asts }

    let isAtom (asts: Ast list) =
        match getOneArgument asts with
        | Ast.Atom _ -> Ast.Boolean true
        | _ -> Ast.Boolean false

    let deref (asts: Ast list) =
        (Ast.unwrapAtom (getOneArgument asts)).Value

    let reset (asts: Ast list) =
        let atomAst, value = getTwoArguments asts
        let atom = Ast.unwrapAtom atomAst
        atom.Value <- value
        value

    let swap (asts: Ast list) =
        let atom = getOneArgument asts |> Ast.unwrapAtom

        let result =
            match List.tail asts with
            | Ast.CoreFunction func :: otherArgs -> func (atom.Value :: otherArgs)

            | Ast.UserDefinedFunction (env, argNames, body) :: otherArgs ->
                let functionEnv = Evaluator.createInnerEnv env argNames (atom.Value :: otherArgs)
                Evaluator.evalAst functionEnv body

            | _ -> raise (ArgumentError "Expected that second argument is a function.")

        atom.Value <- result
        result

    let cons (asts: Ast list) =
        let firstAst, secondAst = getTwoArguments asts

        firstAst :: Ast.unwrapCollection secondAst
        |> Ast.List

    let concat (asts: Ast list) =
        List.collect Ast.unwrapCollection asts
        |> Ast.List

    let vec (asts: Ast list) =
        Ast.unwrapCollection (getOneArgument asts)
        |> Ast.Vector

    let nth (asts: Ast list) =
        let firstAst, secondAst = getTwoArguments asts
        let items = Ast.unwrapCollection firstAst
        let index = Ast.unwrapInteger secondAst
        if index >= 0 && index < items.Length then
            List.item index items
        else
            raise (ArgumentError "Index out of bounds")

    let first (asts: Ast list) =
        match getOneArgument asts with
        | Ast.Nil -> Ast.Nil
        | ast ->
            match Ast.unwrapCollection ast with
            | [] -> Ast.Nil
            | items -> List.head items

    let rest (asts: Ast list) =
        match getOneArgument asts with
        | Ast.Nil -> Ast.List []
        | ast ->
            match Ast.unwrapCollection ast with
            | [] -> Ast.List []
            | items -> Ast.List (List.tail items)

    let createRootEnv writeLine =
        let env = Env(None)

        [ "+", add
          "-", substract
          "*", multiply
          "/", divide
          "list", list
          "list?", isList
          "empty?", isEmpty
          "count", count
          "=", areEqual
          "<", lessThan
          "<=", lessThanOrEqual
          ">", greaterThan
          ">=", greaterThanOrEqual
          "pr-str", prStr
          "str", str
          "prn", prn writeLine
          "println", println writeLine
          "read-string", readString
          "slurp", slurp
          "eval", getOneArgument >> Evaluator.evalAst env
          "atom", atom
          "atom?", isAtom
          "deref", deref
          "reset!", reset
          "swap!", swap
          "cons", cons
          "concat", concat
          "vec", vec
          "nth", nth
          "first", first
          "rest", rest ]
        |> List.iter (fun (symbolName, func) -> env.Set(symbolName, Ast.CoreFunction func))

        env
