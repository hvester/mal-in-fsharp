namespace Interpreter

open System.Collections.Generic

type Env private (outer: Env option) =

    let bindings = Dictionary<string, Ast>()

    member _.Set(symbol, ast) =
        let symbolName = Ast.getSymbolName symbol
        bindings[symbolName] <- ast

    member _.Resolve(symbolName) =
        match bindings.TryGetValue(symbolName) with
        | true, value -> value
        | false, _ ->
            match outer with
            | Some o -> o.Resolve(symbolName)
            | None -> raise (EvaluationError $"{symbolName} not found")

    member this.CreateInner(symbols, values) =
        let innerEnv = Env(Some this)

        let rec loop =
            function
            | [], _ -> ()
            | Ast.Symbol "&" :: [], _ -> raise (EvaluationError $"Encountered & without following symbol.")

            | Ast.Symbol "&" :: restSymbol :: _, restValues -> innerEnv.Set(restSymbol, Ast.List restValues)

            | symbol :: remainingSymbols, value :: remainingValues ->
                innerEnv.Set(symbol, value)
                loop (remainingSymbols, remainingValues)

            | symbol :: remainingSymbols, [] ->
                innerEnv.Set(symbol, Ast.Nil)
                loop (remainingSymbols, [])

        loop (symbols, values)
        innerEnv

    static member CreateInitial() =
        let env = Env(None)

        [ "+", Core.add
          "-", Core.substract
          "*", Core.multiply
          "/", Core.divide
          "list", Core.list
          "list?", Core.isList
          "empty?", Core.isEmpty
          "count", Core.count
          "=", Core.areEqual
          "<", Core.lessThan
          "<=", Core.lessThanOrEqual
          ">", Core.greaterThan
          ">=", Core.greaterThanOrEqual
          "prn", Core.prn ]
        |> List.iter (fun (symbolName, func) -> env.Set(Ast.Symbol symbolName, Ast.Function func))

        env