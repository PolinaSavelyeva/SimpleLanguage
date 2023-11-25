module SimpleLanguage.Interpreter

open System.Collections.Generic
open SimpleLanguage.AST

type CustomBox =
    val Value: int
    val Type: string

    new(boxValue) = { Value = boxValue; Type = "int" }

    new(boxBoolValue: bool) =
        { Value = System.Convert.ToInt32 boxBoolValue
          Type = "bool" }

    member this.Unbox = System.Convert.ToInt32 this.Value
    member this.IsInt = this.Type = "int"
    member this.IsBool = this.Type = "bool"

let comparativeSymbolParse (symbol: string) =
    match symbol with
    | ">" -> (>)
    | "<" -> (<)
    | ">=" -> (>=)
    | "<=" -> (<=)
    | "==" -> (=)
    | "!=" -> (<>)
    | _ -> failwithf $"Unknown comparative symbol {symbol}."

let rec evaluateExpression (context: Dictionary<_, _>) expression =
    let intMapping =
        (fun expression ->
            let boxedExpression: CustomBox = evaluateExpression context expression

            if boxedExpression.IsInt then
                boxedExpression.Value
            else
                failwith $"Expected int type as argument in %A{expression} expression.")

    let boolMapping =
        (fun expression ->
            let boxedExpression = evaluateExpression context expression

            if boxedExpression.IsBool then
                boxedExpression.Value = 1
            else
                failwith $"Expected bool type as argument in %A{expression} expression.")

    match expression with
    | Number number -> CustomBox number
    | Boolean boolean -> CustomBox boolean
    | Mult listOfExpressions -> listOfExpressions |> List.map intMapping |> List.reduce (*) |> CustomBox
    | Add listOfExpressions -> listOfExpressions |> List.map intMapping |> List.reduce (+) |> CustomBox
    | Variable variableName ->
        if context.ContainsKey variableName then
            context[variableName]
        else
            failwithf $"Variable with name {variableName} not declared."
    | And listOfExpressions -> listOfExpressions |> List.map boolMapping |> List.reduce (&&) |> CustomBox
    | Or listOfExpressions -> listOfExpressions |> List.map boolMapping |> List.reduce (||) |> CustomBox
    | Compare(comparativeSymbol, listOfExpressions) ->

        let rec inner comparativeSymbol (list: int list) =
            match list with
            | hd1 :: hd2 :: [] -> comparativeSymbol hd1 hd2
            | _ -> failwith "Non empty list expected."

        try
            List.map intMapping listOfExpressions
            |> inner (comparativeSymbolParse comparativeSymbol)
            |> CustomBox
        with _ ->
            List.map boolMapping listOfExpressions
            |> List.reduce (comparativeSymbolParse comparativeSymbol)
            |> CustomBox

let evaluateAST (statements: AST) =
    let context = Dictionary<string, CustomBox>()

    let rec inner (currentAST: AST) =

        let rec evaluateStatement context statement =
            match statement with
            | Assignment(variableName, expression) -> Some(variableName, evaluateExpression context expression)
            | Print expression ->
                let boxedExpression = evaluateExpression context expression

                if boxedExpression.IsInt then
                    printfn $"{boxedExpression.Value}"
                    None
                else
                    printfn $"{boxedExpression.Value = 1}"
                    None
            | Condition(condition, (thenBranch, elseBranch)) ->
                let boxedCondition = evaluateExpression context condition

                if boxedCondition.IsBool then
                    if boxedCondition.Value = 1 then
                        inner thenBranch
                    else
                        inner elseBranch
                else
                    failwithf "Condition must be of type bool."

                None

        List.fold
            (fun (context: Dictionary<_, _>) statement ->
                let result = evaluateStatement context statement

                match result with
                | Some(variableName, result) ->
                    if context.ContainsKey variableName then
                        context[variableName] <- result
                    else
                        context.Add(variableName, result)
                | None -> ()

                context)
            context
            currentAST
        |> ignore

    inner statements
