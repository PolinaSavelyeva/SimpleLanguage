module SimpleLanguage.Interpreter

open System.Collections.Generic
open SimpleLanguage.AST

let rec evaluateExpression (context: Dictionary<_, _>) expression =
    match expression with
    | Number number -> number
    | Mult listOfExpressions -> listOfExpressions |> List.map (evaluateExpression context) |> List.reduce (*)
    | Add listOfExpressions -> listOfExpressions |> List.map (evaluateExpression context) |> List.reduce (+)
    | Variable variableName ->
        if context.ContainsKey variableName then
            context[variableName]
        else
            failwithf $"Variable with name {variableName} not declared."

let evaluateAST (statements: AST) =
    let context = Dictionary<string, int>()

    let rec inner (currentAST: AST) =

        let rec evaluateStatement context statement =
            match statement with
            | Assignment(variableName, expression) -> Some(variableName, evaluateExpression context expression)
            | Print expression ->
                printfn $"{evaluateExpression context expression}"
                None
            | Condition(condition, (thenBranch, elseBranch)) ->
                if evaluateExpression context condition = 1 then
                    inner thenBranch
                else
                    inner elseBranch

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
