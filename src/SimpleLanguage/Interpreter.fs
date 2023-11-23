module SimpleLanguage.Interpreter

open System.Collections.Generic
open SimpleLanguage.AST

let rec evaluateExpression (context: Dictionary<_, _>) expression =
    match expression with
    | Number number -> number
    | Mult listOfExpressions -> listOfExpressions |> List.map (evaluateExpression context) |> List.reduce (*)
    | Add list -> list |> List.map (evaluateExpression context) |> List.reduce (+)
    | Variable variableName ->
        if context.ContainsKey variableName then
            context[variableName]
        else
            failwithf $"Variable with name {variableName} not declared."

let rec evaluateStatement context statement =
    match statement with
    | Assignment(variableName, expression) -> Some(variableName, evaluateExpression context expression)
    | Print expression ->
        printfn $"{evaluateExpression context expression}"
        None

let evaluateAST (statements: AST) =
    let context = Dictionary<string, int>()

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
        statements
    |> ignore

// let evaluateProgram (program: list<char>) =
