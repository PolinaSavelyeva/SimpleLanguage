module SimpleLanguage.Interpreter

open System.Collections.Generic
open SimpleLanguage.AST

let rec evaluateExpression (context: Dictionary<_, _>) expression =
    match expression with
    | Number number -> number
    | Mult list ->
        list
        |> List.map (evaluateExpression context)
        |> List.reduce (*)
    | Add list ->
        list
        |> List.map (evaluateExpression context)
        |> List.reduce (+)
    | Var variableName ->
        if context.ContainsKey variableName then
            context[variableName]
        else
            failwithf $"Var with name {variableName} not declared."

let rec evaluateStatement context statement =
    match statement with
    | Asgn(variableName, expression) -> Some(variableName, evaluateExpression context expression)
    | Print expression ->
        printfn $"{evaluateExpression context expression}"
        None

let evaluateProgram (statements: list<sourceAst>) =
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

            context
        )
        context
        statements
    |> ignore
