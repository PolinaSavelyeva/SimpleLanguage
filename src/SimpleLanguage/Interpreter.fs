module SimpleLanguage.Interpreter

open System.Collections.Generic
open SimpleLanguage.AST

let rec evaluateExpression (context: Dictionary<_, _>) expression =
    match expression with
    | Number number -> number, "int"
    | Boolean boolean -> System.Convert.ToInt32 boolean, "bool"
    | Mult listOfExpressions ->
        (listOfExpressions
         |> List.map (fun expression ->
             let expressionValue, expressionType = evaluateExpression context expression

             if expressionType = "int" then
                 expressionValue
             else
                 failwith "Expected int type in multiplication.")
         |> List.reduce (*)),
        "int"
    | Add listOfExpressions ->
        listOfExpressions
        |> List.map (fun expression ->
            let expressionValue, expressionType = evaluateExpression context expression

            if expressionType = "int" then
                expressionValue
            else
                failwith "Expected int type in addition.")
        |> List.reduce (+),
        "int"
    | Variable variableName ->
        if context.ContainsKey variableName then
            context[variableName]
        else
            failwithf $"Variable with name {variableName} not declared."

let evaluateAST (statements: AST) =
    let context = Dictionary<string, int * string>()

    let rec inner (currentAST: AST) =

        let rec evaluateStatement context statement =
            match statement with
            | Assignment(variableName, expression) -> Some(variableName, evaluateExpression context expression)
            | Print expression ->
                let expressionValue, expressionType = evaluateExpression context expression

                if expressionType = "int" then
                    printfn $"{expressionValue}"
                    None
                else
                    printfn $"{System.Convert.ToBoolean expressionValue}"
                    None
            | Condition(condition, (thenBranch, elseBranch)) ->
                let conditionValue, conditionType = evaluateExpression context condition

                if conditionType = "bool" then
                    if conditionValue = 1 then
                        inner thenBranch
                    else
                        inner elseBranch
                else
                    failwithf $"Condition must be of type bool."

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
