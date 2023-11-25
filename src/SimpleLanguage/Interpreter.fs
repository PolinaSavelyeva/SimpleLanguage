module SimpleLanguage.Interpreter

open System.Collections.Generic
open SimpleLanguage.AST

let rec evaluateExpression (context: Dictionary<_, _>) expression =
    let intMapping =
        (fun expression ->
            let expressionValue, expressionType = evaluateExpression context expression

            if expressionType = "int" then
                expressionValue
            else
                failwith $"Expected int type as argument in %A{expression} expression.")

    let boolMapping =
        (fun expression ->
            let expressionValue, expressionType = evaluateExpression context expression

            if expressionType = "bool" then
                expressionValue = 1
            else
                failwith $"Expected bool type as argument in %A{expression} expression.")

    match expression with
    | Number number -> number, "int"
    | Boolean boolean -> System.Convert.ToInt32 boolean, "bool"
    | Mult listOfExpressions -> listOfExpressions |> List.map intMapping |> List.reduce (*), "int"
    | Add listOfExpressions -> listOfExpressions |> List.map intMapping |> List.reduce (+), "int"
    | Variable variableName ->
        if context.ContainsKey variableName then
            context[variableName]
        else
            failwithf $"Variable with name {variableName} not declared."
    | And listOfExpressions -> listOfExpressions |> List.map boolMapping |> List.reduce (&&) |> System.Convert.ToInt32, "bool"
    | Or listOfExpressions -> listOfExpressions |> List.map boolMapping |> List.reduce (||) |> System.Convert.ToInt32, "bool"

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
                    printfn $"{expressionValue = 1}"
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
