module SimpleLanguage.Tests.Helper

open Expecto
open SimpleLanguage.Main
open SimpleLanguage.Interpreter
open SimpleLanguage.AST
open System.Collections.Generic

let config =
    { FsCheckConfig.defaultConfig with
        arbitrary = [ typeof<Generators.Generators> ]
        maxTest = 10 }

let code =
    "if:true>falsethen:
x=true<true
print:x<false
else:
x=true
if:x==truethen:
x=true
else:
x=true
end
print:true
x=true
end
x=3+4+5
print:true&&false<true||false
"

let codeAST = generateProgramAST code

let acc = []

let listAST (statements: AST) =
    let context = Dictionary<string, CustomBox>()

    let rec inner (currentAST: AST) =

        let rec evaluateStatement context statement =
            match statement with
            | Assignment(variableName, expression) -> Some(variableName, evaluateExpression context expression)
            | Print expression ->
                let boxedExpression = evaluateExpression context expression

                if boxedExpression.IsInt then
                    boxedExpression.Value :: acc |> ignore
                    None
                else
                    boxedExpression.Value :: acc |> ignore
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
