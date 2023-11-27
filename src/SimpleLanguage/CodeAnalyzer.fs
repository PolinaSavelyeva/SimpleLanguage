module SimpleLanguage.CodeAnalyzer

open SimpleLanguage.AST
open SimpleLanguage.Interpreter

let rec optimiseAST ast =
    let rec optimiseExpression (expression: Expression) =
        match expression with
        | Add expressionsList ->
            try
                Add[List.reduce (fun acc expression ->
                        match acc, expression with
                        | Mult [ Number a ], Mult [ Number b ] -> Mult[Number(a + b)]
                        | _ -> failwith "Variables are not expected.")
                    <| List.map optimiseExpression expressionsList]
            with _ ->
                expression
        | Mult expressionsList ->
            try
                Mult
                    [ List.reduce
                          (fun acc expression ->
                              match acc, expression with
                              | Number a, Number b -> Number(a * b)
                              | _ -> failwith "Variables are not expected.")
                          expressionsList ]
            with _ ->
                expression
        | And expressionsList ->
            try
                And
                    [ List.reduce (fun acc expression ->
                          match acc, expression with
                          | Compare("", [ Boolean a ]), Compare("", [ Boolean b ]) -> Compare("", [ Boolean(a && b) ])
                          | _ -> failwith "Variables are not expected.")
                      <| List.map optimiseExpression expressionsList ]
            with _ ->
                expression
        | Or expressionsList ->
            try
                Or
                    [ List.reduce (fun acc expression ->
                          match acc, expression with
                          | And [ Compare("", [ Boolean a ]) ], And [ Compare("", [ Boolean b ]) ] -> Compare("", [ Boolean(a || b) ])
                          | _ -> failwith "Variables are not expected.")
                      <| List.map optimiseExpression expressionsList ]
            with _ ->
                expression
        | Compare(comparativeSign, [ Boolean bool1; Boolean bool2 ]) -> Compare("", [ Boolean(comparativeSymbolParse comparativeSign bool1 bool2) ])
        | Compare(comparativeSign, [ addExpressionsList1; addExpressionsList2 ]) ->
            try
                Compare("", [ Boolean(comparativeSymbolParse comparativeSign (optimiseExpression addExpressionsList1) (optimiseExpression addExpressionsList2)) ])
            with _ ->
                expression
        | _ -> expression

    List.fold
        (fun acc statement ->
            match statement with
            | Condition(condition, (trueBranch, elseBranch)) ->
                let optimiseExpression = optimiseExpression condition

                match optimiseExpression with
                | Or [ And [ Compare("", [ Boolean bool ]) ] ] -> acc @ (if bool then trueBranch else elseBranch |> optimiseAST)
                | _ -> acc @ [ Condition(optimiseExpression, (trueBranch, elseBranch)) ]
            | _ -> acc @ [ statement ])
        []
        ast
