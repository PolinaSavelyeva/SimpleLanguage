module SimpleLanguage.Tests.Generators

open FsCheck
open SimpleLanguage.AST

type ASTWrapper = AST

let variableNameGen =
    Gen.elements [ "x"; "y"; "z"; "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"; "k"; "l"; "m"; "n"; "o"; "p"; "q"; "r"; "s"; "t"; "u"; "v"; "w" ]

let comparativeSignGen = Gen.elements [ "<"; "<="; ">"; ">="; "=="; "!=" ]

let variableGen = variableNameGen |> Gen.map Variable

let rec expressionGen depth =
    Gen.sized (fun size ->
        if size <= 0 || depth <= 0 then
            Gen.oneof [ Gen.choose (-100, 100) |> Gen.map Number; Gen.choose (0, 1) |> Gen.map (fun n -> Boolean(n = 0)); variableGen ]
        else
            Gen.oneof
                [ Gen.choose (-100, 100) |> Gen.map Number
                  Gen.choose (0, 1) |> Gen.map (fun n -> Boolean(n = 0))
                  variableGen
                  Gen.listOf (expressionGen (depth - 1)) |> Gen.map Mult
                  Gen.listOf (expressionGen (depth - 1)) |> Gen.map Add
                  Gen.listOf (expressionGen (depth - 1)) |> Gen.map And
                  Gen.listOf (expressionGen (depth - 1)) |> Gen.map Or
                  Gen.map2 (fun comparativeSign lst -> Compare(comparativeSign, lst)) comparativeSignGen (Gen.listOf (expressionGen (depth - 1))) ])

let rec statementGen depth =
    Gen.sized (fun size ->
        if size <= 0 || depth <= 0 then
            Gen.oneof
                [ Gen.map2
                      (fun variableName expression -> Assignment(variableName, expression))
                      (Gen.map
                          (fun variable ->
                              match variable with
                              | Variable name -> name
                              | _ -> failwith "Variable was expected here.")
                          variableGen)
                      (expressionGen depth)
                  expressionGen depth |> Gen.map Print ]
        else
            Gen.oneof
                [ Gen.map2
                      (fun variableName expression -> Assignment(variableName, expression))
                      (Gen.map
                          (fun variable ->
                              match variable with
                              | Variable name -> name
                              | _ -> failwith "Variable was expected here.")
                          variableGen)
                      (expressionGen depth)
                  expressionGen depth |> Gen.map Print
                  Gen.map2
                      (fun expression branches -> Condition(expression, branches))
                      (expressionGen (depth - 1))
                      (Gen.map2 (fun trueBranch elseBranch -> (trueBranch, elseBranch)) (Gen.listOf (statementGen (depth - 1))) (Gen.listOf (statementGen (depth - 1)))) ])

let astGen =
    gen {
        let! depth = Gen.choose (1, 5)
        return! Gen.listOf (statementGen depth)
    }

let rec generateCodeFromAST (ast: AST) =

    let rec nonEmptyFold list sign =
        match list with
        | [ hd ] -> generateCodeFromExpression hd
        | hd :: tl -> sprintf "%s%s%s" (generateCodeFromExpression hd) sign (nonEmptyFold tl sign)
        | [] -> failwith "Unexpected empty list."

    and generateCodeFromExpression (expression: Expression) =
        match expression with
        | Number number -> sprintf "%d" number
        | Boolean bool -> sprintf "%b" bool
        | Variable variable -> variable
        | Mult listOfExpressions -> nonEmptyFold listOfExpressions "*"
        | Add listOfExpressions -> nonEmptyFold listOfExpressions "+"
        | And listOfExpressions -> nonEmptyFold listOfExpressions "&&"
        | Or listOfExpressions -> nonEmptyFold listOfExpressions "||"
        | Compare(comparativeSign, listOfExpressions) -> nonEmptyFold listOfExpressions comparativeSign

    List.fold
        (fun codeString statement ->
            match statement with
            | Assignment(variableName, expression) -> codeString + sprintf "%s=%s\n" variableName (generateCodeFromExpression expression)
            | Print expression -> codeString + sprintf "print:%s\n" (generateCodeFromExpression expression)
            | Condition(expression, (trueBranch: Statement list, elseBranch: Statement list)) ->
                codeString
                + sprintf "if:%sthen:\n" (generateCodeFromExpression expression)
                + sprintf "%s" (generateCodeFromAST trueBranch)
                + "else:\n"
                + sprintf "%send\n" (generateCodeFromAST elseBranch))
        ""
        ast

type Generators =
    static member ASTWrapper() = Arb.fromGen astGen
