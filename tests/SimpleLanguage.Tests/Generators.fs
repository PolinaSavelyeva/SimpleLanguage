module SimpleLanguage.Tests.Generators

open FsCheck
open SimpleLanguage.AST

type ASTWrapper = AST

let variableNameGen =
    Gen.elements [ "x"; "y"; "z"; "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"; "k"; "l"; "m"; "n"; "o"; "p"; "q"; "r"; "s"; "t"; "u"; "v"; "w" ]

let comparativeSignGen = Gen.elements [ "<"; "<="; ">"; ">="; "=="; "!=" ]

let variableGen = variableNameGen |> Gen.map Variable

let rec numberGen = Gen.choose (0, 100) |> Gen.map Number

let rec booleanGen = Gen.choose (0, 1) |> Gen.map (fun n -> Boolean(n = 0))

let rec multGen =
    Gen.nonEmptyListOf (Gen.oneof [ variableGen; numberGen ]) |> Gen.map Mult

let rec addGen = Gen.nonEmptyListOf multGen |> Gen.map Add

let rec compareGen =
    Gen.map (fun expression -> Compare("", expression)) (Gen.listOfLength 1 booleanGen)

let rec andGen = Gen.nonEmptyListOf compareGen |> Gen.map And

let rec orGen = Gen.nonEmptyListOf andGen |> Gen.map Or

let rec statementGen input =
    Gen.sized (fun size ->
        if size <= 0 then
            Gen.oneof
                [ Gen.map2
                      (fun variableName expression -> Assignment(variableName, expression))
                      (Gen.map
                          (fun variable ->
                              match variable with
                              | Variable name -> name
                              | _ -> failwith "Variable was expected here.")
                          variableGen)
                      orGen
                  orGen |> Gen.map Print ]
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
                      orGen
                  orGen |> Gen.map Print
                  Gen.map2
                      (fun expression branches -> Condition(expression, branches))
                      orGen
                      (Gen.map2 (fun trueBranch elseBranch -> (trueBranch, elseBranch)) (Gen.nonEmptyListOf (statementGen <| input)) (Gen.nonEmptyListOf (statementGen <| input))) ])

let astGen =
    gen {
        let! depth = Gen.choose (1, 5)
        return! Gen.nonEmptyListOf (statementGen depth)
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
