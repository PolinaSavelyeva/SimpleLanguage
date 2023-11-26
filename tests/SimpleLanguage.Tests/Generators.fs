module SimpleLanguage.Tests.Generators

open FsCheck
open System
open SimpleLanguage.AST

let stringGen =
    Gen.elements [ "x"; "y"; "z"; "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"; "k"; "l"; "m"; "n"; "o"; "p"; "q"; "r"; "s"; "t"; "u"; "v"; "w" ]
let variableNameGen =
    stringGen
    |> Gen.map Variable

let rec expressionGen depth =
    Gen.sized (fun size ->
        if size <= 0 || depth <= 0 then
            Gen.oneof [
                Gen.choose (Int32.MinValue, Int32.MaxValue) |> Gen.map Number
                Gen.choose (0, 1) |> Gen.map (fun n -> Boolean (n = 0))
                variableNameGen
            ]
        else
            Gen.oneof [
                Gen.choose (Int32.MinValue, Int32.MaxValue) |> Gen.map Number
                Gen.choose (0, 1) |> Gen.map (fun n -> Boolean (n = 0))
                variableNameGen
                Gen.listOf (expressionGen (depth - 1)) |> Gen.map  Mult
                Gen.listOf (expressionGen (depth - 1)) |> Gen.map Add
                Gen.listOf (expressionGen (depth - 1)) |> Gen.map And
                Gen.listOf (expressionGen (depth - 1)) |> Gen.map Or
                Gen.map2 (fun s lst -> Compare (s, lst))
                    stringGen
                    (Gen.listOf (expressionGen (depth - 1)))
            ]
    )

let rec statementGen depth =
    Gen.sized (fun size ->
        if size <= 0 || depth <= 0 then
            Gen.oneof [
                variableNameGen |> Gen.map (fun s ->
                    match s with
                    | Variable s -> Assignment (s, Number 0))
                expressionGen depth |> Gen.map Print
            ]
        else
            Gen.oneof [
                variableNameGen |> Gen.map (fun s ->
                    match s with
                    | Variable s -> Assignment (s, Number 0))
                expressionGen depth |> Gen.map Print
                Gen.map2 (fun e branches -> Condition (e, branches))
                    (expressionGen (depth - 1))
                    (Gen.map2 (fun l r -> (l, r))
                        (Gen.listOf (statementGen (depth - 1)))
                        (Gen.listOf (statementGen (depth - 1)))
                    )
            ]
    )

let astGen =
    gen{
        let! depth = Gen.choose (1, 5)
        return! Gen.listOf (statementGen depth)
        }

let rec generateCodeFromAST (ast: AST) =
    let rec generateCodeFromExpression (e: Expression) =
        match e with
        | Number n -> sprintf "%d" n
        | Boolean b -> sprintf "%b" b
        | Variable v -> v
        | Mult lst -> List.fold (fun acc e -> acc + sprintf "%s * " (generateCodeFromExpression e)) "" lst
        | Add lst -> List.fold (fun acc e -> acc + sprintf "%s + " (generateCodeFromExpression e)) "" lst
        | And lst -> List.fold (fun acc e -> acc + sprintf "%s && " (generateCodeFromExpression e)) "" lst
        | Or lst -> List.fold (fun acc e -> acc + sprintf "%s || " (generateCodeFromExpression e)) "" lst
        | Compare (s, lst) -> List.fold (fun acc e -> acc + sprintf "%s %s %s" (generateCodeFromExpression e) s (generateCodeFromExpression e)) "" lst

    List.fold (fun acc s ->
        match s with
        | Assignment (v, e) -> acc + sprintf "%s = %s\n" v (generateCodeFromExpression e)
        | Print e -> acc + sprintf "print(%s)\n" (generateCodeFromExpression e)
        | Condition (e, branches) ->
            acc + sprintf "if%s:" (generateCodeFromExpression e)
            + List.fold (fun acc (l: Statement list, r: Statement list) ->
                acc + sprintf "    %s\n" (generateCodeFromAST l)
                + sprintf "} else {\n"
                + sprintf "    %s\n" (generateCodeFromAST r)
            ) "" branches
    ) "" ast

type Generators =
    static member AST() = Arb.fromGen astGen
