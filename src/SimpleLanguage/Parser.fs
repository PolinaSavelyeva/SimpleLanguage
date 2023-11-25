module SimpleLanguage.Parser

open SimpleLanguage.ParserCombinators
open SimpleLanguage.AST

let stringParser: Parser<string> =
    someParser (satisfyPredicate (fun x -> List.contains x [ 'a' .. 'z' ]))
    |> mapParser (fun res -> res |> Array.ofList |> System.String)

let numberParser =
    makeAlternativeParser
        (bindParsers (satisfyPredicate (fun x -> List.contains x [ '1' .. '9' ])) (fun res -> mapParser (fun tl -> res :: tl) (manyParser (satisfyPredicate (fun x -> List.contains x [ '0' .. '9' ])))))
        (bindParsers (satisfyPredicate (fun x -> List.contains x [ '0' .. '9' ])) (fun digit -> reverseParser [ digit ] (satisfyPredicate (fun x -> List.contains x [ '0' .. '9' ]))))
    |> mapParser (fun res -> res |> Array.ofList |> System.String |> int |> Number)

let booleanParser =
    makeAlternativeParser (makeKeywordParser "true") (makeKeywordParser "false")
    |> mapParser (fun res -> res |> System.Convert.ToBoolean |> Boolean)

let multParser =
    makeListParser (makeAlternativeParser numberParser (mapParser Variable stringParser)) (makeIgnoreParser (makeCharParser '*'))
    |> mapParser Mult

let addParser =
    makeListParser multParser (makeIgnoreParser (makeCharParser '+')) |> mapParser Add

let assignmentParser =
    bindParsers stringParser (fun variableName -> bindParsers (makeIgnoreParser (makeCharParser '=')) (fun _ -> mapParser (fun expression -> Assignment(variableName, expression)) addParser))

let printParser =
    bindParsers (makeKeywordParser "print") (fun _ -> bindParsers (makeCharParser ':') (fun _ -> mapParser Print addParser))

let rec conditionParser input =
    bindParsers (makeKeywordParser "if") (fun _ ->
        bindParsers (makeCharParser ':') (fun _ ->
            bindParsers addParser (fun condition ->
                mapParser (fun thenAndElseBranchesResult -> condition, thenAndElseBranchesResult)
                <| bindParsers (makeKeywordParser "then") (fun _ ->
                    bindParsers (makeCharParser ':') (fun _ ->
                        bindParsers (makeCharParser '\n') (fun _ ->
                            bindParsers (makeListParser (composeAlternativeParser [ conditionParser; printParser; assignmentParser ]) (makeIgnoreParser (makeCharParser '\n'))) (fun thenBranch ->
                                mapParser (fun elseBranchResult -> thenBranch, elseBranchResult)
                                <| bindParsers (makeCharParser '\n') (fun _ ->
                                    bindParsers (makeKeywordParser "else") (fun _ ->
                                        bindParsers (makeCharParser ':') (fun _ ->
                                            bindParsers (makeCharParser '\n') (fun _ ->
                                                (bindParsers
                                                    (makeListParser (composeAlternativeParser [ conditionParser; printParser; assignmentParser ]) (makeIgnoreParser (makeCharParser '\n')))
                                                    (fun beforeEndResult -> mapParser (fun _ -> beforeEndResult) (makeKeywordParser "end"))))))))))))))
    |> mapParser Condition
    <| input

let programParser =
    makeListParser (composeAlternativeParser [ conditionParser; printParser; assignmentParser ]) (makeIgnoreParser (makeCharParser '\n'))
