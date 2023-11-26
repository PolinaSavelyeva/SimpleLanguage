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

let variableParser = (mapParser Variable stringParser)

let multParser =
    makeListParser (makeAlternativeParser numberParser (mapParser Variable stringParser)) (makeIgnoreParser (makeCharParser '*'))
    |> mapParser Mult

let addParser =
    makeListParser multParser (makeIgnoreParser (makeCharParser '+')) |> mapParser Add

let lessParser = makeKeywordParser "<"
let lessThanOrEqual = makeKeywordParser "<="
let greaterParser = makeKeywordParser ">"
let greaterThanOrEqual = makeKeywordParser ">="
let equalParser = makeKeywordParser "=="
let notEqualParser = makeKeywordParser "!="

let booleanParser =
    composeAlternativeParser
        [ (makeAlternativeParser (makeKeywordParser "true") (makeKeywordParser "false")
           |> mapParser (fun res -> Boolean(res = "true")))
          variableParser ]

let compareParser =
    let mutable comparisonSymbol = ""

    makeListParser
        (composeAlternativeParser [ booleanParser; addParser ])
        (mapParser (fun result -> comparisonSymbol <- result) (composeAlternativeParser [ lessParser; lessThanOrEqual; greaterParser; greaterThanOrEqual; equalParser; notEqualParser ]))
    |> mapParser (fun result ->
        let tmp = comparisonSymbol
        comparisonSymbol <- ""
        Compare(tmp, result))

let andParser =
    makeListParser compareParser (makeIgnoreParser (makeKeywordParser "&&")) |> mapParser And

let orParser =
    makeListParser andParser (makeIgnoreParser (makeKeywordParser "||")) |> mapParser Or


let assignmentParser =
    bindParsers stringParser (fun variableName -> bindParsers (makeIgnoreParser (makeCharParser '=')) (fun _ -> mapParser (fun expression -> Assignment(variableName, expression)) orParser))

let printParser =
    bindParsers (makeKeywordParser "print") (fun _ -> bindParsers (makeCharParser ':') (fun _ -> mapParser Print orParser))

let rec conditionParser input =
    bindParsers (makeKeywordParser "if") (fun _ ->
        bindParsers (makeCharParser ':') (fun _ ->
            bindParsers orParser (fun condition ->
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
                                                    (fun beforeEndResult -> bindParsers (makeCharParser '\n') (fun _ -> mapParser (fun _ -> beforeEndResult) (makeKeywordParser "end")))))))))))))))
    |> mapParser Condition
    <| input

let programParser =
    makeListParser (composeAlternativeParser [ conditionParser; printParser; assignmentParser ]) (makeIgnoreParser (makeCharParser '\n'))
