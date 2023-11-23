module SimpleLanguage.Parser

open SimpleLanguage.ParserCombinators
open SimpleLanguage.AST

let stringParser: Parser<string> =
    someParser (satisfyPredicate (fun x -> List.contains x [ 'a' .. 'z' ]))
    |> mapParser (fun res -> res |> Array.ofList |> System.String)

let numberParser =
    bindParsers (satisfyPredicate (fun x -> List.contains x [ '1' .. '9' ])) (fun res -> mapParser (fun tl -> res :: tl) (manyParser (satisfyPredicate (fun x -> List.contains x [ '0' .. '9' ]))))
    |> mapParser (fun res -> res |> Array.ofList |> System.String |> int |> Number)

let multParser =
    makeListParser (makeAlternativeParser numberParser (mapParser Variable stringParser)) (makeIgnoreParser (makeCharParser '*'))
    |> mapParser Mult

let addParser =
    makeListParser multParser (makeIgnoreParser (makeCharParser '+')) |> mapParser Add

let assignmentParser =
    bindParsers stringParser (fun variableName -> bindParsers (makeIgnoreParser (makeCharParser '=')) (fun _ -> mapParser (fun expression -> Assignment(variableName, expression)) addParser))

let printParser = makeKeywordParser "print"

let statementParser =
    bindParsers printParser (fun _ -> bindParsers (makeCharParser ':') (fun _ -> mapParser Print addParser))

let programParser =
    makeListParser (makeAlternativeParser statementParser assignmentParser) (makeIgnoreParser (makeCharParser '\n'))
