module SimpleLanguage.ExprParser

open SimpleLanguage.Combinators
open SimpleLanguage.AST

let makeStringParser: Parser<string> =
    makeSomeParser (satisfyPredicate (fun x -> List.contains x [ 'a' .. 'z' ]))
    |> parserMap (fun res ->
        res
        |> Array.ofList
        |> System.String
    )

let makeNumberParser =
    makeParserOfParsers
        (satisfyPredicate (fun x -> List.contains x [ '1' .. '9' ]))
        (fun res ->
            parserMap
                (fun tl -> res :: tl)
                (makeManyParser (satisfyPredicate (fun x -> List.contains x [ '0' .. '9' ])))
        )
    |> parserMap (fun res ->
        res
        |> Array.ofList
        |> System.String
        |> int
        |> Number
    )

let makeMultParser =
    makeListParser
        (makeAlternativeParser makeNumberParser (parserMap Var makeStringParser))
        (makeIgnoreParser (makeCharParser '*'))
    |> parserMap Mult

let makeAddParser =
    makeListParser makeMultParser (makeIgnoreParser (makeCharParser '+'))
    |> parserMap Add

let makeAsgnParser =
    makeParserOfParsers
        makeStringParser
        (fun ident_name ->
            makeParserOfParsers
                (makeIgnoreParser (makeCharParser '='))
                (fun _ -> parserMap (fun expr -> Asgn(ident_name, expr)) makeAddParser)
        )


let makePrintParser = makeKeywordParser "print"

let makeSourceAstParser =
    makeParserOfParsers
        makePrintParser
        (fun _ -> makeParserOfParsers (makeCharParser ':') (fun _ -> parserMap Print makeAddParser))

let makeProgramParser =
    makeListParser
        (makeAlternativeParser makeSourceAstParser makeAsgnParser)
        (makeIgnoreParser (makeCharParser '\n'))
