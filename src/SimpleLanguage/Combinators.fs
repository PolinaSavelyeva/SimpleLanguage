module SimpleLanguage.Combinators

type Parser<'result> = list<char> -> Option<list<char> * 'result>

let satisfyPredicate predicate : Parser<char> =
    fun charList ->
        match charList with
        | firstElementOfList :: restOfList when predicate firstElementOfList ->
            Some(restOfList, firstElementOfList)
        | _ -> None

let makeCharParser char : Parser<char> =
    satisfyPredicate (fun currentChar -> currentChar = char)

let makeEpsilonParser: Parser<unit> =
    fun charListCharList -> Some(charListCharList, ())

let makeParserOfParsers
    (parser: Parser<'result1>)
    (parserFunction: 'result1 -> Parser<'result2>)
    : Parser<'result2> =
    fun charList ->
        match parser charList with
        | None -> None
        | Some(restOfList, result) -> parserFunction result restOfList

let makeAlternativeParser (parser1: Parser<'result>) (parser2: Parser<'result>) : Parser<'result> =
    fun charList ->
        match parser1 charList with
        | None -> parser2 charList
        | parserResult -> parserResult

let parserMap (func: 'result1 -> 'result2) (parser: Parser<'result1>) : Parser<'result2> =
    fun charList ->
        match parser charList with
        | None -> None
        | Some(rest, res) -> Some(rest, func res)

let rec makeManyParser (parser: Parser<'result>) : Parser<list<'result>> =
    makeAlternativeParser
        (makeParserOfParsers
            parser
            (fun result ->
                parserMap
                    (fun tl ->
                        result
                        :: tl
                    )
                    (makeManyParser parser)
            ))
        (parserMap (fun _ -> []) makeEpsilonParser)

let makeSomeParser (parser: Parser<'result>) : Parser<list<'result>> =
    makeParserOfParsers
        parser
        (fun result ->
            parserMap
                (fun tl ->
                    result
                    :: tl
                )
                (makeManyParser parser)
        )

let makeListParser (elementParser: Parser<'elem>) (epsilonParser: Parser<unit>) =
    makeParserOfParsers
        elementParser
        (fun element ->
            parserMap
                (fun tl ->
                    element
                    :: tl
                )
                (makeManyParser (makeParserOfParsers epsilonParser (fun _ -> elementParser)))
        )

let makeIgnoreParser parser = parserMap (fun _ -> ()) parser

let makeKeywordParser (keyword: string) : Parser<string> =
    let chars = keyword.ToCharArray()

    Array.fold
        (fun parser currentChar ->
            makeParserOfParsers
                parser
                (fun res ->
                    parserMap
                        (fun char ->
                            char
                            :: res
                        )
                        (makeCharParser currentChar)
                )
        )
        (parserMap (fun _ -> []) makeEpsilonParser)
        chars
    |> parserMap (fun charList ->
        charList
        |> List.rev
        |> Array.ofList
        |> System.String
    )

let run =
    fun (parser: Parser<'result>) (string: string) ->
        parser (
            List.ofArray
            <| string.ToCharArray()
        )
