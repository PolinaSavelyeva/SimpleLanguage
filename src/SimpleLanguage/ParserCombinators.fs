module SimpleLanguage.ParserCombinators

type Parser<'result> = list<char> -> Option<list<char> * 'result>

let satisfyPredicate predicate : Parser<char> =
    fun charList ->
        match charList with
        | firstElementOfList :: restOfList when predicate firstElementOfList -> Some(restOfList, firstElementOfList)
        | _ -> None

let makeCharParser char : Parser<char> =
    satisfyPredicate (fun currentChar -> currentChar = char)

let epsilonParser: Parser<unit> = fun charList -> Some(charList, ())

let bindParsers (parser: Parser<'result1>) (parseFunction: 'result1 -> Parser<'result2>) : Parser<'result2> =
    fun charList ->
        match parser charList with
        | None -> None
        | Some(restOfList, result) -> parseFunction result restOfList

let makeAlternativeParser (parser1: Parser<'result>) (parser2: Parser<'result>) : Parser<'result> =
    fun charList ->
        match parser1 charList with
        | None -> parser2 charList
        | parserResult -> parserResult

let mapParser (mapping: 'result1 -> 'result2) (parser: Parser<'result1>) : Parser<'result2> =
    fun charList ->
        match parser charList with
        | None -> None
        | Some(restOfList, result) -> Some(restOfList, mapping result)

let reverseParser (result2Value: 'result2) (parser: Parser<'result1>) : Parser<'result2> =
    fun charList ->
        match parser charList with
        | None -> Some(charList, result2Value)
        | _ -> None

let rec manyParser (parser: Parser<'result>) : Parser<list<'result>> =
    makeAlternativeParser (bindParsers parser (fun result -> mapParser (fun tl -> result :: tl) (manyParser parser))) (mapParser (fun _ -> []) epsilonParser)

let someParser (parser: Parser<'result>) : Parser<list<'result>> =
    bindParsers parser (fun result -> mapParser (fun tl -> result :: tl) (manyParser parser))

let makeListParser (elementParser: Parser<'element>) (epsilonParser: Parser<unit>) : Parser<list<'element>> =
    bindParsers elementParser (fun element -> mapParser (fun tl -> element :: tl) (manyParser (bindParsers epsilonParser (fun _ -> elementParser))))

let makeIgnoreParser parser = mapParser (fun _ -> ()) parser

let makeKeywordParser (keyword: string) : Parser<string> =
    let chars = keyword.ToCharArray()

    Array.fold (fun parser currentChar -> bindParsers parser (fun result -> mapParser (fun char -> char :: result) (makeCharParser currentChar))) (mapParser (fun _ -> []) epsilonParser) chars
    |> mapParser (fun charList -> charList |> List.rev |> Array.ofList |> System.String)

let run =
    fun (parser: Parser<'result>) (string: string) -> parser (List.ofArray <| string.ToCharArray())
