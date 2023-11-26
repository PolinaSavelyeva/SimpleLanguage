namespace SimpleLanguage

open Interpreter
open Parser

module Main =

    let getProgramAST (program: string) =
        (program.ToCharArray() |> List.ofArray |> programParser).Value |> snd

    let evaluateProgram (program: string) =
        let charListProgram = program.ToCharArray() |> List.ofArray
        let programParserResult = programParser charListProgram

        match programParserResult with
        | Some([], result) -> evaluateAST result
        | Some(hd, tl) -> printfn $"Error %A{hd} + %A{tl}"
        | None -> printfn "Syntax error."

    [<EntryPoint>]
    let main _ = 0
