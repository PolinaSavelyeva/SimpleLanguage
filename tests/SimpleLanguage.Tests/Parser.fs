module SimpleLanguage.Tests.Parser

open Expecto
open SimpleLanguage.AST
open Generators
open SimpleLanguage.Main

let config =
    { FsCheckConfig.defaultConfig with
        arbitrary = [ typeof<Generators.Generators> ] }


[<Tests>]
let tests =
    testList
        "program parser tests"
        [ testPropertyWithConfig config "AST generated from code is the same as the previous one AST"
          <| fun (generatedAST: AST) ->
              let code = generateCodeFromAST generatedAST
              let ast = getProgramAST code
              Expect.equal ast generatedAST "Generated ASTs were expected to be equal" ]
