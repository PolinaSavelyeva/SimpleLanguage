module SimpleLanguage.Tests.CodeGenerator

open Expecto
open Generators
open Helper

[<Tests>]
let tests =
    testList
        "tests for code generator function"
        [ testCase "Generated code from AST is the same with the previous code"
          <| fun _ ->

              let generatedCode = generateCodeFromAST codeAST

              Expect.equal code generatedCode "Generated code were expected to be equal" ]
