module SimpleLanguage.Tests.Parser

open Expecto
open Generators
open SimpleLanguage.Main
open SimpleLanguage.AST

let config =
    { FsCheckConfig.defaultConfig with
        arbitrary = [ typeof<Generators.Generators> ] }


[<Tests>]
let tests =
    testList
        "program parser tests"
        [ testPropertyWithConfig config "AST generated from code is the same as the previous one AST"
          <| fun (generatedAST: ASTWrapper) ->
              let code = generateCodeFromAST generatedAST
              let ast = getProgramAST code
              Expect.equal ast generatedAST "ASTs were expected to be equal"

          testCase "AST generated from simple print-command-code is the same as expected one"
          <| fun _ ->
              let code = "print:x<true"
              let ast = getProgramAST code

              let expectedAST =
                  [ Print(Or [ And [ Compare("<", [ Variable "x"; Boolean true ]) ] ]) ]

              Expect.equal ast expectedAST "ASTs were expected to be equal"
          testCase "AST generated from simple assignment-command-code is the same as expected one"
          <| fun _ ->
              let code = "x=true&&false<false"
              let ast = getProgramAST code

              let expectedAST = [Assignment
                       ("x",
                        Or
                          [And
                             [Compare ("<", [Boolean true]);
                              Compare ("", [Boolean false; Boolean false])]])]

              Expect.equal ast expectedAST "ASTs were expected to be equal" ]
