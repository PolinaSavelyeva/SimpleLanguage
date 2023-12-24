module SimpleLanguage.Tests.CodeAnalyzer

open Expecto
open Generators
open SimpleLanguage.CodeAnalyzer
open SimpleLanguage.AST
open SimpleLanguage.Interpreter
open Helper

[<Tests>]
let tests =
    testList
        "code analyzer tests"
        [ testPropertyWithConfig config "Evaluated program results from optimised AST are the same as the evaluated program results from previous one AST"
          <| fun (generatedAST: ASTWrapper) ->
              let optimisedAst = optimiseAST generatedAST

              try
                  listAST generatedAST
              with _ ->
                  Expect.throws (fun _ -> listAST optimisedAst) "Optimised AST should not evaluate when the original does not"

              Expect.equal (listAST generatedAST) (listAST optimisedAst) "Optimised AST should be evaluated when the original does"

          testCase "Optimised AST is the same as expected one"
          <| fun _ ->
              let actualResult = optimiseAST codeAST

              let expectedResult =
                  [ Assignment("x", Or [ And [ Compare("<", [ Boolean true; Boolean true ]) ] ])
                    Print(Or [ And [ Compare("<", [ Variable "x"; Boolean false ]) ] ])
                    Assignment("x", Or [ And [ Compare("", [ Add [ Mult [ Number 3 ]; Mult [ Number 4 ]; Mult [ Number 5 ] ] ]) ] ])
                    Print(Or [ And [ Compare("", [ Boolean true ]); Compare("<", [ Boolean false; Boolean true ]) ]; And [ Compare("", [ Boolean false ]) ] ]) ]

              Expect.equal actualResult expectedResult "ASTs were expected to be equal" ]
