module SimpleLanguage.Tests.CodeGenerator

open Expecto
open Generators
open SimpleLanguage.Parser
open SimpleLanguage.Main

[<Tests>]
let tests =
    testList
        "tests for code generator function"
        [ testCase "Generated code from AST is the same with the previous code"
          <| fun _ ->

              let code =
                  "if:true>falsethen:
x=true<true
print:x<false
else:
x=true
if:x==truethen:
x=true
else:
x=true
end
print:true
x=true
end
x=3+4+5
print:true&&false<true||false
"

              let ast = getProgramAST code
              let generatedCode = generateCodeFromAST ast

              Expect.equal code generatedCode "Generated code were expected to be equal" ]
