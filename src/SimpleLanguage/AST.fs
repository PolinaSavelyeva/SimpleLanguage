module SimpleLanguage.AST

type Expression =
    | Number of int
    | Boolean of bool
    | Mult of list<Expression>
    | Add of list<Expression>
    | Variable of string
    | And of list<Expression>
    | Or of list<Expression>
    | Compare of comparisonSymbol: string * list<Expression>

type Statement =
    | Assignment of variableName: string * expression: Expression
    | Print of Expression
    | Condition of condition: Expression * trueAndElseBranches: (list<Statement> * list<Statement>)

type AST = list<Statement>
