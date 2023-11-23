module SimpleLanguage.AST

type Expression =
    | Number of int
    | Mult of list<Expression>
    | Add of list<Expression>
    | Variable of string

type Statement =
    | Assignment of variableName: string * expression: Expression
    | Print of Expression

type AST = list<Statement>
