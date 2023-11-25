module SimpleLanguage.AST

type Expression =
    | Number of int
    | Mult of list<Expression>
    | Add of list<Expression>
    | Variable of string

type Statement =
    | Assignment of variableName: string * expression: Expression
    | Print of Expression
    | Condition of condition: Expression * trueAndElseBranches: (list<Statement> * list<Statement>)

type AST = list<Statement>
