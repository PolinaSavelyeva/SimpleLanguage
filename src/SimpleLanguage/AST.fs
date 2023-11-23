module SimpleLanguage.AST

type CompareOperator =
    | Equal
    | NotEqual
    | LessThan
    | LessThanOrEqual
    | GreaterThan
    | GreaterThanOrEqual

type Expression =
    | Number of int
    | Mult of list<Expression>
    | Add of list<Expression>
    | Variable of string
    | Compare of left: Expression * operator: CompareOperator * right: Expression

type Statement =
    | Assignment of variableName: string * expression: Expression
    | Print of Expression
    | Condition of Expression * thenBranch: list<Statement> * elseBranch: list<Statement>

type AST = list<Statement>
