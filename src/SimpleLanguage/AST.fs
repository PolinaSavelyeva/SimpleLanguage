module SimpleLanguage.AST

type sourceExpr =
    | Number of int
    | Mult of list<sourceExpr>
    | Add of list<sourceExpr>
    | Var of string

type sourceAst =
    | Asgn of string * sourceExpr
    | Print of sourceExpr
