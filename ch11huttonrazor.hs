module Chapter11HuttonRazor where

data Expr =
    Lit Integer
  | Add Expr Expr
  | Group Expr

eval :: Expr -> Integer
eval (Lit n)           = n
eval (Add lexpr rexpr) = eval lexpr + eval rexpr
eval (Group expr)      = eval expr

printExpr :: Expr -> String
printExpr (Lit n)           = show n
printExpr (Add lexpr rexpr) = printExpr lexpr ++ " + " ++ printExpr rexpr
printExpr (Group expr)      = "(" ++ printExpr expr ++ ")"
