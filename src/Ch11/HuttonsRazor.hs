module Ch11.HuttonsRazor where
    data Expr = Lit Integer | Add Expr Expr
    
    eval :: Expr -> Integer
    eval (Lit i) = i
    eval (Add left right) = eval left + eval right

    printExpr :: Expr -> String
    printExpr (Lit i) = show i
    printExpr (Add left right) = printExpr left ++ " + " ++ printExpr right
