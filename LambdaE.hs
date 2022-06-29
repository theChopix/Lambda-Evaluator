module LambdaE where

type Symbol = String
data Expr = Var Symbol | App Expr Expr | Lambda Symbol Expr deriving Eq

instance Show Expr where
    show (Var x) = x
    show (App y w) = "(" ++ show y ++ " " ++ show w ++ ")"
    show (Lambda z q) = "(\\" ++ z ++ "." ++ show q ++ ")"

symbols :: [Symbol]
symbols = ["a" ++ show i | i <- [0..]]

takeSymbol :: Int -> Symbol
takeSymbol n = last (take n symbols)

symbolInExpression :: Symbol -> Expr -> Bool
symbolInExpression s (App e1 e2) = (symbolInExpression s e1) || (symbolInExpression s e2)
symbolInExpression s (Var x) 
                        | s == x = True
                        | otherwise = False
symbolInExpression s (Lambda sy e) = (s == sy) || (symbolInExpression s e)

substituteHlp :: Symbol -> Expr -> Expr -> Int -> (Expr, Int)
substituteHlp x (App e1 e2) z n = let
                                    (first, n') = substituteHlp x e1 z n
                                    (second, n'') = substituteHlp x e2 z n'
                                    in ((App first second), n'')
substituteHlp x (Var y) z n
            | x == y = (z, n)
            | otherwise = (Var y, n)
substituteHlp x (Lambda s e) z n
                    | (Var x) == (Var s) = ((Lambda s e), n) 
                    | (symbolInExpression s z) = let 
                                        (inner, n') = substituteHlp s e (Var (takeSymbol n)) (n+1)
                                        (outer, n'') = substituteHlp x inner z n'
                                        in (Lambda (takeSymbol n) outer, n'')
                    | otherwise = let 
                                    (first, n') = substituteHlp x e z n
                                    in (Lambda s first, n')

applyHlp :: Expr -> Expr-> Int -> (Expr, Int)
applyHlp (Lambda x y) e2 n = let 
                                    (only, n') = substituteHlp x y e2 n
                                    in evalHlp only n'

applyHlp (Var x) e2 n = (App (Var x) e2, n)
applyHlp (App e1 e2) e3 n = (App (App e1 e2) e3, n)

evalHlp :: Expr -> Int -> (Expr, Int)
evalHlp (Var x) n = (Var x, n) 
evalHlp (App y w) n = let 
                        (first , n') = evalHlp y n
                        (second , n'') = evalHlp w n'
                        in applyHlp first second n''
evalHlp (Lambda z q) n = let 
                            (only , n') = evalHlp q n
                            in (Lambda z only, n')                    

eval :: Expr -> Expr
eval e = fst (evalHlp e 1)

