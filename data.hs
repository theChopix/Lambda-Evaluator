--test cases

type Symbol = String
data Expr = Var Symbol | App Expr Expr | Lambda Symbol Expr deriving Eq

test1 :: Expr
test1 = Var "x"

test2 :: Expr
test2 = Lambda "x" (Var "x")

test3 :: Expr
test3 = App (Lambda "x" (Var "x")) (Var "y")

test4 :: Expr
test4 = Lambda "s" (Lambda "z" (App (Var "s") (App (Var "s") (Var "z"))))

--

test5 :: Expr
test5 = App (Var "x") (Var "y")

test6 :: Expr
test6 = Lambda "x" (Var "x")

test7 :: Expr 
test7 = App (Lambda "x" (Var "x")) (Var "y")

test8 :: Expr
test8 = App (Lambda "x" (Lambda "y" (App (Var "x") (Var "y")))) (Var "y")

test9 :: Expr
test9 = App (Lambda "x" (Lambda "y" (Var "y"))) (Var "y")

test10 :: Expr
test10 = App (Lambda "x"
            (Lambda "y"
                (Lambda "z" (App (App (Var "x") (Var "y")) (Var "z")))))
                    (App (Var "y") (Var "z"))
-- should be (\a0.(\a1.(((y z) a0) a1)))

one :: Expr
one = Lambda "s" (Lambda "z" (App (Var "s") (Var "z")))
suc :: Expr
suc = Lambda "w"
        (Lambda "y"
            (Lambda "x"
                (App (Var "y")
                    (App (App (Var "w") (Var "y"))
                        (Var "x")
                    )
                )   
            )
        )

test12 :: Expr
test12 = App suc one

test13 :: Expr
test13 = App (Lambda "z" one) (App (Var "s") (Var "z"))