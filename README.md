## Evaluator of Lambda-terms

Program with function **'eval'** resolves lambda-terms which are stored in data structure Expr.  
  
`data Expr = Var Symbol | App Expr Expr | Lambda Symbol Expr deriving Eq`  

### Input
  
lambda-term stored in Expr data structure  

### Output

reduction of given lambda-term stored in Expr data structure using *Applicative Order Evaluation*

### Example(s)

`eval(Var "x")`   
`x`  
  
`eval(App (Lambda "x" (Var "x")) (Var "y"))`  
`y`
