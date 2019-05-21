--4
data ExprCL = AppCL ExprCL ExprCL | K | S | VarCL Int deriving (Show)
data Cocktail =  Lmd Int ExprCL
translate :: Expr -> ExprCL
translate (Var x) = (VarCL x)
translate (App x y) = AppCL (translate x) (translate y) 
translate (Lam x y) = if elem x (freeVariables y)
	then case y of
		(Var a) -> AppCL (AppCL S K) K
		(Lam a b) -> translateCL (Lmd x (translate (Lam a b)))
		(App h i) -> AppCL (AppCL S (translate (Lam x h))) (translate (Lam x i))
	else AppCL K (translate y)


freeVariablesCL :: ExprCL -> [Int]
freeVariablesCL (VarCL x) = [x]
freeVariablesCL (AppCL x y) = freeVariablesCL x ++ freeVariablesCL y
freeVariablesCL _ = []


translateCL :: Cocktail -> ExprCL
translateCL (Lmd x y) = if elem x (freeVariablesCL y)
	then case y of
		(VarCL a) -> AppCL (AppCL S K) K
		(AppCL a b) -> AppCL (AppCL S (translateCL (Lmd x a))) (translateCL (Lmd x b))
		K -> AppCL K K
		S -> AppCL K S
	else AppCL K y