import Data.Char
import Control.Applicative

data Expr = App Expr Expr | Lam Int Expr | Var Int deriving (Show, Eq)

--1a
freeVariables :: Expr -> [Int]
freeVariables (Var x) = [x]
freeVariables (App x y) = freeVariables x ++ freeVariables y
freeVariables (Lam x ex) = [m | m<-freeVariables ex, m/=x]

--1b
rename :: Expr -> Int -> Int -> Expr
rename (Var x) y z = (Var x)
rename (App ex1 ex2) x y = (App (rename ex1 x y) (rename ex2 x y))
rename (Lam x ex) y z = if x==y 
	then (Lam z (renameWithin ex y z))
	else (Lam x (rename ex y z))

renameWithin (Var x) y z = if x==y then (Var z) else (Var x)
renameWithin (App ex1 ex2) x y = (App (renameWithin ex1 x y) (renameWithin ex2 x y))
renameWithin (Lam x ex) y z = if x==y 
	then (Lam x ex)
	else (Lam x (renameWithin ex y z))

--1c
alphaEquivalent :: Expr -> Expr -> Bool
alphaEquivalent (Var x) (Var y) = x==y
alphaEquivalent (Var _) ex = False
alphaEquivalent (App ex1 ex2) (App ex3 ex4) =
	alphaEquivalent ex1 ex3 && alphaEquivalent ex2 ex4
alphaEquivalent (App _ _) ex = False
alphaEquivalent (Lam x ex1) (Lam y ex2) = if x==y 
	then alphaEquivalent ex1 ex2
	else alphaEquivalent ex1 (renameWithin ex2 y x)
alphaEquivalent (Lam _ _) ex = False

--1d
hasRedex :: Expr -> Bool
hasRedex (Var _) = False
hasRedex (Lam _ ex) = hasRedex ex
hasRedex (App (Lam _ _) _) = True
hasRedex (App ex1 ex2) = hasRedex ex1 || hasRedex ex2

--1e
substitute :: Expr -> Int -> Expr -> Expr
substitute (Var x) y e = if x==y then e else (Var x)
substitute (App ex1 ex2) x e = (App (substitute ex1 x e) (substitute ex2 x e))
substitute (Lam x ex) y e = if x==y
	then (Lam x ex)
	else if elem x (freeVariables e) 
		then substitute (rename (Lam x ex) x nxt) y e
		else (Lam x (substitute ex y e))
		where
			nxt = maximum (freeVariables e ++ freeVariables ex) + 1

--2
prettyPrint :: Expr -> String
prettyPrint (Var i) = "x" ++ show i

prettyPrint (App (Lam i ex1) ex2) = "(" ++ prettyPrint (Lam i ex1) ++
	")" ++ prettyPrint ex2

prettyPrint (App ex1 (App ex2 ex3)) = prettyPrint ex1 ++ "(" ++
	prettyPrint (App ex2 ex3) ++ ")"

prettyPrint (App ex1 ex2) = prettyPrint ex1 ++ prettyPrint ex2

prettyPrint (Lam i (Lam j ex)) = "\\x" ++ show i ++	printNoLam (Lam j ex)

prettyPrint (Lam i ex) = "\\x" ++ show i ++ "->" ++ prettyPrint ex

printNoLam (Lam i (Lam j ex)) = "x" ++ show i ++ printNoLam (Lam j ex)

printNoLam (Lam i ex) = "x" ++ show i ++ "->" ++ prettyPrint ex


--3 
--code from book
data ExtExpr = ExtApp ExtExpr ExtExpr | ExtLam [Int] ExtExpr | ExtVar Int
	deriving (Show, Eq)

newtype Parser a = P (String -> [(a,String)])

parse :: Parser a -> String -> [(a,String)]
parse (P p) x = p x

item :: Parser Char
item = P (\cs -> case cs of 
	"" -> []
	(c:cs) -> [(c,cs)])

instance Functor Parser where
	fmap g p = P (\x -> case parse p x of
		[] -> []
		[(v, out)] -> [(g v, out)]) 

instance Applicative Parser where
	pure x = P (\y -> [(x, y)])
	pg <*> px = P (\y -> case parse pg y of
		[] -> []
		[(g, out)] -> parse (fmap g px) out)

instance Monad Parser where
	p >>= f = P (\inp -> case parse p inp of
		[] -> []
		[(v, out)] -> parse (f v) out)

instance Alternative Parser where
	empty = P (\inp -> [])
	p <|> q = P (\inp -> case parse p inp of
		[] -> parse q inp
		[(v, out)] -> [(v, out)])
 
sat :: (Char -> Bool) -> Parser Char
sat p = do
	x <- item
	if p x then return x else empty

digit :: Parser Char
digit = sat isDigit

char :: Char -> Parser Char
char x = sat (==x)

string :: String -> Parser String
string [] = return []
string (x:xs) = do
	char x
	string xs
	return (x:xs)

nat :: Parser Int
nat = do
	xs <- some digit
	return (read xs)

space :: Parser ()
space = do
	many (sat isSpace)
	return ()

token :: Parser a -> Parser a
token p = do
	space
	v <- p
	space
	return v

symbol :: String -> Parser String
symbol xs = token (string xs)


--mycode
var :: Parser ExtExpr
var = do
	char 'x'
	i <- nat
	return (ExtVar i)

vars :: Parser [Int]
vars = do
	char 'x'
	x <- nat
	xs <- many (do
		char 'x'
		nat)
	return (x:xs)

expr :: Parser ExtExpr
expr = do
	  	xs <- many (dummy)
	  	if length xs == 1 
			then return $ head xs
			else return $ mkTree xs
	where
		mkTree (x:y:xs) = foldl f (ExtApp x y) xs
		f y x = ExtApp y x

dummy :: Parser ExtExpr
dummy = do
	  	symbol "("
	  	y <- expr
	  	symbol ")"
	  	return y
	  <|> do
	  	x <- var
	  	return x
	  <|> do
	  	symbol "\\"
	  	y <- vars
	  	symbol "->"
	  	z <- expr
	  	return (ExtLam y z)
	  <|> empty


parseLam :: String -> Maybe ExtExpr
parseLam xs = case (parse expr xs) of
	[(n, [])] -> Just n
	_ -> Nothing

--4
data ExprCL = AppCL ExprCL ExprCL | K | S | VarCL Int deriving (Show)
data Mixed = Lm Int ExprCL

translate :: Expr -> ExprCL
translate (Var x) = VarCL x
translate (App ex1 ex2) = AppCL (translate ex1) (translate ex2)
translate (Lam x ex1) = if elem x (freeVariables ex1)
	then case ex1 of
		(Var y) -> AppCL (AppCL S K) K
		(Lam y ex2) -> translateMixed (Lm x (translate (Lam y ex2)))
		(App ex3 ex4) -> AppCL (AppCL S (translate (Lam x ex3))) (translate (Lam x ex4))
	else AppCL K (translate ex1)

freeVariablesCL :: ExprCL -> [Int]
freeVariablesCL (VarCL x) = [x]
freeVariablesCL (AppCL ex1 ex2) = freeVariablesCL ex1 ++ freeVariablesCL ex2
freeVariablesCL _ = []

translateMixed :: Mixed -> ExprCL
translateMixed (Lm x ex1) = if elem x (freeVariablesCL ex1)
	then case ex1 of
		(VarCL y) -> AppCL (AppCL S K) K
		(AppCL ex2 ex3) -> AppCL (AppCL S (translateMixed (Lm x ex2))) (translateMixed (Lm x ex3))
		K -> AppCL K K
		S -> AppCL K S
	else AppCL K ex1