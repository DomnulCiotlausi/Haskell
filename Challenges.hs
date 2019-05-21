-- Seriosly, it is incredible that the coursework got changed 4 times from the beginning.
-- Please, next time do the coursework right on the first try because it is not acceptable 
-- to have to change what a I already done just because you decided to change the problem.

import Data.Char
import Control.Applicative

data Expr = Var Int | App Expr Expr | Lam Int Expr deriving Show

-- 1a
freeVariables :: Expr -> [Int]
freeVariables (Var x) = [x]
freeVariables (App x y) = freeVariables x ++ freeVariables y
freeVariables (Lam x expr) = [y | y <- freeVariables expr, y/=x]

--1b
rename :: Expr -> Int -> Int -> Expr
rename (Var x) y z = (Var x)
rename (App expr1 expr2) y z = App (rename expr1 y z) (rename expr2 y z)
rename (Lam x expr) y z = if x/=y then Lam x (rename expr y z) else Lam z (rename2 expr y z)

rename2 :: Expr -> Int -> Int -> Expr
rename2 (Var x) y z = if x==y then Var z else Var y
rename2 (App expr1 expr2) y z = App (rename2 expr1 y z) (rename2 expr2 y z)
rename2 (Lam x expr) y z = if x==y then Lam x expr else Lam x (rename2 expr y z)
		
--1c
alphaEquivalent :: Expr -> Expr -> Bool
alphaEquivalent (Var x) (Var y) = x==y
alphaEquivalent (Var _) expr = False
alphaEquivalent expr (Var _) = False
alphaEquivalent (App expr1 expr2) (App expr3 expr4) = alphaEquivalent expr1 expr3 && alphaEquivalent expr2 expr4
alphaEquivalent (App _ _) expr = False
alphaEquivalent expr (App _ _) = False
alphaEquivalent (Lam x expr1) (Lam y expr2) = if x==y then alphaEquivalent expr1 expr2 else alphaEquivalent expr1 (rename2 expr2 y x)

--1d
hasRedex :: Expr -> Bool
hasRedex (Var _) = False
hasRedex (App (Lam _ _) _) = True
hasRedex (App expr1 expr2) = hasRedex expr1 || hasRedex expr2
hasRedex (Lam _ expr) = hasRedex expr

--1e
substitute :: Expr -> Int -> Expr -> Expr
substitute (Var x) r expr = if x==r then expr else (Var x)
substitute (App expr1 expr2) r expr = App (substitute expr1 r expr) (substitute expr2 r expr)
substitute (Lam x expr1) r expr2 = if x==r 
	then (Lam x expr1) 
	else if notElem x (freeVariables expr2)
		then (Lam x (substitute expr1 r expr2))
		else substitute (rename (Lam x expr1) x z) r expr2
			where
				z = [a | a <- [1..], notElem a (freeVariables expr2)]!!0


--2
prettyPrint :: Expr -> String
prettyPrint (Var x) = "x" ++ show x
prettyPrint (App (Lam x expr1) expr2) = "(" ++ prettyPrint (Lam x expr1) ++ ")" ++ prettyPrint expr2
prettyPrint (App expr1 (Lam x expr2)) = prettyPrint expr1 ++ "(" ++ prettyPrint (Lam x expr2) ++ ")"
prettyPrint (App (App expr1 expr2) expr3) = "(" ++ prettyPrint (App expr1 expr2) ++ ")" ++ prettyPrint expr3
prettyPrint (App expr1 (App expr2 expr3)) = prettyPrint expr1 ++ "(" ++ prettyPrint (App expr2 expr3) ++ ")"
prettyPrint (App expr1 expr2) = prettyPrint expr1 ++ prettyPrint expr2
prettyPrint (Lam x (Lam y expr)) = "\\x" ++ show x ++ prettyPrint2 (Lam y expr)
	where
		prettyPrint2 (Lam x (Lam y expr)) = "x" ++ show x ++ prettyPrint2 (Lam y expr)
		prettyPrint2 (Lam x expr) = "x" ++ show x ++ "->" ++ prettyPrint expr
prettyPrint (Lam x expr) = "\\x" ++ show x ++ "->" ++ prettyPrint expr


--3
data ExtExpr = ExtApp ExtExpr ExtExpr | ExtLam [Int] ExtExpr | ExtVar Int deriving (Show, Eq)

newtype Parser a = P (String -> [(a, String)])

--taken from "Programming in Haskell" by Graham Hutton

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

item :: Parser Char 
item = P (\inp -> case inp of
	[]		-> []
	(x:xs) 	-> [(x,xs)])
	
instance Functor Parser where
	fmap g p = P (\inp -> case parse p inp of
		[]			-> []
		[(v,out)]	-> [(g v, out)])
		
instance Applicative Parser where 
	pure v = P (\inp -> [(v,inp)])
	pg <*> px = P (\inp -> case parse pg inp of
		[]			-> []
		[(g,out)]	-> parse (fmap g px) out)

instance Monad Parser where 
	p >>= f = P (\inp -> case parse p inp of
		[]			-> []
		[(v,out)]	-> parse (f v) out)
		
instance Alternative Parser where
	empty = P (\inp -> [])
	p <|> q = P (\inp -> case parse p inp of
		[]			-> parse q inp
		[(v,out)]	-> [(v,out)])

sat :: (Char -> Bool) -> Parser Char
sat p = do 	
		x <- item
		if p x then return x else empty
			
char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String 
string [] = return []
string (x:xs) = do 	
				char x
				string xs 
				return (x:xs)

digit :: Parser Char
digit = sat isDigit				
				
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

--my part
var :: Parser ExtExpr
var = do 
		char 'x'
		x <- nat
		return (ExtVar x)

vars :: Parser [Int]
vars = do 
		char 'x'
		x <- nat 
		xs <- many (do 
					char 'x'
					nat)
		return (x:xs)

parser :: Parser ExtExpr
parser = do
			x <- var
			return x
		<|> do
			symbol "("
			x <- expr
			symbol ")"
			return x
		<|> do
			symbol "\\"
			x <- vars
			symbol "->"
			y <- expr
			return (ExtLam x y)
		<|> empty
		
expr :: Parser ExtExpr
expr = do
		xs <- many (parser)
		if length xs == 1 then return (head xs) else return (tree xs)
			where 
				tree (x:y:xs) = foldl f (ExtApp x y) xs
				f x y = ExtApp x y
		
parseLam :: String -> Maybe ExtExpr
parseLam xs = case (parse expr xs) of 
				[(x, [])] -> Just x
				_ -> Nothing

--4				
data ExprCL = AppCL ExprCL ExprCL | K | S | VarCL Int deriving (Show)
data Combination = Lamda Int ExprCL		
				
translate :: Expr -> ExprCL
translate (Var x) = (VarCL x)
translate (App expr1 expr2)	= AppCL (translate expr1) (translate expr2)
translate (Lam x expr) = if elem x (freeVariables expr)
	then case expr of 
		(Var a) -> AppCL (AppCL S K) K
		(Lam a b) -> translateCL (Lamda x (translate (Lam a b)))
		(App expr3 expr4) -> AppCL (AppCL S (translate (Lam x expr3))) (translate (Lam x expr4))
	else AppCL K (translate expr)

freeVariablesCL :: ExprCL -> [Int]
freeVariablesCL (VarCL x) = [x]
freeVariablesCL (AppCL expr1 expr2) = freeVariablesCL expr1 ++ freeVariablesCL expr2
freeVariablesCL _ = []

translateCL :: Combination -> ExprCL
translateCL (Lamda x expr) = if elem x (freeVariablesCL expr)
	then case expr of
		(VarCL y) -> AppCL (AppCL S K) K
		(AppCL expr1 expr2) -> AppCL (AppCL S (translateCL (Lamda x expr1))) (translateCL(Lamda x expr2))
		K -> AppCL K K
		S -> AppCL K S
	else AppCL K expr