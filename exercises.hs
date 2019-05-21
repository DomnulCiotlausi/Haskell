-- Why would you restrict the libraries we can use?
-- I believe if there is a simpler and more eficient way to do a thing, then I should be able and allowed to do it that way
-- The worst resources are the ones you don't use
import Data.List
import Data.Char

-- 1
subtotal :: Num a => [a] -> [a]
subtotal [] = []
subtotal xs = subtotal (init xs) ++ [sum(xs)]

-- 2
histogram :: Int -> [Int] -> [Int]
histogram _ [] = []
histogram 0 _ = []
histogram x xs = f1 0 x xs

f1 :: Int -> Int -> [Int] -> [Int]
f1 a b xs = if maximum xs >= a
				then [count a b xs] ++ f1 b (2*b-a) xs 
				else []
					where 
						count :: Int -> Int -> [Int] -> Int
						count x y xs = length [a | a <- xs, a >= x, a<y]

-- 3
meetsOffer :: String -> Int -> Bool
meetsOffer "" x = if x<=0 then True else False
meetsOffer (c:cs) x 
	| c=='A' = meetsOffer cs (x-48)
	| c=='B' = meetsOffer cs (x-40)
	| c=='C' = meetsOffer cs (x-32)
	| c=='D' = meetsOffer cs (x-24)
	| c=='E' = meetsOffer cs (x-16)
	| c=='*' = meetsOffer cs (x-8)

-- 4
data TypeOfSort = Ascending | NonDescending | Constant | NonAscending | Descending | NotSorted
	deriving Show

sortType :: [Int] -> TypeOfSort 
sortType xs 
	| ascending xs		= Ascending
	| nonAscending xs 	= NonAscending
	| descending xs 	= Descending
	| nonDescending xs 	= NonDescending
	| otherwise 		= NotSorted

ascending :: [Int] -> Bool
ascending [] = True
ascending [x] = True
ascending (x:y:xs) = x < y && ascending (y:xs)

nonAscending :: [Int] -> Bool
nonAscending [] = True
nonAscending [x] = True
nonAscending (x:y:xs) = x <= y && nonAscending (y:xs)

descending :: [Int] -> Bool
descending [] = True
descending [x] = True
descending (x:y:xs) = x > y && descending (y:xs)

nonDescending :: [Int] -> Bool
nonDescending [] = True
nonDescending [x] = True
nonDescending (x:y:xs) = x >= y && nonDescending (y:xs)

-- 5
rpcalc :: (Read a, Fractional a) => [Char] -> a
rpcalc xs = head (foldl foldingFunction [] (map f4 xs))
	where 
		f4 :: Char -> [Char]
		f4 x = [x]
		foldingFunction :: (Fractional a, Read a) => [a] -> [Char] -> [a]
		foldingFunction (x:y:ys) "*" = (x * y):ys
		foldingFunction (x:y:ys) "+" = (x + y):ys
		foldingFunction (x:y:ys) "-" = (y - x):ys
		foldingFunction (x:y:ys) "/" = (y / x):ys
		foldingFunction xs numberString = read numberString:xs

-- 6
neighbours :: (Num a, Floating a, Ord a) => Int -> (a,a) -> [(a,a)] -> [(a,a)]
neighbours k p xs = eliminate(take k (sortBy (compare) [(a,b,d) | (a,b) <- xs, let d = distance p (a,b)]))
	where
		third (_, _, c) = c
		distance x y = sqrt((fst x - fst y)^2 + (snd x - snd y)^2)
		eliminate [] = []
		eliminate xs = [(a,b) | (a,b,c) <- xs]
		compare x y
			| (third x)<(third y) = LT
			| (third x)>(third y) = GT
			| otherwise = EQ
			
-- 7
data SearchTree = Node SearchTree Int SearchTree | Leaf Int

balanced :: SearchTree -> Bool
balanced (Leaf _) = True
balanced (Node x y z) = if val x<y && y<val z && d>=(-1) && d<=1 && balanced x && balanced z then True else False
	where 
		val :: SearchTree -> Int
		val (Leaf x) = x
		val (Node a b c) = b
		height :: SearchTree -> Int
		height (Leaf _) = 1
		height (Node x y z) = (max (height x) (height z)) + 1
		dif :: SearchTree -> SearchTree -> Int
		dif x y = height x - height y
		d = dif x z
-- 8
newtonRootSequence :: Double -> [Double]
newtonRootSequence d = [1] ++ init (f3 1 d)

f3 :: Double -> Double -> [Double]
f3 x d = [f x d] ++ f3 (f x d) d
	where
		f x d = ((x + (d/x)) / 2)

newtonRoot :: Double -> Double -> Double
newtonRoot d e = get (f3 1 d)
	where 
		get (x:y:xs) = if abs(x-y)<e then y else get (y:xs)

-- 9
hyperOperator :: Int -> Int -> Int -> Int
hyperOperator n a b 
	| n == 0			= b+1
	| n == 1 && b==0	= a 
	| n == 2 && b==0	= 0
	| n >= 0 && b==0	= 1
	| otherwise 		= hyperOperator (n-1) a (hyperOperator n a (b-1))


-- 10
encode :: String -> [Int]
encode "" = []
encode (x:xs) = addBit ++ encode xs
	where 
		enc = [if m == True then 1 else 0 | m <- map (testBit (ord x)) [7,6..0]]
			where
				recalculate n = [n `mod` 2] ++ recalculate (n `div` 2)
				testBit x y = if (recalculate x)!!y == 1 then True else False
		addBit = enc ++ [if length (filter (==1) enc) `mod` 2 == 1 then 1 else 0]

-- 11
decode :: [Int] -> String
decode [] = []
decode xs = if length xs `mod` 9 == 0 && test xs then 
	[chr $ foldr f4 0 (zip byte [7,6..0])] ++ decode (drop 9 xs)
	else ""
	where 
		test :: [Int] -> Bool
		test [] = True
		test xs = if length (filter (==1) (take 9 xs)) `mod` 2 == 1 then False else test (drop 9 xs)
		byte = take 8 $ take 9 xs
		f4 :: (Int, Int) -> Int -> Int
		f4 a b = fst a * 2 ^ snd a + b 
		
-- 12
makeChange :: Int -> [Int] -> [Int]
makeChange n coins = reverse . snd $ foldl next (n, []) (coins)
    where next (remaining, cs) coin
            | coin <= remaining = (rem, cnt:cs)
            | otherwise         = (remaining, 0:cs)
            where rem = remaining `mod` coin
                  cnt = remaining `div` coin

-- 13
goodsteinSequence :: (Int, [Int]) -> [(Int, [Int])]
goodsteinSequence (n, []) = [(n, [])]
goodsteinSequence (n, xs) = [(n, xs)] ++ goodsteinSequence ((n+1), (recalculate ((calculate (n+1) 0 xs)-1) (n+1)))
	where 
		calculate n a [] = 0
		calculate n a (x:xs) = x*(n^a) + calculate n (a+1) xs
		recalculate 0 _ = []
		recalculate n b = [n `mod` b] ++ recalculate (n `div` b) b
		
--14
-- a lot of these functions were taken from the "Programming in haskell" by Graham Hutton
data Prop = Const Bool | Var Char | Not Prop | And Prop Prop | Imply Prop Prop

type Assoc k v = [(k, v)]
fnd :: Eq  k => k -> Assoc k v -> v
fnd k t = head [v | (k1, v) <- t, k == k1]

type Subst = Assoc Char Bool

eval :: Subst -> Prop -> Bool
eval _ (Const b) = b
eval s (Var x) = fnd x s
eval s (Not p) = not (eval s p)
eval s (And p q) = eval s p && eval s q
eval s (Imply p q) = eval s p <= eval s q

vars :: Prop -> [Char]
vars (Const _) = []
vars (Var x ) = [x]
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) bss ++ map (True:) bss
	where bss = bools (n-1)
	
rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x:rmdups (filter (/=x) xs)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
	where vs = rmdups (vars p)
	
isSat :: Prop -> Subst
isSat p = check [(eval s p, s) | s <- substs p]
	where 
		check [] = []
		check (x:xs) = if fst x then snd x else check xs

--15
isCantorPair :: Int -> Bool
isCantorPair z = if xx+yx==yz then True else False
		where
			roots :: Int -> (Int, Int)
			roots z = ((w-y), (z-t))
				where 
					w = floor((sqrt (fromIntegral (8*z)+1)-1)/2)
					t = (w*w+w) `div` 2
					y = z - t
			xz = fst (roots z)
			yz = snd (roots z)
			xx = fst (roots xz)
			yx = snd (roots xz)