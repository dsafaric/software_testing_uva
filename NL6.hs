module NL6

where
import Data.List
import Data.Binary
import System.Random
import Week6

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
      k <- [2..], 
      isPrime (6*k+1), 
      isPrime (12*k+1), 
      isPrime (18*k+1) ]

-- Exc 1  Modular Exponentiation

-- The main function, multiply all the remainder together, then mod the result	  
exM' :: Integer -> Integer -> Integer -> Integer
exM' x y n = mod (product (map (\exp -> exM1 x exp n) (expList y))) n

-- Calculate the case that exponents are 1, 2, 4, 8, 16, 32, 64 ....
exM1 x y n = exM2 x y n 1 (mod x n)
exM2 x y n i rem
	| (i*2<=y) = exM2 x y n (i*2) (mod (rem^2) n)
	| otherwise = rem

-- Get the list of exponents that decomposed from the given Exponent
-- For instance 117 convert to [64, 32, 16, 4, 1]	
expList num = binToExp (decToBin num)

-- Convert decimal to binary list is copied from http://snipplr.com/view/11807/
decToBin x = reverse $ decToBin' x
  where
    decToBin' 0 = []
    decToBin' y = let (a,b) = quotRem y 2 in [b] ++ decToBin' a
	
-- Convert binary list to exponent list
binToExp xs = binToExp' xs (length xs)
binToExp' [] _ = []
binToExp' (x:xs) n = if (x == 1) then 2^(n-1): binToExp' xs (n-1) else binToExp' xs (n-1)


-- Some test, using the tuple (x,y,z) to feed the value
 testList :: [(Integer,Integer,Integer)]
getNum = map (\(x,y,n) -> x^y) testList
testList = zipWith3 (\x y z -> (x,y,z)) [800..840] [5..25] [3..23]
test1 = map (\(x, y, n)-> (expM x y n) == (exM' x y n)) testList

{-
getRandomInt :: Int -> Int -> IO Int
getRandomInt min max = getStdRandom (randomR (min,max))
-}
--try = ((getRandomInt 1 10),(getRandomInt 11 20),(getRandomInt 91 100))
{-
getRndF 0 = do
	m <- getRandomInt 20 30
	return m
	-}
--ranInt = do m <- getRandomInt 3 return m
--rTest :: Int -> [a]
--rTest 0 = []
--rTest n = getRndF : rTest (n-1)
--((rInt 1 20), (rInt 1 20), (rInt 1 20)) -- : rTest (n-1)
--test1 = map (\ x y n-> (expM x y n) == (exM' x y n)) [()]	
	




