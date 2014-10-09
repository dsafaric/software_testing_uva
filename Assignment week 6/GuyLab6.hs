module Lab6

where
import Data.List
import System.Random
import Week6
import Data.Bits

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
      k <- [2..], 
      isPrime (6*k+1), 
      isPrime (12*k+1), 
      isPrime (18*k+1) ]

--exM :: Integer -> Integer -> Integer -> Integer
--exM x y m = x

maxBaseTwo m n -- 40 0
	| (2^n) <= m = maxBaseTwo m (n+1) -- 2^0 < 40, 2^1 < 40, 2^3 < 40, 2^4 < 40, 2^5(32) < 40
	| otherwise = 2^(n-1) -- 2^(6-1) = 32


modExp :: Integer -> Integer -> Integer -> Integer
modExp b 0 m = 1
modExp b e m = t * modExp ((b * b) `mod` m) (shiftR e 1) m `mod` m
  		   where t = if testBit e 0 then b `mod` m else 1

-- x^n % m
modPow :: Integer -> Integer -> Integer -> Integer
modPow x n m
	| n == 0 = 1
	| n `mod` 2 == 0 = (nOdd*nOdd) `mod` m
	| otherwise = nEvn
	where 
		nOdd = modPow x (n `div` 2) m
		nEvn = ((x `mod` m) * (modPow x (n-1) m)) `mod` m


-- :set +s