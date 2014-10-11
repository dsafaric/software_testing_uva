-- Lab4Hs.hs

module Lab6Hs

where

import Data.Time
import Data.List
import System.Random
import Test.QuickCheck
import System.IO.Unsafe
import Control.Monad
import Test.Hspec
import Week6
import Lab6

-- write a function that does modular exponentian of x^y in polynomial time, by repeadetly squering
	-- module M

-- convert a Int to binary list, from Int to [Int]
-- by following the rules of modular exponentation, the third step should include
	-- a function for modular multiplication, which is in our case predifined in
	-- Week6.hs

exMod :: Integer -> Integer -> Integer -> Integer
exMod _ 0 _ = 1     
exMod x y m | even y = multM e e m 
            | otherwise = multM x (multM e e m) m
  	where  e  = exMod x (y `div` 2) m

-- Exercise 2

randInt :: Int -> Int -> IO Int
randInt = \x -> \ y -> getStdRandom (randomR (x,y)) 

expM' = hspec $ do
	describe "---- expM Function ----" $ do
		it "ExpM function" $ do
			expM m10 m20 19 `shouldBe` (9 :: Integer)

exMod' = hspec $ do
	describe "---- exMod Function ----" $ do
		it "exMod function" $ do
			exMod m10 m20 19 `shouldBe` (9 :: Integer)

-- expM does not perform great when m6 or higher are applied

testProp = 	if e < n then "expM has performed faster -> " ++ show e
			else "exMod has performed faster -> " ++ show n
		where 
			e = unsafePerformIO $ testProp' expM'
			n = unsafePerformIO $ testProp' exMod'

testProp' f = do
	s <- getCurrentTime
	f
	e <- getCurrentTime
	return $ realToFrac $ diffUTCTime e s :: (IO Double)

-- by using QuickCheck

qTestProp' :: (Ord a, Num a2, Num a1, Num a, Eq a2, Eq a1) =>
     (a1 -> a2 -> a -> a) -> a1 -> a2 -> a -> Bool
qTestProp' f x y m	| x == 0 || y == 0 || m == 0 	= True
					| otherwise 					= f (x^2) (y^2) (m^2) < (m^2)

qTestProp1 = quickCheckWith stdArgs {maxSuccess = 5000} (qTestProp' exMod)  
qTestProp2 = quickCheckWith stdArgs {maxSuccess = 5000} (qTestProp' expM)

-- Exercise 3
-- time spent: 45 min
composite :: [Integer]
composite = sieve' [4..]

sieve' :: [Integer] -> [Integer]
sieve' ns = (filter (\ m -> isPrime m == False) ns)

-- Exercise 4
-- time spent: 45 min

testFermat' :: Int -> [Integer] -> IO Integer
testFermat' k (x:xs) = do
    p <- primeF k x
    if p then testFermat' k xs
      else return x

testFermat :: Int -> IO [Char]
testFermat k = do
	c <- testFermat' k composite
	return $ "Failed at composite: " ++ show c 

testPropF = do
	r <- getStdRandom (randomR (1,10))
	testFermat r

-- Exercise 5
-- time spent: min
