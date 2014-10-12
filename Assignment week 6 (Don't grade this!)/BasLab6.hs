module BasLab6

where
import Data.List
import System.Random
import Week6
import Data.Time
import Test.QuickCheck
import System.IO.Unsafe
import Control.Monad
import Data.Bits

---- Assignment 1 ----
-- Time spent :: 45 minutes
---- We included 4 different versions, since we did this assignment separated. modExp is fastest, so you can only look at that one if you want to.
exM' :: Integer -> Integer -> Integer -> Integer
exM' x e n = let list = (findPowerOf2 e)
             in (product $ map (\exp -> exMsub x exp n) list) `mod` n

-- Double the accumulator until it is greater than the exponent, square the remainder and mod it again with n
exMsub :: Integer -> Integer -> Integer -> Integer
exMsub x y n = exM2 x y n 1 (mod x n)
    where
        exM2 x y n i r  | (i*2<=y) = exM2 x y n (i*2) (mod (r^2) n)
                        | otherwise = r

binary :: Integer -> [Integer]
binary 0 = []
binary n | even n = (rem n 2) : (binary (div n 2))
         | odd n = (rem n 2) : (binary (div (n-1) 2))
         
findPowerOf2 :: Integer -> [Integer]
findPowerOf2 n = let list = zip [0..] (binary n)
                 in map (\z -> 2^z) $ map fst $ filter (\(x,y) -> y == 1) list     

-- Another way of doing it:
modExp :: Integer -> Integer -> Integer -> Integer
modExp b 0 m = 1
modExp b e m = t * modExp ((b * b) `mod` m) (shiftR e 1) m `mod` m -- shiftR by 1 == div by 2!
  		   where 
  		   	t = if testBit e 0 
  		   		then b `mod` m else 1 -- is first bit equal to 1? mod!.. nope? number is 0. return 1!

-- similar like above.. more clear... divide problem to smaller ones. log << smaller/shorter
-- x^n % m, time complexity O(log n)
modPow :: Integer -> Integer -> Integer -> Integer
modPow x n m
	| n == 0 = 1
	| n `mod` 2 == 0 = (nOdd*nOdd) `mod` m
	| otherwise = nEvn
	where 
		nOdd = modPow x (n `div` 2) m
		nEvn = ((x `mod` m) * (modPow x (n-1) m)) `mod` m    

-- And a third way:
exMod :: Integer -> Integer -> Integer -> Integer
exMod _ 0 _ = 1     
exMod x y m | even y = multM e e m 
            | otherwise = multM x (multM e e m) m
  	where  e  = exMod x (y `div` 2) m        

---- Assignment 2 ----
getRandomInt :: Integer -> Integer -> IO Integer
getRandomInt l u = getStdRandom (randomR (l,u))

randomValues :: Int -> IO [(Integer,Integer,Integer)]
randomValues 0 = return [] 
randomValues n = do
       x <- getRandomInt m4 m7
       y <- getRandomInt m4 m7
       z <- getRandomInt m4 m7
       xs <- randomValues (n-1)
       return ((x,y,z):xs)
               
    
-- Compares the old and the new exM functions based on performance time
compareExMs :: IO()    
compareExMs = do	
        list <- randomValues 1000
        a <- runningTime $ exM'' 1 list
        b <- runningTime $ exM'' 2 list
        c <- runningTime $ exM'' 3 list
        d <- runningTime $ exM'' 4 list
        e <- runningTime $ exM'' 5 list
        print $ "exM': " ++ show a
        print $ "modPow': " ++ show b
        print $ "modExp': " ++ show c
        print $ "exMod': " ++ show d
        print $ "exM: " ++ show e      
         
-- exM and exM' can only be compared if used in IO (otherwise it gives computing time 0.00)
exM'' :: Int -> [(Integer, Integer, Integer)] -> IO()
exM'' b list = do 
        let m = map (exMCompare b) list
        print $ last m -- Just so the whole screen is not full
        
           
-- Use the exM and exM' on a three tuple of values
exMCompare :: Int -> (Integer,Integer,Integer) -> Integer
exMCompare b (x,y,z) | b == 1 =  exM' x y z
                     | b == 2 = modPow x y z
                     | b == 3 = modExp x y z 
                     | b == 4 = exMod x y z
                     | b == 5 = exM x y z
                     | otherwise = error "wrong enum"

-- Get the running time of function f
runningTime :: IO a -> IO Double
runningTime f = do
	s <- getCurrentTime
	f
	e <- getCurrentTime
	return $ realToFrac $ diffUTCTime e s :: (IO Double)
    

qTestProp' :: (Integer -> Integer -> Integer -> Integer) -> Integer -> Integer -> Integer -> Bool
qTestProp' f x y m	| x == 0 || y == 0 || m == 0 	= True                   -- No valid input
                    | otherwise = f (x^2) (y^2) (m^2) == exM (x^2) (y^2) (m^2)

-- Check for each of they have the same values as the known correct exM
qTestProp1 = quickCheckWith stdArgs {maxSuccess = 5000} (qTestProp' exM')  
qTestProp2 = quickCheckWith stdArgs {maxSuccess = 5000} (qTestProp' modExp) 
qTestProp3 = quickCheckWith stdArgs {maxSuccess = 5000} (qTestProp' modPow) 
qTestProp4 = quickCheckWith stdArgs {maxSuccess = 5000} (qTestProp' exMod) 


---- Assignment 3 ----
-- Since there is no fast way of finding all primes, there is none for finding al composites
-- Just use the same mechanism
composites :: [Integer]
composites = filter (\n -> isPrime n == False) [1..]

---- Assignment 4 ----
testFunction :: Int -> [Integer] -> IO Integer
testFunction k (x:xs) 	= do
    fermatPrime <- primeF k x
    if fermatPrime 
    	then do 
    		return x
      else 
      	testFunction k xs
                    
-- Shows the lowest found falsely labelled prime after 10 times doing the test and a given k (times primeF is applied)
-- When k is increasing the minimum number found as well since testing is stronger
lowestComposite :: Int -> IO Integer
lowestComposite k = testComposites k 10 >>= return.minimum

testComposites :: Int -> Int -> IO [Integer]
testComposites _ 0 = return []
testComposites k n = do
        x <- testFunction k composites 
        xs <- testComposites k (n-1)
        return (x:xs)

-- Example tests
t1 = lowestComposite 1      -- 4
t2 = lowestComposite 4      -- 15
t3 = lowestComposite 7      -- 1105
t4 = lowestComposite 10     -- 2821

---- Assignment 5 ----
carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
      k <- [2..], 
      isPrime (6*k+1), 
      isPrime (12*k+1), 
      isPrime (18*k+1) ]

testPrimeFCarmichael :: (Int -> Integer -> IO Bool) -> Integer -> Int -> [Integer] -> Int -> IO () 
testPrimeFCarmichael f n k [] a 		= print ("Tests failed: " ++ show n ++ " out of " ++ show a)
testPrimeFCarmichael f n k (x:xs) a 	= do
    fermatPrime <- f k x
    if fermatPrime 
    	then do 
    		print ("Falsely labelled prime: " ++ show x)
      		testPrimeFCarmichael f (n+1) k xs a
      else 
      	testPrimeFCarmichael f n k xs a
        
testCarmichael :: Int -> IO()
testCarmichael k = testPrimeFCarmichael primeF 0 k (take 100 carmichael) 100

-- Example tests
t5 = testCarmichael 1       -- Failed 100/100
t6 = testCarmichael 10      -- Failed 100/100
t7 = testCarmichael 30      -- Failed 98 / 100
t8 = testCarmichael 100     -- Failed 93 / 100   

---- Assignment 6 ----
testMillerRabin :: Int -> IO()
testMillerRabin k = testPrimeFCarmichael primeMR 0 k (take 100 carmichael) 100

-- Example tests
t9  = testMillerRabin 1      -- Failed 13 / 100
t10 = testMillerRabin 2      -- Failed 1 / 100
t11 = testMillerRabin 3      -- Failed 0 / 100

---- Assignment 7 ----
mersenne :: Int -> Int -> [Integer] -> IO ()
mersenne _ _ [] = print ("List of primes empty")
mersenne c k (x:xs) = do
	n <- primeMR k (2^x - 1)
	if n 
		then do
			print ("Marsenne's " ++ show c ++ " number: " ++ show (2^x - 1))
			mersenne (c+1) k xs
		else mersenne c k xs

testMersenne :: Int -> IO ()
testMersenne k = mersenne 0 k $ take 1000 primes

-- Example tests
t12 = testMersenne 1
t13 = testMersenne 2
t14 = testMersenne 5

---- Assignment BONUS ----

