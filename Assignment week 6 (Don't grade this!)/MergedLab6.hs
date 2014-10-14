-- MergedLab6.hs

module Lab6ST 

where

import Lab6
import Week6
import Data.Time
import Data.List
import Data.Bits
import Data.Char
import Test.Hspec
import System.Random
import Control.Monad
import Test.QuickCheck
import System.IO.Unsafe


---- Assignment 1 ----
-- Time spent :: 1 hour each probably
---- We included 4 different versions, since we did this assignment separated. modExp is fastest, so you can only look at that one if you want to.
modExp :: Integer -> Integer -> Integer -> Integer
modExp b 0 m = 1
modExp b e m = t * modExp ((b * b) `mod` m) (shiftR e 1) m `mod` m -- shiftR by 1 == div by 2!
  		   where 
  		   	t = if testBit e 0 
  		   		then b `mod` m else 1 -- is first bit equal to 1? mod!.. nope? number is 0. return 1!
                
-- Another way where the input is translated to a bit list and thereafter the method of the assignment is used
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


-- x^n % m, time complexity O(log n)
-- x^n mod m == (x^(n/2) mod m * x^(n/2) mod m) mod m. calculating x^(n/2) is a lot more faster then x^n. 
-- and also when it's even need to calculate x^(n/2) once.
modPow :: Integer -> Integer -> Integer -> Integer
modPow x n m
  | n == 0 = 1
  | n `mod` 2 == 0 = (nEvn*nEvn) `mod` m
  | otherwise = nOdd
  where 
    nEvn = modPow x (n `div` 2) m 
    nOdd = ((x `mod` m) * (modPow x (n-1) m)) `mod` m   

-- And a fourth way:
exMod :: Integer -> Integer -> Integer -> Integer
exMod _ 0 _ = 1     
exMod x y m | even y = multM e e m 
            | otherwise = multM x (multM e e m) m
  	where  e  = exMod x (y `div` 2) m        

---- Assignment 2 ----
getRandomInt :: Integer -> Integer -> IO Integer
getRandomInt l u = getStdRandom (randomR (l,u))

-- Random values bigger than m7 would give problems to the original exM which we are using as a comparison. When exM is excluded bigger numbers can be chosen.
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
         
-- the new functions and exM' can only be compared if used in IO (otherwise it gives computing time 0.00)
exM'' :: Int -> [(Integer, Integer, Integer)] -> IO()
exM'' b list = do 
        let m = map (exMCompare b) list
        print $ last m -- Just so the whole screen is not full, it has to do some computation, otherwise it will return 0.00
        
           
-- Use the exM and the new functions on a three tuple of values
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
    

-- Check if each of the new functions give the same result as the old exM
qTestProp' :: (Integer -> Integer -> Integer -> Integer) -> Integer -> Integer -> Integer -> Bool
qTestProp' f x y m	| x == 0 || y == 0 || m == 0 	= True                   -- No valid input
                    | otherwise = f (x^2) (y^2) (m^2) == exM (x^2) (y^2) (m^2)

-- Check for each of they have the same values as the known correct exM
qTestProp1 = quickCheckWith stdArgs {maxSuccess = 5000} (qTestProp' exM')  
qTestProp2 = quickCheckWith stdArgs {maxSuccess = 5000} (qTestProp' modExp) 
qTestProp3 = quickCheckWith stdArgs {maxSuccess = 5000} (qTestProp' modPow) 
qTestProp4 = quickCheckWith stdArgs {maxSuccess = 5000} (qTestProp' exMod) 


-- Exercise 3
-- Time spent: 10 min

composites :: [Integer]
composites = filter (\n -> isPrime n == False) [1..]


-- Exercise 4

primeF :: Int -> Integer -> IO Bool
primeF _ 2 = return True
primeF 0 _ = return True
primeF k n = do
   a <- randomRIO (1, n-2) :: IO Integer
   if (exM a (n-1) n /= 1) 	-- function modified and exM replaced with exMod
      then return False 
      else primeF (k-1) n

primeMR :: Int -> Integer -> IO Bool
primeMR _ 2 = return True
primeMR 0 _ = return True
primeMR k n = let 
   (r,s) = decomp (n-1) 
   f = \ x -> takeWhile (/= 1) 
       (map (\ j -> exMod x (2^j*s) n)  [0..r]) -- function modified and exM replaced with exMod
  in 
   do 
    a <- randomRIO (1, n-1) :: IO Integer
    if exMod a (n-1) n /= 1 
      then return False 
      else 
        if exMod a s n /= 1 && last (f a) /= (n-1) 
          then return False
          else primeMR (k-1) n

testPrimarity :: Integer -> (Int -> Integer -> IO Bool) -> Int -> [Integer] -> IO () 
testPrimarity n f k [] 		= print ("Test done -> Number of fails: " ++ show n)
testPrimarity n f k (x:xs) 	= do
    p <- f k x
    if p 
    	then do 
    		print ("Failed composite: " ++ show x)
      		testPrimarity (n+1) f k xs 
      else 
      	testPrimarity n f k xs
      		

testPropF :: IO ()
testPropF = do
	k <- getStdRandom (randomR (1,3))
	n <- getStdRandom (randomR (20,1000))
	testPrimarity 0 primeF k $ take n composites


-- Exercise 5
-- time spent: 10 min  

randInt :: Int -> Int -> IO Int
randInt = \x -> \ y -> getStdRandom (randomR (x,y)) 

testPropC :: IO ()
testPropC = do
	k <- randInt 1 3
	n <- randInt 1 10
	testPrimarity 0 primeF k $ take n carmichael 

-- Exercise 6
-- time spent: 30 min

testPropMR :: IO ()
testPropMR = do
	--k <- randInt 1 3
	--n <- randInt 20 30
	testPrimarity 0 primeMR 1 $ take 10 carmichael

-- getting a strange error regarding a bus error:
	-- either a pointer to deallocated 
	-- or overflow of the buffer

-- Exercise 7
-- time spent: 45 min 

mersenne' :: Int -> Int -> [Integer] -> IO ()
mersenne' _ _ [] = print ("List of primes empty")
mersenne' c k (x:xs) = do
	n <- primeMR k (2^x - 1)
	if n 
		then do
			print ("Marsenne's " ++ show c ++ " number: " ++ show (2^x - 1))
			mersenne' (c+1) k xs
		else mersenne' c k xs

testPropMM :: IO ()
testPropMM = do
	k <- randInt 1 3
	n <- randInt 10 1000
	mersenne' 0 k $ take n primes

-- the largest Mersenne's number discovered was the 19th: 2^4423 - 1

---- Assignment BONUS ----
{- 
        For RSA encryption the following steps are done:
        Step 1: Get primes with the same bit length 
        Step 2: RSA encryption
            Create a public and private key with it
        Step 3: Encode a message (String converted to Integer, since encoding expects an Integer)
        Step 4: Decode the message using the private key
        
        This way A can give B the public key, which B uses to encode a message. B sends the message which only A can decode
-}

-- Get random keys of bit length n + 1
getRandomPrime :: Int -> IO Integer
getRandomPrime n = do
                 n <- getRandomInt (2^n) (2^(n+1)-1)
                 returnNextPrime n
    where
        returnNextPrime n = do 
                            r <- primeMR 10 n
                            if r
                            then return n
                            else returnNextPrime (n+1)
          
-- Get primes, public and private key, and encoded and decoded keys  with bitlength n and number k in exM k e x 
type Key = (Integer, Integer)
          
createKeys :: Integer -> Integer -> (Key, Key)
createKeys p q = (rsa_public p q, rsa_private p q) 
         
-- Example of the RSA encryption where a message of 8 characters can be encoded (depends on prime lenght), longer and it will crash 
-- since integer will be to big and the translating will fail. To solve this, split the message in lengths of 8 and encode each 
-- separately to send longer messages (see encryptMessages).
encryptExample :: String -> IO()
encryptExample m = do 
        putStrLn $ "Message as created by B: " ++ m                                     
        let i = messageToInteger m
        p <- getRandomPrime 40
        q <- getRandomPrime 40
        putStrLn $ "Primes 1 & 2: " ++ show p ++ " " ++ show q
        let keys = createKeys p q
        putStrLn $ "Public key, given to B by A: " ++ show (fst keys)                    
        putStrLn $ "Private key, only obtained by A: " ++ show (snd keys)
        let encode =  rsa_encode (fst keys) i                           
        let decode = rsa_decode (snd keys) encode                       
        putStrLn $ "Encoding done by B and send encrypted message to A: " ++ show encode
        putStrLn $ "Decoding done by A after receiving message from B: " ++ show decode
        putStrLn $ "Translates to message: " ++ integerToMessage decode
      
-- Encrypt a arbitrary long message, increasing the key size means that we can also increase the size of the partial messages
-- Use the same key (not very safe, but easier) to encrypt parts of a message separately 
encryptMessages :: String -> IO()
encryptMessages m = do 
        let spl = splitEvery 10 m
        let i = map messageToInteger spl
        p <- getRandomPrime 50
        q <- getRandomPrime 50
        let keys = createKeys p q
        let encode =  map (rsa_encode (fst keys)) i
        print encode
        let decode = map (rsa_decode (snd keys)) encode
        print $ concatMap integerToMessage decode

        
-- Create an integer of a message 
messageToInteger :: String -> Integer
messageToInteger xs = read $ concat $ map evenIntlist intlist
    where
        intlist = map ord xs
        evenIntlist x | x < 100 = '0' : (show x)
                      | x < 1000 = show x

-- Split a list into pieces of each length n
splitEvery :: Int -> [a] -> [[a]]
splitEvery n x | length x > n = let s = splitAt n $ x in (fst s) : (splitEvery n (snd s))
               | otherwise = [x]
              
integerToMessage :: Integer -> String
integerToMessage i = map makeOrd $ list x
    where 
        x = show i
        makeOrd (l:ls) | l == '0' = chr (read ls)
                       | otherwise = chr $ read (l:ls)
        list x = splitEvery 3 $ make3 x
        make3 x | (length x) `rem` 3 /= 0 = '0' : x
                | otherwise = x




