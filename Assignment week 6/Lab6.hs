module Lab6

where
import Data.List
import System.Random
import Week6
import Data.Time
import Test.QuickCheck
import Data.Bits
import Data.Char
import Data.Ord

---- Assignment 1 ----
-- Time spent :: 1 hour each probably
-- We included 4 different versions, since we did this assignment separated. modExp is (mostly, but it is very close) fastest, so you can only look at that one if you want to.
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
	| n `mod` 2 == 0 = (nOdd*nOdd) `mod` m
	| otherwise = nEvn
	where 
		nOdd = modPow x (n `div` 2) m
		nEvn = ((x `mod` m) * (modPow x (n-1) m)) `mod` m    

-- And a fourth way:
exMod :: Integer -> Integer -> Integer -> Integer
exMod _ 0 _ = 1     
exMod x y m | even y = multM e e m 
            | otherwise = multM x (multM e e m) m
  	where  e  = exMod x (y `div` 2) m        

---- Assignment 2 ----
-- Time spent: 2 hours of putting everything together and creating benchmark tests
getRandomInt :: Integer -> Integer -> IO Integer
getRandomInt l u = getStdRandom (randomR (l,u))

-- Random values bigger than m7 would give problems to the original exM which we are using as a comparison. When exM is excluded bigger numbers can be chosen.
randomValues :: Int -> IO [(Integer,Integer,Integer)]
randomValues 0 = return [] 
randomValues n = do
       x <- getRandomInt m5 m7
       y <- getRandomInt m5 m7
       z <- getRandomInt m5 m7
       xs <- randomValues (n-1)
       return ((x,y,z):xs)
               
    
-- Compares all the exM functions based on performance and print them in order of calculation time
compareExMs :: IO()    
compareExMs = do	
        list <- randomValues 500000
        a <- runningTime $ exM'' 1 list
        b <- runningTime $ exM'' 2 list
        c <- runningTime $ exM'' 3 list
        d <- runningTime $ exM'' 4 list
        e <- runningTime $ exM'' 5 list -- Toooo slow
        
        let list = zip ["expM :", "modPow :", "modExp': ", "exMod': ", "exM': "] [a,b,c,d,e] 
        let sortedList = sortBy (comparing snd) list
        let printE [] = return()
            printE (x:xs) = do putStr (fst x); print (snd  x); printE xs
        putStrLn "\nThe order from fastest to slowest + time: "
        printE sortedList     
         
-- the new functions and exM' can only be compared if used in IO (otherwise it gives computing time 0.00)
exM'' :: Int -> [(Integer, Integer, Integer)] -> IO()
exM'' b list = do 
        let m = map (exMCompare b) list
        print $ last m -- Just so the whole screen is not full, it has to do some computation, otherwise it will return 0.00
        
           
-- Use the exM and the new functions on a three tuple of values
exMCompare :: Int -> (Integer,Integer,Integer) -> Integer
exMCompare b (x,y,z) | b == 1 = expM x y z
                     | b == 2 = modPow x y z
                     | b == 3 = modExp x y z 
                     | b == 4 = exMod x y z
                     | b == 5 =  exM' x y z
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


---- Assignment 3 ----
-- Time spent : 15 minutes
-- Since there is no fast way of finding all primes, there is none for finding al composites
-- Just use the same mechanism
composites :: [Integer]
composites = filter (\n -> isPrime n == False) [4..]

---- Assignment 4 ----
-- Time spent: 30 minutes
-- General function which can be used to test primeF and primeMR
testPrimeFunction :: (Int -> Integer -> IO Bool) -> Integer -> Int -> [Integer] -> Int -> IO () 
testPrimeFunction f n k [] a 		= print ("Tests failed: " ++ show n ++ " out of " ++ show a)
testPrimeFunction f n k (x:xs) a 	= do
    prime <- f k x
    if prime
    	then do 
    		print ("Falsely labelled prime: " ++ show x)
      		testPrimeFunction f (n+1) k xs a
      else 
      	testPrimeFunction f n k xs a
        
-- Test prime F
testPrimeF :: Int -> IO()
testPrimeF k = testPrimeFunction primeF 0 k (take 1000 composites) 1000
        
        
-- Check the first false prime 
testFalsePrime :: Int -> [Integer] -> IO Integer
testFalsePrime k (x:xs) 	= do
    fermatPrime <- primeF k x
    if fermatPrime 
    	then do 
    		return x
      else 
      	testFalsePrime k xs
                    
-- Shows the lowest found falsely labelled prime after 10 times doing the test and a given k (times primeF is applied)
-- When k is increasing, the minimum number found will increase as well since testing is stronger
lowestComposite :: Int -> IO Integer
lowestComposite k = testComposites k 10 >>= return.minimum

testComposites :: Int -> Int -> IO [Integer]
testComposites _ 0 = return []
testComposites k n = do
        x <- testFalsePrime k composites 
        xs <- testComposites k (n-1)
        return (x:xs)

-- Example tests
t1 = lowestComposite 1      -- 4
t2 = lowestComposite 4      -- 15
t3 = lowestComposite 7      -- 1105
t4 = lowestComposite 10     -- 2821

---- Assignment 5 ----
-- Time spent : 10 minutes
carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
      k <- [2..], 
      isPrime (6*k+1), 
      isPrime (12*k+1), 
      isPrime (18*k+1) ]

        
testCarmichael :: Int -> IO()
testCarmichael k = testPrimeFunction primeF 0 k (take 100 carmichael) 100

-- Example tests
t5 = testCarmichael 1       -- Failed 100/100
t6 = testCarmichael 10      -- Failed 100/100
t7 = testCarmichael 30      -- Failed 98 / 100
t8 = testCarmichael 100     -- Failed 93 / 100   

---- Assignment 6 ----
-- Time spent: 15 minutes
testMillerRabin :: Int -> IO()
testMillerRabin k = testPrimeFunction primeMR 0 k (take 1000 carmichael) 1000

-- Example tests
t9  = testMillerRabin 1      -- Failed 13 / 100
t10 = testMillerRabin 2      -- Failed 1 / 100
t11 = testMillerRabin 3      -- Failed 0 / 100

---- Assignment 7 ----
-- Time spent: 45 minutes
-- The largest Mersenne's number discovered was the 19th: 2^4423 - 1
mersenne :: Int -> Int -> [Integer] -> IO ()
mersenne _ _ [] = print ("List of primes empty") -- Should of course never happen, but still, no harm done preparing
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
{- 
        Time spent: 2.5 hours
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
          
-- Get primes, public and private key, and encoded and decoded keys  with bit length n and number k in exM k e x 
type Key = (Integer, Integer)
          
createKeys :: Integer -> Integer -> (Key, Key)
createKeys p q = (rsa_public p q, rsa_private p q) 
         
-- Example of the RSA encryption where a message of 8 characters can be encoded (depends on prime length), longer and it will crash 
-- since integer will be to big and the translating will fail. To solve this, split the message in lengths of 8 and encode each 
-- separately to send longer messages (see encryptMessages).
encryptExample :: String -> IO()
encryptExample m = do 
        putStrLn $ "Message as created by B: " ++ m                                     
        let i = messageToInteger m
        putStrLn $ "Translates to integer: " ++ show i
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