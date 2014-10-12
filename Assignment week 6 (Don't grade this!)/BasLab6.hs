module Lab6

where
import Data.List
import System.Random
import Week6
import System.TimeIt

---- Assignment 1 ----
-- Time spent :: 45 minutes
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
         
findPowerOf2 n = let list = zip [0..] (binary n)
                 in map (\z -> 2^z) $ map fst $ filter (\(x,y) -> y == 1) list              

---- Assignment 2 ----
getRandomInt :: Integer -> Integer -> IO Integer
getRandomInt l u = getStdRandom (randomR (l,u))

randomValues :: Int -> IO [(Integer,Integer,Integer)]
randomValues 0 = return [] 
randomValues n = do
               x <- getRandomInt 1000000 100000000
               y <- getRandomInt 10000 1000000
               z <- getRandomInt 1 200
               xs <- randomValues (n-1)
               return ((x,y,z):xs)
               
applyRandom :: Bool -> IO Integer
applyRandom a = do 
                list <- randomValues 50
                let new = map (exM'' True) list
                let old = map (exM'' False) list
                if a then print new else print old

exM'' :: Bool -> (Integer,Integer,Integer) -> Integer
exM'' b (x,y,z) | b =  exM' x y z
                | otherwise = exM x y z

---- Assignment 3 ----
composites :: [Integer]
composites = undefined



---- Assignment 4 ----

---- Assignment 5 ----
carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
      k <- [2..], 
      isPrime (6*k+1), 
      isPrime (12*k+1), 
      isPrime (18*k+1) ]

---- Assignment 6 ----

---- Assignment 7 ----

---- Assignment BONUS ----

