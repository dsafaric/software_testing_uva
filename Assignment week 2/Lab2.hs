module Lab2 where 

import Data.List
import System.Random

-- Exercise 1
data Shape = NoTriangle | Equilateral | Isosceles | Rectangular | Other
    deriving (Eq, Show)

-- 5 minutes
triangle :: Integer -> Integer -> Integer -> Shape
triangle x y z  | x <= 0 || y <= 0 || z <= 0    = NoTriangle
                | x == y && y == z              = Equilateral
                | x == y || x == z || y == z    = Isosceles
                | x*x + y*y == z*z              = Rectangular
                | y*y + z*z == x*x              = Rectangular
                | x*x + z*z == y*y              = Rectangular
                | otherwise                     = Other
                
-- Exercise 2
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation [] _ = False
isPermutation _ [] = False
isPermutation (x:xs) ys | elem x ys = isPermutation xs (delete x ys)
                        | otherwise = False

-- Exercise 3
-- Two lists which are permutations should have the same length
testProp1 xs ys = length xs == length ys

-- Two lists which are permutations of each other should be the same after ordering
testProp2 xs ys = sort xs == sort ys

-- Every element of one list should be in the other list
testProp3 (x:xs) ys = elem x ys && testProp3 xs ys

-- Exercise 4


-- Exercise 4

-- Exercise 5

-- Exercise 6

-- Exercise 7

-- Exercise 8

-- Exercise 9
