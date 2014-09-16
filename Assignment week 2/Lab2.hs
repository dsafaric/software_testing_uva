module Lab2 where 

import Data.List
import System.Random

---- Exercise 1 ----
data Shape = NoTriangle | Equilateral | Isosceles | Rectangular | Other
    deriving (Eq, Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle x y z  -- Negative or zero sides can not make a triangle
                | x <= 0 || y <= 0 || z <= 0    = NoTriangle
                
                -- When two sides are together shorter than the third side it is no triangle
                | x + y < z                     = NoTriangle
                | x + z < y                     = NoTriangle
                | y + z < x                     = NoTriangle
                
                -- All sides are equal
                | x == y && y == z              = Equilateral
                
                -- Two sides are equal
                | x == y || x == z || y == z    = Isosceles
                
                -- Pythagorean theorem
                | x*x + y*y == z*z              = Rectangular
                | y*y + z*z == x*x              = Rectangular
                | x*x + z*z == y*y              = Rectangular
                
                | otherwise                     = Other
                
--- Triangle tests ---
triangles1, triangles2, triangles3, triangles4 :: [Shape]
testTriangles1, testTriangles2, testTriangles3, testTriangles4 :: Bool

-- Should give exactly two Isosceles (x == 10 || x == 5) & five noTriangles since up till [x + 5 < 10 | x <- [0..4]
triangles1 = [triangle 10 5 x | x <- [0..15]]
testTriangles1 = let list = triangles1 
                 in length (filter (== Isosceles) list) == 2 &&
                    length (filter (== NoTriangle) list) == 5
                    
-- triangles2 should have one rectangular (x == 5, 3*3 + 4*4 = 5*5), two Isosceles (x == 3 || x == 4) and no NoTriangle since the range should make it 
-- impossible to get one of the first cases defined in triangle
triangles2 = [triangle 3 4 x | x <- [1..7]]
testTriangles2 = let list = triangles2
                 in length (filter (== Isosceles) list) == 2 &&
                    length (filter (== NoTriangle) list) == 0 && 
                    length (filter (== Rectangular) list) == 1
                    
-- triangles3 gives 1000 results (10 * 10 * 10), only 10 of those should be Equilateral, since in only 10 of those cases x == y == z
triangles3 = [triangle x y z | x <- [1..10], y <- [1..10], z <- [1..10]]
testTriangles3 = let list = triangles3
                 in length (filter (== Equilateral) list) == 10
                
-- triangles4 should not contain NoTriangle since x, y and z are all greater than 0 and the respectively two out of x, y, z is greater or equal to the other
triangles4 = [triangle x y z | x <- [1..20], y <- [1..20], z <- [1..20], x + y >= z, x + z >= y, y + z >= x]
testTriangles4 = let list = triangles4
                 in length (filter (== NoTriangle) list) == 0
                   
                
---- Exercise 2 ----
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True                                                  -- base case
isPermutation [] _ = False                                                  -- second list is longer
isPermutation _ [] = False                                                  -- first list is longer
isPermutation (x:xs) ys | elem x ys = isPermutation xs (delete x ys)        -- recursively check if each element of the first list is in the second
                        | otherwise = False

---- Exercise 3 ----
-- Two lists which are permutations should have the same length
testProp1, testProp2 :: Eq a=> [a] -> [a] -> Bool
testProp1 xs ys = length xs == length ys

-- Every element of one list should be in the other list
testProp2 [] [] = True
testProp2 (x:xs) ys = elem x ys && testProp2 xs ys

-- Two lists which are permutations of each other should be the same after ordering
testProp3 :: Ord a => [a] -> [a] -> Bool
testProp3 xs ys = sort xs == sort ys

-- Two list which are permutations of each other should have the same sum when the elements are added to each other (in the case of a list of Num)
testProp4 :: (Num a, Eq a) => [a] -> [a] -> Bool
testProp4 xs ys = sum xs == sum ys

---- Exercise 4 ----
-- Sub-function inBetween to put an element between all other elements in a list and return all new possible lists.
-- E.g. for 1 [2,3] it will return [1,2,3], [2,1,3] and [2,3,1]
-- Use this sub-function to insert the top of the current list in all permutations of the rest of the list and do this also for the remainder of the list
perms :: Eq a => [a] -> [[a]]
perms [ ] = [[]]
perms (x : xs) = concat (map (inBetween x) (perms xs))
    where
        inBetween e [ ] = [[e]]                                             
        inBetween e (y:ys) = (e:y:ys) :[ y:res | res <- inBetween e ys]

---- Exercise 5 ----
-- Check if two lists are permutations of each other and for each element at index i if it is not the same as the other list at index i
isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement xs ys = isPermutation xs ys && notSame xs ys
    where 
        notSame [] [] = True
        notSame (x:xs) (y:ys) | x /= y = notSame xs ys
                              | otherwise = False

---- Exercise 6 ----
-- Create all permutations of the list [0..x-1] and filter out the derangements
deran :: Int -> [[Int]]
deran 0 = []
deran x = filter (isDerangement [0..(x -1)]) (perms [0..(x - 1)]) 

---- Exercise 7 ----
-- The same testable properties can be used as for isPermutation
testPropsD :: (Num a, Ord a) => [a] -> [a] -> Bool
testPropsD xs ys = testProp1 xs ys &&
                   testProp2 xs ys &&
                   testProp3 xs ys &&
                   testProp4 xs ys

-- Next to that not one element may be on the same spot in the other list
testProp5 [] [] = True
testProp5 (x:xs) (y:ys) | x /= y = testProp5 xs ys
                         | otherwise = False

---- Exercise 8 ----
-- Function modified after taken from http://rosettacode.org/wiki/Pick_random_element#Haskell
-- It works by wrapping the pure function xs!! into a IO one and filling the blank spot by a random integer so it picks a random element
pick :: [[Int]] -> IO [Int]
pick xs = randomRIO (0, length xs - 1) >>= return . (xs !!)

-- Create a random derangement by computing all derangements from a list and thereafter choosing one at random
randomDerangement :: [Int] -> IO [Int]
randomDerangement xs = do
                        let list = filter (isDerangement xs) (perms xs)
                        element <- pick list
                        return element
                        
-- testDerangement expects a list makes a derangement from it and thereafter checks if
-- the testable properties are met                        
testDerangement :: [Int] -> IO Bool
testDerangement xs = do 
                  list <- randomDerangement [1..5] 
                  let b1 = testPropsD xs list &&
                           testProp5 xs list        
                  return b1
                  
---- Exercise 9 ----
-- The amount of derangements can be calculated using this function: !n = (n - 1) * (!(n-1) + !(n-2))
-- This is implemented in dLength with base cases length 0, 1 and 2
dLength :: Int -> Int
dLength 0 = 0
dLength 1 = 0
dLength 2 = 1
dLength n = (n-1) * (dLength (n-1) + dLength (n-2))

-- The test function given by the assignment
dlTest :: Int -> [Int]
dlTest x = [length $ deran n | n <- [0..x]]

-- Check if they are the same for list of length x
test x = (map dLength [0..x]) == (dlTest x)