module Sol2

where

import TAMO
import GS

-- Exercise 2.13
theorem1 :: Bool
theorem1 = True == not False && False == not True -- Always true

theorem2, theorem3, theorem3b, theorem4, theorem4b, theorem5, theorem6 :: Bool -> Bool 
theorem2 p = (p <= False) == not p -- Always true

theorem3 p  = p ||True -- Always true
theorem3b p = p && False -- Always false

theorem4 p = p || False -- p
theorem4b p = p && True   -- p

theorem5 p = p || not p -- Always true
theorem6 p = p && not p -- Always false

-- Exercise 2.15
contradiction1 :: (Bool -> Bool) -> Bool
contradiction1 f | f True == False = True
                 | f False == False = True
                 | otherwise = False
                  
-- Use list comprehensions to assign both boolean values to the variables p, q and redundant
-- If all of them fail, contradiction2 (& contradiction3) succeed
contradiction2 :: (Bool -> Bool -> Bool) -> Bool
contradiction2 f = and [not (f p q) | p <- [True, False], q <- [True, False]]

contradiction3 :: (Bool -> Bool -> Bool -> Bool) -> Bool
contradiction3 f = and [not (f p q r) | p <- [True, False], q <- [True, False], r <- [True, False]]            

-- Exercise 2.51
unique :: (a -> Bool) -> [a] -> Bool
unique _ [] = False
unique f xs = length (filter f xs) == 1

-- Exercise 2.52
parity :: [Bool] -> Bool
parity xs = even $ length $ filter (==True) xs 

-- Exercise 2.53 
evenNR :: (a -> Bool) -> [a] -> Bool
evenNR _ [] = True
evenNR f xs = parity (map f xs)
