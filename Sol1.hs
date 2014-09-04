module Sol1 where
import GS

-- Exercise 1.1
calculations x = (x + x) * x - (x / x)

-- Exercise 1.2 & 1.3
-- Can not find a actual question..

-- Exercise 1.9
maxInteger :: Ord a => [a] -> a
maxInteger [] = error "No maximum of empty list possible"
maxInteger (x:xs) = max x (maxInteger xs)

-- Exercise 1.10
removeFst :: Eq a => a -> [a] -> [a]
removeFst _ [] = []
removeFst m (x:xs) | x == m = xs
                   | x /= m = x: removeFst m xs
                     
-- Exercise 1.13
count :: Char -> String -> Int
count c [] = 0
count c (x:xs) | c == x = 1 + count c xs
               | otherwise = count c xs
               
-- Exercise 1.14
blowup :: String -> String
blowup (x:xs) = blowupAcc 1 (x:xs)
    where
        blowupAcc _ [] = []
        blowupAcc n (x:xs) = replicate n x ++ blowupAcc (n+1) xs
        
-- Exercise 1.15
-- strString using a form of quicksort
-- strString is applied to all in xs smaller or equal to x concatenated with x and thereafter concatenated with strString applied to all xs greater than x
strString :: [String] -> [String]
strString (x:xs) = strString [y | y <- xs, y <= x] ++ [x] ++ strString[y | y <- xs, y > x]

-- Exercise 1.17
substring :: String -> String -> Bool
substring xs (y:ys) | length xs > length (y:ys) = False
                    | prefix xs (y:ys) = True
                    | substring xs ys = True
                    | otherwise = False
    
-- Exercise 1.18 & 1.19

-- Exercise 1.20
lengths :: [[a]] -> [Int]
lengths [[]] = []
lengths xs = map length xs

-- Exercise 1.21
--sumLengths :: [[a]] -> Int
sumLengths [[]] = 0
sumLenghts xs = foldr (+) 0 (map length xs) 

-- Exercise 1.24
-- It expexcts another parameter


        
        
