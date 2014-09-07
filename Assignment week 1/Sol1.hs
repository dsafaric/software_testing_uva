module Sol1 where
import GS

-- Exercise 1.9
-- Maximum of list of one is that one element, otherwise recursively check the top element with 
-- rest of the list
maxInteger :: Ord a => [a] -> a
maxInteger [] = error "No maximum of empty list possible"
maxInteger [x] = x 
maxInteger (x:xs) = max x (maxInteger xs)

-- Exercise 1.10
-- Recursively check if the current top is the same as the element given
-- If so, remove it and return the rest, otherwise search further
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
-- Each letter in the string given is replicated one time more using an accumulator which increments every call 
blowup :: String -> String
blowup (x:xs) = blowupAcc 1 (x:xs)
    where
        blowupAcc _ [] = []
        blowupAcc n (x:xs) = replicate n x ++ blowupAcc (n+1) xs
        
-- Exercise 1.15
-- strString using a form of quicksort
-- strString is applied to all in elements in xs smaller or equal to x, which is concatenated with x and thereafter 
-- concatenated with strString applied to all elements in xs greater than x
strString :: [String] -> [String]
strString (x:xs) = strString [y | y <- xs, y <= x] ++ [x] ++ strString[y | y <- xs, y > x]

-- Exercise 1.17
-- Substring when xs is a prefix of (y:ys) or if xs is a prefix of the remainder string ys, otherwise it is not
substring :: String -> String -> Bool
substring xs (y:ys) | length xs > length (y:ys) = False
                    | prefix xs (y:ys) = True
                    | substring xs ys = True
                    | otherwise = False
    
-- Exercise 1.20
lengths :: [[a]] -> [Int]
lengths [[]] = []
lengths xs = map length xs

-- Exercise 1.21
-- Using lengths and foldr to create the definition of sum 
sumLengths :: [[a]] -> Int
sumLengths [[]] = 0
sumLenghts xs = foldr (+) 0 (map length xs)     
        