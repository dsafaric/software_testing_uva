module Bonus

where
import Lab1Bonus

-- Exercise 1
length' :: [a] -> Int
length' xs = foldr (\x -> (+1)) 0 xs

elem' :: Eq a => a -> [a] -> Bool
elem' x xs = foldr (\e -> (||) (e ==x)) False xs

or' :: [Bool] -> Bool
or' xs = foldr (||) False xs

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x -> (:) (f x)) [] xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' f = foldr (\x xs -> if f x then x : xs else xs) []

--  +-+ is our version of ++
(+-+) :: [a] -> [a] -> [a]
p +-+ q = foldr (:) q p

-- b is added initially to the empty list
-- Thereafter the top of the new list is added to that
-- So creating a list with the first element after the second one
-- By doing this to the whole list it is reversed
-- Had to admit this took some help from the internet
reverse' :: [a] -> [a]
reverse' xs = foldr (\b g x -> g (b : x)) id xs []

-- Exercise 2
reverse'' :: [a] -> [a]
reverse'' xs = foldl (flip (:)) [] xs

-- Exercise 3
-- foldr can work on infinite lists while foldl cannot. foldl are 'nested' on the outside so before being able to calculate anything the whole list
-- should be expanded, which is impossible with infinite lists. In the definition of foldr you can see that f x is calculated before going on with
-- foldr f xs while in the definition of foldl it is clear that foldl f (f x) xs makes sure the list is expanded completely before returning an answer.
-- For example take 10 $ foldr (:) [] [1..] works while the foldl equivalent does not.
-- Resource: stackoverflow/questions/7396978/left-and-right-folding-over-an-infinite-list

-- Exercise 4  
-- Will give the answer (Tiger, Lady)
sign3, sign4 :: (Creature, Creature) -> Bool
sign3 (x,y) = any (==Lady) [x,y]
sign4 (x,y) = x == Tiger && y == Lady

solution2 :: [(Creature,Creature)]
solution2 = 
 [ (x,y) | x <- [Lady,Tiger], 
           y <- [Lady,Tiger], 
           sign3 (x,y) == sign4 (x,y), x /= y]
           
--Exercise 5
-- Will give the answer (Knave, Knight)
john2, bill :: (Islander, Islander) -> Bool
john2 (x,y) = x == y
bill (x,y) = x /= y

solution4 :: [(Islander,Islander)]
solution4 = [(x,y) | x <- [Knight, Knave], 
                     y <- [Knight, Knave], 
                     if john(x,y) then x == Knight else x == Knave, -- Only if john is right is he a knight
                     if bill(x,y) then y == Knight else y == Knave] -- The same for bill

-- Exercise 6
-- Our own puzzle: John says that bill is a knave and bill says that neither are knaves
-- Will give the answer (Knave, Knight)
john3, bill2 :: (Islander, Islander) -> Bool
john3 (x,y) = y == Knave
bill2 (x,y) = x /= Knave && y /= Knave

solution5 :: [(Islander,Islander)]
solution5 = [(x,y) | x <- [Knight, Knave], 
                     y <- [Knight, Knave], 
                     if john3(x,y) then x == Knight else x == Knave, -- Only if john is right is he a knight
                     if bill2(x,y) then y == Knight else y == Knave] -- The same for bill
                     
-- Exercise 7
-- guilty gives the list of possible guilty boys according to the given boy
guilty :: (Boy -> Bool) -> [Boy]
guilty x = filter x boys

-- guiltyAccordingEach tells us for each boy who is a possible guilty one according to that boy
guiltyAccordingEach :: [[Boy]]
guiltyAccordingEach = map guilty declarations 

-- check will check for the given Boy if it is in the list of guilty ones
check :: Boy -> [Bool]
check x = map (\xs -> any (==x) xs) guiltyAccordingEach

-- blame will get the length for the boy for how many times it is in a list of guilty ones
blame :: Boy -> Int
blame x = length $ filter (== True) (check x)

-- Since three boys are telling the truth, the same name of a possible guilty boy should be in at least three lists for it to be true
solution :: [Boy]
solution = map (\(x,y) -> y) $ filter (\(x,y) -> x==3) $ zip (map blame boys) boys