module Bonus

where

-- Exercise 1
length' :: [a] -> Int
length' xs = foldr (\x -> (+1)) 0 xs

--elem' :: a -> [a] -> Bool
--elem' x xs = foldr (\e -> e == x False xs

or' :: [Bool] -> Bool
or' xs = foldr (||) False xs

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x -> (:) (f x)) [] xs

filter' :: (a -> Bool) -> [a] -> [a]
--filter' f xs = foldr (\x -> (:) (f x)) [] xs
