module CrossSudoku

where

import SS
import System.Random
import Control.Monad
import Data.List

------- Extra assignment 2 of the slides -------
-- Crossed Sudoku
-- Extra constraints: Diagonals need to be unique too

crossChecks :: ConstraintsFunctions
crossChecks = inDiagonal : normalSChecks 

crossfreeFunctions = freeInDiagonal1 : freeInDiagonal2: normalfreeFunctions
crossConstraints = crossConstraint : normalSConstraints
crossConstraint s = and $ [diagonalInjective True s (r,c) | r <- positions, c <- positions] ++ 
                          [diagonalInjective False s (r,c) | r <- positions, c <- positions]

-- Values in the diagonals
diagonal1 s (r,c) = [ s (r',c') | c' <- positions, r' <- positions, c' == r' ]
diagonal2 s (r,c) = [ s (r',c') | c' <- positions, r' <- positions, c' + r' == 10 ]

freeInDiagonal1 s (r,c) = freeInSeq $ diagonal1 s (r,c)
freeInDiagonal2 s (r,c) = freeInSeq $ diagonal2 s (r,c)

freeAtPos3 :: Sudoku -> (Row,Column) -> [Value]
freeAtPos3 s (r,c) = freeAtPos' s (r,c) crossfreeFunctions
   
diagonalInjective :: Bool -> Sudoku -> (Row,Column) -> Bool
diagonalInjective b s (r,c) = injective $ vs b
 where 
   vs True = filter (/= 0) (diagonal1 s (r,c))
   vs False = filter (/= 0) (diagonal2 s (r,c))
   
consistent3 s  = consistent' s crossConstraints
    
    
extendNode3 :: Node -> Constraint -> [Node]
extendNode3 = extendNode' crossChecks
         
prune3 :: (Row,Column,Value) -> [Constraint] -> [Constraint]
prune3 = prune' crossChecks crossChecks
  
inDiagonal (r,c) (x,y) = (x + y == 10 && r + c == 10) ||
                         (x == y && r == c)
                         
initNode3 :: Grid -> [Node]
initNode3 gr = initNode' gr crossConstraints [freeAtPos3]
               
solveNs3 :: [Node] -> [Node]
solveNs3 = search succNode3 solved 

succNode3 :: Node -> [Node]
succNode3 = succNode' crossChecks

-- Generating
minimalize3 :: Node -> [(Row,Column)] -> Node
minimalize3 n [] = n
minimalize3 n ((r,c):rcs) 
   | uniqueSol3 n' = minimalize3 n' rcs
   | otherwise     = minimalize3 n  rcs
  where n' = eraseN n (r,c)
  
uniqueSol3 :: Node -> Bool
uniqueSol3 node = singleton (solveNs3 [node]) where 
  singleton [] = False
  singleton [x] = True
  singleton (x:y:zs) = False
  
genProblemCross :: Node -> IO Node
genProblemCross n = do ys <- randomize xs
                       return (minimalize3 n ys)
   where xs = filledPositions (fst n)
                      
genRandomSudoku3 :: IO Node
genRandomSudoku3 = do [r] <- rsolveNs3 [emptyN]
                      return r
                      
rsuccNode3 :: Node -> IO [Node]
rsuccNode3 (s,cs) = 
  do xs <- getRandomCnstr cs
     if null xs 
        then return []
        else return (extendNode3 (s,cs\\xs) (head xs))

rsolveNs3 :: [Node] -> IO [Node]
rsolveNs3 ns = rsearch rsuccNode3 solved (return ns)  

genRandomProblemCross f = genRandomSudoku3 >>= \r -> genProblemCross r >>= \s -> f s
-- This takes a loooooooong time, takes even longer if we try to figure out for sure if it has one solution (uncomment lines below)
test2 :: IO()
test2 = do 
       x <- genRandomProblemCross return
       let s = uniqueSol3 x
       if s then showNode x else error "No minimal Cross Sudoku"