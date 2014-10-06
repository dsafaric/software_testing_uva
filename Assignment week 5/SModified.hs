module SModified where 

import Data.List
import System.Random
import SS
import Control.Monad

------- Exercise 4 -------
-- Time spent: 45 minutes
-- Example grid from the assignment
exampleGridNRC :: Grid
exampleGridNRC = [[0,0,0,3,0,0,0,0,0],
                  [0,0,0,7,0,0,3,0,0],
                  [2,0,0,0,0,0,0,0,8],
                  [0,0,6,0,0,5,0,0,0],
                  [0,9,1,6,0,0,0,0,0],
                  [3,0,0,0,7,1,2,0,0],
                  [0,0,0,0,0,0,0,3,1],
                  [0,8,0,0,4,0,0,0,0],
                  [0,0,2,0,0,0,0,0,0]]

-- The extra blocks 
blocks2 :: [[Int]]
blocks2 = [[2..4],[6..8]]

-- Show the new grid
showRow' :: [Value] -> IO()
showRow' [a1,a2,a3,a4,a5,a6,a7,a8,a9] = 
 do  putChar '|'         ; putChar ' '
     putStr (showVal a1) ; putChar ' '
     putStr "  "
     putStr (showVal a2) ; putChar ' '
     putStr (showVal a3) ; putChar ' '
     putChar '|'         ; putChar ' '
     putStr (showVal a4) ; putChar ' '
     putStr " "
     putStr (showVal a5) ; putChar ' '
     putStr "  "
     putStr (showVal a6) ; 
     putChar '|'         ; putChar ' '
     putStr (showVal a7) ; putChar ' '
     putStr (showVal a8) ; putChar ' '
     putStr "  "
     putStr (showVal a9) ; putChar ' '
     putChar '|'         ; putChar '\n'
     
showRowBetween :: IO()
showRowBetween = do putStrLn "|   +-----|--+   +--|-----+   |"

showRow2 :: [Value] -> IO()
showRow2 [a1,a2,a3,a4,a5,a6,a7,a8,a9] = 
 do  putChar '|'         ; putChar ' '
     putStr (showVal a1) ; putChar ' '
     putChar '|'         ; putChar ' '
     putStr (showVal a2) ; putChar ' '
     putStr (showVal a3) ; putChar ' '
     putChar '|'         ; putChar ' '
     putStr (showVal a4) ; 
     putChar '|'         ; putChar ' '
     putStr (showVal a5) ; putChar ' '
     putChar '|'         ; putChar ' '
     putStr (showVal a6) ; 
     putChar '|'         ; putChar ' '
     putStr (showVal a7) ; putChar ' '
     putStr (showVal a8) ; putChar ' '
     putChar '|'         ; putChar ' '
     putStr (showVal a9) ; putChar ' '
     putChar '|'         ; putChar '\n'
     
showGrid2 :: Grid -> IO()
showGrid2 [as,bs,cs,ds,es,fs,gs,hs,is] =
 do putStrLn ("+---------+---------+---------+")
    showRow' as; showRowBetween; showRow2 bs; showRow2 cs
    putStrLn ("+---------+---------+---------+")
    showRow2 ds; showRowBetween; showRow' es; showRowBetween; showRow2 fs
    putStrLn ("+---------+---------+---------+")
    showRow2 gs; showRow2 hs; showRowBetween; showRow' is
    putStrLn ("+---------+---------+---------+")
     
-- Show the new Sudoku
showSudoku2 :: Sudoku -> IO()
showSudoku2 = showGrid2 . sud2grid

-- The extra blocks
bl2 :: Int -> [Int]
bl2 x = concat $ filter (elem x) blocks2

-- The extra sub grids
subGrid2 :: Sudoku -> (Row,Column) -> [Value]
subGrid2 s (r,c) = 
  [ s (r',c') | r' <- bl2 r, c' <- bl2 c ]

-- Check for free values in the extra sub grids, the extra constraints formalized
freeInSubgrid2 :: Sudoku -> (Row,Column) -> [Value]
freeInSubgrid2 s (r,c) = freeInSeq (subGrid2 s (r,c))  

-- The same as freeAtPos but intersect it with the extra sub grid
freeAtPos2 :: Sudoku -> (Row,Column) -> [Value]
freeAtPos2 s (r,c) = freeAtPos s (r,c) `intersect` (freeInSubgrid2 s (r,c))
   
-- New injective for the new sub grid
subgrid2Injective :: Sudoku -> (Row,Column) -> Bool
subgrid2Injective s (r,c) = injective vs where 
   vs = filter (/= 0) (subGrid2 s (r,c))
   
-- Consistent now iff the consistent first and with the sub grid
consistent2 :: Sudoku -> Bool
consistent2 s = consistent s && (and [ subgrid2Injective s (r,c) | r <- [2,6], c <- [2,6]])
                    
-- Also the same as before, only with another if case, namely for the the new sub grid
prune2 :: (Row,Column,Value) -> [Constraint] -> [Constraint]
prune2 _ [] = []
prune2 (r,c,v) ((x,y,zs):rest)
  | r == x = (x,y,zs\\[v]) : prune2 (r,c,v) rest
  | c == y = (x,y,zs\\[v]) : prune2 (r,c,v) rest
  | sameblock (r,c) (x,y)  = (x,y,zs\\[v]) : prune2 (r,c,v) rest
  | sameblock2 (r,c) (x,y) = (x,y,zs\\[v]) : prune2 (r,c,v) rest -- New
  | otherwise = (x,y,zs) : prune2 (r,c,v) rest
  
-- Check for values in the new subgrid 
sameblock2 :: (Row,Column) -> (Row,Column) -> Bool
sameblock2 (r,c) (x,y) = bl2 r == bl2 x && bl2 c == bl2 y 

-- All below is basically the same as before only with the new functions to solve NRC Sudoku s
initNode2 :: Grid -> [Node]
initNode2 gr = let s = grid2sud gr in 
               if (not . consistent2) s then [] 
               else [(s, constraints2 s)]

constraints2 :: Sudoku -> [Constraint] 
constraints2 s = sortBy length3rd 
    [(r,c, freeAtPos2 s (r,c)) | 
                       (r,c) <- openPositions s ]
                       
extendNode2 :: Node -> Constraint -> [Node]
extendNode2 (s,constraints) (r,c,vs) = 
   [(extend s ((r,c),v),
     sortBy length3rd $ 
         prune2 (r,c,v) constraints) | v <- vs ]
 
showNode2 :: Node -> IO()
showNode2 = showSudoku2 . fst

solveNs2 :: [Node] -> [Node]
solveNs2 = search succNode2 solved 

succNode2 :: Node -> [Node]
succNode2 (s,[]) = []
succNode2 (s,p:ps) = extendNode2 (s,ps) p 

solveAndShow2 :: Grid -> IO()
solveAndShow2 gr = solveShowNs2 (initNode2 gr)

solveShowNs2 :: [Node] -> IO()
solveShowNs2 = sequence_ . fmap showNode2 . solveNs2

------- Exercise 5 -------
-- Almost everything is the same only using the new functions associated with NRC Sudoku s
-- Time spent: 10 minutes
minimalize2 :: Node -> [(Row,Column)] -> Node
minimalize2 n [] = n
minimalize2 n ((r,c):rcs) 
   | uniqueSol2 n' = minimalize2 n' rcs
   | otherwise     = minimalize2 n  rcs
  where n' = eraseN n (r,c)
  
uniqueSol2 :: Node -> Bool
uniqueSol2 node = singleton (solveNs2 [node]) where 
  singleton [] = False
  singleton [x] = True
  singleton (x:y:zs) = False
  
genProblemNRC :: Node -> IO Node
genProblemNRC n = do ys <- randomize xs
                     return (minimalize2 n ys)
   where xs = filledPositions (fst n)
   
-- f = return for only creating a random Sudoku and f = showNode2 to show it
-- Also just trying out another notation for IO actions
genRandomProblemNRC f = genRandomSudoku2 >>= \r -> genProblemNRC r >>= \s -> f s
                      
genRandomSudoku2 :: IO Node
genRandomSudoku2 = do [r] <- rsolveNs2 [emptyN]
                      return r
                      
rsuccNode2 :: Node -> IO [Node]
rsuccNode2 (s,cs) = 
  do xs <- getRandomCnstr cs
     if null xs 
        then return []
        else return (extendNode2 (s,cs\\xs) (head xs))

rsolveNs2 :: [Node] -> IO [Node]
rsolveNs2 ns = rsearch rsuccNode2 solved (return ns)

-- This takes a loooooooong time, takes even longer if we try to figure out for sure if it has one solution (uncomment lines below)
test :: IO()
test = do 
       x <- genRandomProblemNRC return
       --let s = uniqueSol2 x
       --if s then 
       showNode2 x --else error "No minimal NRC Sudoku"