module Lab5

where
import Data.List
import SS
import Test.Hspec
import Test.QuickCheck
import Control.Monad
import System.Random
import SModified
import CrossSudoku

------- Exercise 1 -------
-- Time spent: Too much... At least 3 hours, especially to use arbitrary and testable instances of Sudoku.. Which did not work
runSpec :: IO()
runSpec = hspec specSudoku
                
specSudoku :: Spec
specSudoku = do
  describe "Normal Sudoku" $ do
   -- it "An empty grid should throw an exception" $ do
       -- shouldThrow anyException >>= (sud2grid (grid2sud [[]])) 
    it "A random Grid with duplicates should be be an invalid Sudoku" $ do
        g <- getRandomUnvalidGrid
        let s = consistent (grid2sud g)
        shouldBe s False
    it "If you create a random valid Sudoku it should be have all unique values" $ do
       r <- genRandomSudoku 
       let s = (consistent.fst) r 
       shouldBe s True 
    it "When a Sudoku is solved it should not have any zeros any more" $ do
        r <- genRandomSudoku
        let s = length (concatMap (filter (==0)) (sud2grid (fst r))) == 0
        shouldBe s True
    -- Take into account that this will take a while to compute
    it "A minimal Sudoku should have always one solution" $ do
        r <- genRandomSudoku
        s <- genProblem r
        let q = uniqueSol s
        shouldBe q True
        
-- Create a random grid which can not be a valid Sudoku
getRandomUnvalidGrid :: IO Grid
getRandomUnvalidGrid = getRandomGrid2 9
    where
        getRandomGrid2 0 = return []
        getRandomGrid2 n = do
                           x <- getRandomGrid' 9
                           xs <- getRandomGrid2 (n-1)
                           return (x:xs)

        getRandomGrid' 0 = return []
        getRandomGrid' n = do
                        x <- getRandomInt 8 -- Makes sure there are doubles
                        xs <- getRandomGrid' (n-1)
                        return (x:xs)
               
------- Exercise 2 -------
-- As the assignment suggests, we need to check two things:
-- If the problem is solvable and has one solution
-- Time spent: 30/ 45  minutes
testSolvable :: Sudoku -> Bool
testSolvable s = uniqueSol (s, constraints s)

-- If the problem with any of the erased positions has more than one unique solution the original Sudoku was not minimal
-- This computing time is quite long, just so you know it does work only takes some time
testUnique :: Sudoku -> Bool
testUnique s = length (
                filter (==True)
                (map testSolvable 
                [eraseS s (r,c) | r <- positions, c <- positions, s (r,c) /= 0]) -- all the Sudoku s with one positions erased
                ) == 0
                
testMinimal :: Sudoku -> Bool
testMinimal s = testSolvable s && testUnique s

-- Create a random unsolved Sudoku
createRandomSudoku :: IO Sudoku
createRandomSudoku = do
                     r <- genRandomSudoku
                     p <- genProblem r
                     return $ fst p
      
testProperties :: IO Bool      
testProperties = createRandomSudoku >>= return.testMinimal

------- Exercise 3 -------
-- Time spent: 1.5 / 2 hours
-- Delete all the values in the two given lists
deleteBlock :: [Row] -> [Column] -> Sudoku -> Sudoku
deleteBlock _ [] s = s
deleteBlock xs (y:ys) s = deleteBlock xs ys (deleteB' s xs y) 
    where
        deleteB' s [] _ = s
        deleteB' s (x:xs) y = deleteB' (eraseS s (x,y)) xs y
        
randomBlock :: IO ([Row], [Column])
randomBlock = do
                x <- getRandomInt 2
                y <- getRandomInt 2
                return ([x*3 + 1, x*3 +2 ,x*3 + 3], [y*3+1, y*3+2, y*3+3])
   
randomBlocks :: Int -> [([Row], [Column])] -> Sudoku -> IO Sudoku
randomBlocks 0 _ s = return s   
randomBlocks n list s  = do 
                         x <- randomBlock
                         if not (elem x list) 
                         then randomBlocks (n-1) (x:list) (deleteBlock (fst x) (snd x) s)
                         else randomBlocks n list s
   
-- It will work for 3 and 4 and give unique solution Sudokus, for 5 and up it will give non unique solutions
deleteRandomBlocks :: Int -> IO()  
deleteRandomBlocks n = do 
                      r <- genRandomSudoku
                      s <- randomBlocks n [] (fst r)
                      let q = solveNs[(s, constraints s)]
                      if length q == 1 then showSudoku s else deleteRandomBlocks n
             
            
-- Function which expects a function that can be used on Sudoku s and there after shows the result with a random Sudoku (for debugging)
showRandomSudoku :: (Sudoku -> IO Sudoku) -> IO()            
showRandomSudoku f = do
                     r <- genRandomSudoku
                     s <- f (fst r)
                     showSudoku s
       
example3EmptyUnique, example4EmptyUnique, example5EmptyUnique, example5Empty :: IO()
example3EmptyUnique = deleteRandomBlocks 3 
example4EmptyUnique = deleteRandomBlocks 4

-- Does not seem to terminate, so deleting 5 blocks will probably result in a Sudoku without an unique solution
example5EmptyUnique = deleteRandomBlocks 5 

-- Will work, but gives a solution with multiple solutions, so you have to guess and multiple answers are correct, 
-- logically this can be done for all number up till 9 (empty Sudoku)
example5Empty = showRandomSudoku $ randomBlocks 5 [] 
       
-- For debugging purposes
fullGrid :: Grid                 
fullGrid = replicate 9 [1..9]                            
------- Exercise 4 & Exercise 5 -------

---- See SModified.hs ----

------- Exercise 6 -------
-- Time spent: 3,5 hours
{-
    According to the internet, which is of course always right, difficulty depends mostly on which techniques are used to
    solve a Sudoku. Since this is a bit out of our scope we checked if there was another way to determine difficulty.
    Something we found was that difficulty depends not only on the amount of number given but also on how the given values 
    are distributed. When whole grids are left empty it is more difficult to solve the Sudoku. Next to that, if certain 
    number appear less (and evidentially others more) it is more difficult than if they are evenly balanced. Below are an 
    easy (random) Sudoku with 30 given values and a harder one.
-}

-- The list of values that may be deleted, this way a number is never less than three times represented in the Sudoku
permitDelete :: [Int]
permitDelete = concatMap (replicate 6) [1..9]       

-- Checks the amount of empty positions in a subGrid
checkSubGrid :: Sudoku -> (Row, Column) -> Int
checkSubGrid s (r,c) = length $ filter (/=0) (subGrid s (r,c))          
    
genEasy :: IO Node          
genEasy = do n <- genRandomSudoku
             let xs = filledPositions (fst n)
             ys <- randomize xs
             return (selectiveMinimalizeEasy n ys 51 permitDelete)        
        
selectiveMinimalizeEasy :: Node -> [(Row,Column)] -> Int -> [Int] -> Node
selectiveMinimalizeEasy n [] _ _= n
selectiveMinimalizeEasy n _ 0 _= n
selectiveMinimalizeEasy n ((r,c):rcs) k list
   | not $ elem (fst n (r,c)) list = selectiveMinimalizeEasy n  rcs k list-- If the value at (r,c) is already 6 times deleted then go on with another field
   | checkSubGrid (fst n) (r,c) <= 1 = selectiveMinimalizeEasy n  rcs k list-- Each subGrid should at least have one value
   | uniqueSol n'  = selectiveMinimalizeEasy n' rcs (k-1) $ delete (fst n (r,c)) list
   | otherwise     = selectiveMinimalizeEasy n  rcs k list
  where n' = eraseN n (r,c)
  
exampleEasy = genEasy >>= \x -> showNode x
  
-- And now the hard one
-- First create list of permitDelete
mustDelete = do 
               x <- getRandomInt 8
               y <- getRandomInt 8
               return $ replicate 7 (x + 1) ++ replicate 7 (y + 2)
               
genHard = do r <- genRandomSudoku
             n <- randomBlocks 2 [] (fst r) -- Delete two random blocks
             let b = uniqueSol (n, constraints n) -- While still have a unique solution
             let xs = filledPositions n
             ys <- randomize xs
             md <- mustDelete
             if b then return (deleteList (n, constraints n) ys 33 md []) else genHard
    
deleteList n [] k _ cns = selectiveMinimalizeHard n cns k 
deleteList n rcs k [] cns = selectiveMinimalizeHard n (cns ++ rcs) k    
deleteList n ((r,c):rcs) k list cns
   | elem s list && uniqueSol n' = deleteList n' rcs (k-1) (delete s list) cns
   | otherwise = deleteList n rcs k list (cns ++ [(r,c)])
  where n' = eraseN n (r,c)
        s  = fst n (r,c)
             
selectiveMinimalizeHard :: Node -> [(Row,Column)] -> Int -> Node
selectiveMinimalizeHard n [] _ = n
selectiveMinimalizeHard n _ 0 = n  
selectiveMinimalizeHard n ((r,c):rcs) k
   | uniqueSol n'  = selectiveMinimalizeHard n' rcs (k-1)
   | otherwise     = selectiveMinimalizeHard n  rcs k
  where n' = eraseN n (r,c) 
  
length3 (a,b,cs) = length cs

-- Count the constraints of easy or hard Sudokus
countConstraints :: Bool -> IO Int
countConstraints b =  
 do
    s <- sub' b
    let r = snd s 
    return $ sum $ map length3 r
 where
    sub' True = genEasy
    sub' False = genHard
        
-- Count the amounts of constraints of multiple easy or hard Sudokus 
genAmount :: Int -> Bool -> IO [Int]
genAmount 0 _ = return []           
genAmount n b = 
 do
    x <- sub' b
    xs <- genAmount (n-1) b
    return (x:xs)
 where
    sub' True = countConstraints True
    sub' False = countConstraints False
    
-- Show the average, pretty slow though
genAverage :: IO ()
genAverage = do
             x <- genAmount 20 False
             y <- genAmount 20 True
             let vx = (fromIntegral $ sum x) / 20
             let vy = (fromIntegral $ sum y) / 20
             print $ "Hard: " ++ show vx ++ ", Easy: "  ++ 
                     show vy ++ ", Difference: " ++ show (vx - vy)
  
