module Lab5

where
import Data.List
import SS
import Test.Hspec
import Test.QuickCheck
import Control.Monad
import System.Random


------- Exercise 1 -------
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

createRandomSudoku :: IO Sudoku
createRandomSudoku = do
                     r <- genRandomSudoku
                     p <- genProblem r
                     return $ fst p
      
testProperties :: IO Bool      
testProperties = createRandomSudoku >>= return.testMinimal

------- Exercise 3 -------
deleteB :: [Row] -> [Column] -> Sudoku -> Sudoku
deleteB _ [] s = s
deleteB xs (y:ys) s = deleteB xs ys (deleteB' s xs y) 
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
                         then randomBlocks (n-1) (x:list) (deleteB (fst x) (snd x) s)
                         else randomBlocks n list s
   
-- It will work for 3 and give unique solution Sudoku s, for 4 or 5 it gives multiple solutions Sudoku s, and 6 is not possible
deleteRandomBlocks :: Int -> IO()  
deleteRandomBlocks n = do 
                      r <- genRandomSudoku
                      s <- randomBlocks n [] (fst r)
                      let q = solveNs[(s, constraints s)]
                      if length q >= 1 then showSudoku s else deleteRandomBlocks n
             
            
-- Function which expects a function that can be used on Sudoku s and there after shows the result with a random Sudoku (for debugging)
showRandomSudoku :: (Sudoku -> IO Sudoku) -> IO()            
showRandomSudoku f = do
                     r <- genRandomSudoku
                     s <- f (fst r)
                     showSudoku s
                 
fullGrid :: Grid                 
fullGrid = replicate 9 [1..9]                            
------- Exercise 4 -------

------- Exercise 5 -------

------- Exercise 6 -------