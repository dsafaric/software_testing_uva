-- Lab5.hs Exercise

module Lab5

where

import Week5
import Data.List
import Test.Hspec
import System.Random
import Test.QuickCheck
import System.IO.Unsafe
import SetOrd
-- Exercise 1
-- get the constraints function, and uniqueSolv 
-- you basically need to write a sudoko consistency check using HSpec
-- the sudoko should always have unique solution (only one solution) and should always be consistent

cons :: IO ()
cons = hspec $ do
	describe "consistent" $ do
		it "sudoko is consistent" $ do
			consistent (grid2sud example1) `shouldBe` (True :: Bool)
	describe "unique" $ do
		it "sudoko solution is unique" $ do
			uniqueSol (nonIO genRandomSudoku) `shouldBe` (True :: Bool)
		-- should we use generated and solved Sudoku's or the unsolved ones?
		-- in case of an solved one, we can use genRandomSudoku, while in the other case
		-- we use genSud which generates a solved sudoku and generates a problems of it

genSud :: IO Node
genSud = do
		[r] <- rsolveNs [emptyN]
		s <- genProblem r
		return s

nonIO :: IO a -> a
nonIO = \x -> unsafePerformIO x

-- the following function can be used for testing so that cons won't test on the basis
	-- of one sample only

genRandomSudokus :: Int -> IO [Node]
genRandomSudokus 0 = return []
genRandomSudokus n = do
		x <- genRandomSudoku
		xs <- genRandomSudokus (n-1)
		return (x:xs)

-- Exercise 2
-- every Sudoku problem P is minimal if it admits a unique solution

-- solution 1
minimal :: IO Node -> Bool
minimal = uniqueSol . nonIO 	-- the function uses nonIO (IO Node) because of we want to test
								-- the property on a random generated Sudoku problem
								-- we are using the genSud function, althouth the predifined
								-- genRandomSudoku could be used as well
-- the tests are returning a true value of every input 

-- solution 2

randInt :: Int -> Int -> IO Int
randInt = \x -> \y -> getStdRandom (randomR (x,y))

randList :: IO [Int]
randList = do 
	g <- getStdGen
	x <- randInt 10 20
	return $ take x (randomRs (1,9) g)

randCons :: [Int] -> [(Row, Column)]
randCons [] = []
randCons [x] = []
randCons (x:y:xs) = (x,y) : randCons xs

randCons' :: [(Row,Column)]
randCons' = nub $ randCons (nonIO randList)

-- node that was generated

minimalHspec :: Node -> [(Row, Column)] -> IO ()
minimalHspec n xs = hspec $ do 
	describe "uniqueSol" $ do
		it "Node has a unique solution"  $ do
			(uniqueSol $ minimalize n xs) `shouldBe` (True :: Bool) 


testProp :: IO ()
testProp = minimalHspec (nonIO genRandomSudoku) randCons'
-- the idea was to either use a list of nodes or a list of constraints to minimalize nodes to a level of 
	-- minimalness.. But that unfortunally doesn't work with Hspec since applying it to a list
	-- of objects is difficult because it accepts only the Testable objects -  while Sudoko isn't


